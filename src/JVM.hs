module JVM (compile) where

import Control.Monad (foldM, forM_)
import Control.Monad.RWS.Lazy (MonadState (get), RWS, put, runRWS, tell)
import Data.Foldable (foldrM)
import Data.Map (Map, empty, insert, member, (!))
import Data.Text (replace)
import Distribution.SPDX (LicenseId (DOC))
import Instant.Abs (Exp, Ident, Program (Prog), Stmt)
import qualified Instant.Abs as Abs
import Instant.ErrM (Err)
import System.FilePath (replaceExtension, takeBaseName, takeDirectory)
import System.Process (system)

type Loc = Int

type Code = [String]

type JVMMonad = RWS () Code (Map Ident Loc)

data Instr = Add | Mul | Sub | Div | Swp | Load Loc | Store Loc | Push Integer

data SwpStmt = SAss Ident SwpExp | SExp SwpExp

data SwpExp
  = ExpAdd SwpExp SwpExp
  | ExpSub SwpExp SwpExp
  | ExpMul SwpExp SwpExp
  | ExpDiv SwpExp SwpExp
  | ExpLit Integer
  | ExpVar Ident
  | Swap SwpExp

type SwpExpCons = SwpExp -> SwpExp -> SwpExp

constMax, bipushMax, sipushMax :: Integer
constMax = 5
bipushMax = 127
sipushMax = 32767

instance Show Instr where
  show Add = "  iadd"
  show Mul = "  imul"
  show Sub = "  isub"
  show Div = "  idiv"
  show Swp = "  swap"
  show (Load loc) = (if loc <= 3 then "  iload_" else "  iload ") ++ show loc
  show (Store loc) = (if loc <= 3 then "  istore_" else "  istore ") ++ show loc
  show (Push int) =
    ( if int <= constMax
        then "  iconst_"
        else
          if int <= bipushMax
            then "  bipush "
            else
              if int <= sipushMax
                then "  sipush "
                else "  ldc "
    )
      ++ show int

compile :: Program -> String -> IO ()
compile (Prog stmts) =
  let (_, _, code) = runRWS (compiler stmts) () empty
   in saveToFile code

compiler :: [Stmt] -> JVMMonad ()
compiler stmts = do
  count <- addLocals stmts
  limitLocals count
  let swpStmts = map swapifyStmt stmts
  (depth, betterStmts) <- optimise swpStmts
  limitStack depth
  getCode betterStmts

addLocals :: [Stmt] -> JVMMonad Int
addLocals = foldM addLocal 1
  where
    addLocal :: Loc -> Stmt -> JVMMonad Int
    addLocal loc (Abs.SExp _) = return loc
    addLocal loc (Abs.SAss ident _) = do
      env <- get
      if member ident env
        then do
          return loc
        else do
          put $ insert ident loc env
          return $ loc + 1

limitLocals :: Int -> JVMMonad ()
limitLocals count = tell [".limit locals " ++ show count]

limitStack :: Int -> JVMMonad ()
limitStack depth = tell [".limit stack " ++ show depth]

swapifyStmt :: Stmt -> SwpStmt
swapifyStmt (Abs.SAss id exp) = SAss id $ swapifyExp exp
swapifyStmt (Abs.SExp exp) = SExp $ swapifyExp exp

swapifyExp :: Exp -> SwpExp
swapifyExp (Abs.ExpAdd exp exp') = ExpAdd (swapifyExp exp) (swapifyExp exp')
swapifyExp (Abs.ExpSub exp exp') = ExpSub (swapifyExp exp) (swapifyExp exp')
swapifyExp (Abs.ExpMul exp exp') = ExpMul (swapifyExp exp) (swapifyExp exp')
swapifyExp (Abs.ExpDiv exp exp') = ExpDiv (swapifyExp exp) (swapifyExp exp')
swapifyExp (Abs.ExpLit n) = ExpLit n
swapifyExp (Abs.ExpVar id) = ExpVar id

optimise :: [SwpStmt] -> JVMMonad (Int, [SwpStmt])
optimise = foldrM stmtOptimiser (1, [])

stmtOptimiser :: SwpStmt -> (Int, [SwpStmt]) -> JVMMonad (Int, [SwpStmt])
stmtOptimiser (SAss ident exp) (maxDepth, stmts) = do
  let (depth, betterExp) = optimiseExpr exp
  return (max maxDepth depth, SAss ident betterExp : stmts)
stmtOptimiser (SExp exp) (maxDepth, stmts) = do
  let (depth, betterExp) = optimiseExpr exp
  return (max maxDepth (depth + 1), SExp betterExp : stmts)

optimiseExpr :: SwpExp -> (Int, SwpExp)
optimiseExpr (ExpAdd exp1 exp2) = optimiseCommutative exp1 exp2 ExpAdd
optimiseExpr (ExpMul exp1 exp2) = optimiseCommutative exp1 exp2 ExpMul
optimiseExpr (ExpSub exp1 exp2) = optimiseNoncommutative exp1 exp2 ExpSub
optimiseExpr (ExpDiv exp1 exp2) = optimiseNoncommutative exp1 exp2 ExpDiv
optimiseExpr (Swap _) = undefined
optimiseExpr expr = (1, expr)

optimiseCommutative :: SwpExp -> SwpExp -> SwpExpCons -> (Int, SwpExp)
optimiseCommutative exp1 exp2 expF =
  let (depth1, betterExp1) = optimiseExpr exp1
      (depth2, betterExp2) = optimiseExpr exp2
   in if depth1 == depth2
        then (depth1 + 1, expF betterExp1 betterExp2)
        else
          if depth1 > depth2
            then (depth1, expF betterExp1 betterExp2)
            else (depth2, expF betterExp2 betterExp1)

optimiseNoncommutative :: SwpExp -> SwpExp -> SwpExpCons -> (Int, SwpExp)
optimiseNoncommutative exp1 exp2 expF =
  let (depth1, betterExp1) = optimiseExpr exp1
      (depth2, betterExp2) = optimiseExpr exp2
   in if depth1 == depth2
        then (depth1 + 1, expF betterExp1 betterExp2)
        else
          if depth1 > depth2
            then (depth1, expF betterExp1 betterExp2)
            else (depth2, Swap $ expF betterExp2 betterExp1)


getCode :: [SwpStmt] -> JVMMonad ()
getCode stmts = forM_ stmts showStmt
  where
    showStmt :: SwpStmt -> JVMMonad ()
    showStmt (SAss ident exp) = do
      showExpr exp
      env <- get
      tell [show $ Store $ env ! ident]
    showStmt (SExp exp) = do
      tell ["  getstatic java/lang/System/out Ljava/io/PrintStream;"]
      showExpr exp
      tell ["  invokevirtual java/io/PrintStream/println(I)V"]

showExpr :: SwpExp -> JVMMonad ()
showExpr (ExpAdd exp1 exp2) = showExpr exp1 >> showExpr exp2 >> tell [show Add]
showExpr (ExpMul exp1 exp2) = showExpr exp1 >> showExpr exp2 >> tell [show Mul]
showExpr (ExpSub exp1 exp2) = showExpr exp1 >> showExpr exp2 >> tell [show Sub]
showExpr (ExpDiv exp1 exp2) = showExpr exp1 >> showExpr exp2 >> tell [show Div]
showExpr (ExpLit lit) = tell [show $ Push lit]
showExpr (ExpVar var) = get >>= (\env -> tell [show $ Load $ env ! var])
showExpr (Swap (ExpDiv exp1 exp2)) =
  showExpr exp1 >> showExpr exp2 >> tell [show Swp] >> tell [show Div]
showExpr (Swap (ExpSub exp1 exp2)) =
  showExpr exp1 >> showExpr exp2 >> tell [show Swp] >> tell [show Sub]
showExpr (Swap _) = undefined

saveToFile :: [String] -> String -> IO ()
saveToFile code name =
  let jvm = wrapCode name $ unlines code
      out = replaceExtension name ".j"
   in do
        writeFile out jvm
        system $ "java -jar lib/jasmin.jar " ++ out ++ " -d " ++ takeDirectory name
        return ()

wrapCode :: String -> String -> String
wrapCode name code =
  ".class public "
    ++ takeBaseName name
    ++ "\n"
    ++ ".super java/lang/Object\n\n"
    ++ ".method public <init>()V\n"
    ++ "  aload_0\n"
    ++ "  invokespecial java/lang/Object/<init>()V\n"
    ++ "  return\n"
    ++ ".end method\n\n"
    ++ ".method public static main([Ljava/lang/String;)V\n"
    ++ code
    ++ "  return\n"
    ++ ".end method\n"
