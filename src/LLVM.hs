module LLVM (compile) where

import Control.Monad (forM_)
import Control.Monad.RWS.Lazy (RWS, get, put, runRWS, tell)
import Data.Map.Lazy (Map, empty, insert, (!))
import Instant.Abs (Exp (ExpAdd, ExpDiv, ExpLit, ExpMul, ExpSub, ExpVar), Ident, Program (Prog), Stmt (SAss, SExp))
import Instant.ErrM (Err)
import System.FilePath (replaceExtension, takeDirectory)
import System.Process (system)

type Loc = Integer

type Code = [String]

type LLVMMonad = RWS () Code (Loc, Map Ident ExprVal)

data ExprVal = Reg Integer | Lit Integer

data Instr
  = Add Loc ExprVal ExprVal
  | Mul Loc ExprVal ExprVal
  | Sub Loc ExprVal ExprVal
  | Div Loc ExprVal ExprVal

instance Show ExprVal where
  show (Reg int) = "%r" ++ show int
  show (Lit int) = show int

instance Show Instr where
  show (Add x v1 v2) = "    " ++ show (Reg x) ++ " = add i32 " ++ show v1 ++ ", " ++ show v2
  show (Mul x v1 v2) = "    " ++ show (Reg x) ++ " = mul i32 " ++ show v1 ++ ", " ++ show v2
  show (Sub x v1 v2) = "    " ++ show (Reg x) ++ " = sub i32 " ++ show v1 ++ ", " ++ show v2
  show (Div x v1 v2) = "    " ++ show (Reg x) ++ " = sdiv i32 " ++ show v1 ++ ", " ++ show v2

compile :: Program -> String -> IO ()
compile (Prog stmts) =
  let (_, _, code) = runRWS (getCode stmts) () (0, empty)
   in saveToFile code

getCode :: [Stmt] -> LLVMMonad ()
getCode stmts = forM_ stmts showStmt
  where
    showStmt :: Stmt -> LLVMMonad ()
    showStmt (SAss ident exp) = do
      v <- showExpr exp
      (reg, env) <- get
      put (reg, insert ident v env)
    showStmt (SExp exp) = do
      v <- showExpr exp
      tell ["    call void @printInt(i32 " ++ show v ++ ")"]

showExpr :: Exp -> LLVMMonad ExprVal
showExpr (ExpAdd exp1 exp2) = showExprAux exp1 exp2 Add
showExpr (ExpMul exp1 exp2) = showExprAux exp1 exp2 Mul
showExpr (ExpSub exp1 exp2) = showExprAux exp1 exp2 Sub
showExpr (ExpDiv exp1 exp2) = showExprAux exp1 exp2 Div
showExpr (ExpLit lit) = return $ Lit lit
showExpr (ExpVar ident) = get >>= (\(_, env) -> return $ env ! ident)

showExprAux :: Exp -> Exp -> (Loc -> ExprVal -> ExprVal -> Instr) -> LLVMMonad ExprVal
showExprAux exp1 exp2 expF = do
  v1 <- showExpr exp1
  v2 <- showExpr exp2
  (reg, env) <- get
  put (reg + 1, env)
  tell [show $ expF (reg + 1) v1 v2]
  return $ Reg $ reg + 1

saveToFile :: Code -> String -> IO ()
saveToFile code name =
  let llvm = wrapCode $ unlines code
      out = replaceExtension name ".ll"
   in do
        writeFile out llvm
        system $ "llvm-link -v -o " ++ replaceExtension name ".bc" ++ " " ++ out
        return ()

wrapCode :: String -> String
wrapCode main =
  "@dnl = internal constant [4 x i8] c\"%d\\0a\\00\"\n\n"
    ++ "declare i32 @printf(i8*, ...) nounwind\n\n"
    ++ "define void @printInt(i32 %i) {\n"
    ++ "entry:  %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n"
    ++ "    call i32 (i8*, ...) @printf(i8* %t0, i32 %i)\n"
    ++ "    ret void\n"
    ++ "}\n\n"
    ++ "define i32 @main() #0 {\n"
    ++ main
    ++ "    ret i32 0\n"
    ++ "}\n"
