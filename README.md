# Instant JVM & LLVM Compiler

## Language syntax
```
Prog. Program ::= [Stmt] ;
SAss. Stmt ::= Ident "=" Exp;
SExp. Stmt ::= Exp ;
separator Stmt ";" ;

ExpAdd.            Exp1   ::= Exp2 "+"  Exp1 ;
ExpSub.            Exp2   ::= Exp2 "-"  Exp3 ;
ExpMul.            Exp3   ::= Exp3 "*"  Exp4 ;
ExpDiv.            Exp3   ::= Exp3 "/"  Exp4 ;
ExpLit.            Exp4   ::= Integer ;
ExpVar.            Exp4   ::= Ident ;
coercions Exp 4;
```

## Prerequisites

[GHC](https://www.haskell.org/ghc/) - Haskell Compiler  
[Happy](https://www.haskell.org/happy/) - The Parser Generator for Haskell  
[Alex](https://www.haskell.org/alex/) - A lexical analyser generator for Haskell  
[BNFC](https://bnfc.digitalgrammars.com/) - Compiler front-end generator  

## Usage

In the root call `make`. This should create two executable files `insc_jvm` and `insc_llvm`.  
Both of them accept one argument which is a path to a file to be compiled and output the
final (`.class`, `.bc`) and itermediate (`.j`, `.ll`) files in the same location.  
`insc_jvm` requires the `lib/` to be adjacent to it bacause of the usage of `jasmin.jar`.

## Project Tree

```
lib/
 ├─ jasmin.jar   # JVM Assembler
src/
 ├─ Compiler.hs  # Main module reponsible for reading and passing arguments
 ├─ JVM.hs       # Implementation of JVM compiler
 ├─ LLVM.hs      # Implementation of LLVM compiler
Instant.cf       # Language Syntax Grammar
Makefile         # Project Makefile 
README.md        # This README
```
