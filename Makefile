GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc
ifeq (, $(shell which bnfc))
	BNFC   = /home/students/inf/PUBLIC/MRJP/bin/bnfc
else
	BNFC   = bnfc
endif

.PHONY : all clean

all : jvm llvm

Instant/Abs.hs Instant/Lex.x Instant/Par.y Instant/Print.hs Instant/Test.hs : Instant.cf
	${BNFC} --haskell -d Instant.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

jvm : Instant/Abs.hs Instant/Lex.hs Instant/Par.hs Instant/Print.hs Instant/Test.hs src/JVM.hs src/Compiler.hs
	${GHC} src/Compiler.hs src/JVM.hs -DC_JVM -o insc_jvm

llvm : Instant/Abs.hs Instant/Lex.hs Instant/Par.hs Instant/Print.hs Instant/Test.hs src/LLVM.hs src/Compiler.hs
	${GHC} src/Compiler.hs src/LLVM.hs -DC_LLVM -o insc_llvm

clean :
	-rm -rf Instant/ src/*.hi src/*.o insc_jvm insc_llvm

