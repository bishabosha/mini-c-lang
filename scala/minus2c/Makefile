OBJS = lex.yy.bc C.tab.bc SymbTable.bc ast.bc mycc.bc
SRCS = lex.yy.c C.tab.c SymbTable.c ast.c mycc.c
GUFF = C.tab.c C.tab.h lex.yy.c C.output
CC = ${GRAAL_CC}
BISON = /usr/local/opt/bison/bin/bison
LINK = ${LLVM_LINK}
LLVM_HOME = /usr/local/opt/llvm/bin
OPT = ${LLVM_HOME}/opt
GRAALVM_HOME = /Library/Java/JavaVirtualMachines/graalvm-ee-1.0.0-rc10/Contents/Home
GRAAL_CC = clang -I${GRAALVM_HOME}/jre/languages/llvm -emit-llvm -O1
GRAAL_CC_SANDBOXED = ${GRAALVM_HOME}/bin/clang-sandboxed
LLVM_LINK = ${LLVM_HOME}/llvm-link

TARGET = src/main/resources/mycclib

DOTTY_VERSION = 0.11
JAR = target/scala-${DOTTY_VERSION}/minus2c_${DOTTY_VERSION}-0.1.0.jar
TAIL = ${DOTTY_VERSION}.0-RC1
DOTTY_LIB = dotty-library_${DOTTY_VERSION}
IVY = /Users/jamie/.ivy2/cache

CP = ${IVY}/ch.epfl.lamp/${DOTTY_LIB}/jars/${DOTTY_LIB}-${TAIL}.jar:$\
${IVY}/ch.epfl.lamp/scala-library/jars/scala-library-${TAIL}.jar:$\
${IVY}/org.scala-lang/scala-library/jars/scala-library-2.12.7.jar

MYCC = dotr -cp ${CP}:${JAR} mycc.Parser

PARSER = cat testfile | ${MYCC}

all:  ${TARGET}

cp:
	echo "${CP}" | pbcopy

mips:
	${PARSER} -m > out.asm

run:
	${PARSER} -i

pkg: ${TARGET}
	sbt package

normal:
	${PARSER} -n

parse:
	${PARSER} -n -m -t -sep

run_timed:
	${PARSER} -i -time -sep

tac:
	${PARSER} -t

timed:
	${PARSER} -time -n -m -t -sep

boot: clean rm

reboot: boot all

clean:
	rm ${OBJS}
	rm ${GUFF}

rm:
	rm ${TARGET}

rm_logs:
	rm *.log

%.bc:
	${CC} -g -c $*.c

${TARGET}: C.tab.c lex.yy.c ${OBJS}
	${LINK} -o ${TARGET} ${OBJS}
	${OPT} -mem2reg -adce -argpromotion -constmerge -globaldce -globalopt -O3 ${TARGET} -o=${TARGET}

lex.yy.c: C.flex
	flex C.flex

C.tab.c: C.y
	${BISON} -d -t -v C.y

depend:	
	${CC} -M $(SRCS) > .deps
	cat Makefile .deps > makefile

dist:	SymbTable.c ast.c mycc.c Makefile C.flex C.y  ast.h
	tar cvfz mycc.tgz SymbTable.c ast.c mycc.c Makefile C.flex C.y \
		ast.h
