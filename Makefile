OBJS = lex.yy.bc C.tab.bc SymbTable.bc ast.bc mmc.bc
SRCS = lex.yy.c C.tab.c SymbTable.c ast.c mmc.c
GUFF = C.tab.c C.tab.h lex.yy.c C.output
CC = ${GRAAL_CC}
CLANG = ${GRAALVM_HOME}/jre/languages/llvm/native/bin/clang
BISON = /usr/local/opt/bison/bin/bison
LINK = ${LLVM_LINK}
LLVM_HOME = /usr/local/opt/llvm/bin
OPT = ${LLVM_HOME}/opt
GRAALVM_HOME = /Library/Java/JavaVirtualMachines/graalvm-ce-19.2.0/Contents/Home
POLYGLOT_H=${GRAALVM_HOME}/jre/languages/llvm/include
GRAAL_CC = ${CLANG} -I${POLYGLOT_H} -emit-llvm -O1
GRAAL_CC_SANDBOXED = ${GRAALVM_HOME}/bin/clang-sandboxed
LLVM_LINK = ${LLVM_HOME}/llvm-link

TARGET = src/main/resources/mmclib

SCALA_LIB_VERSION = 2.13.0
DOTTY_VERSION     = 0.20
DOTTY_MINOR       = 0
DOTTY_SUFFIX      = RC1
JAR = target/scala-${DOTTY_VERSION}/mmc_${DOTTY_VERSION}-0.1.0.jar
TAIL = ${DOTTY_VERSION}.${DOTTY_MINOR}-${DOTTY_SUFFIX}
DOTTY_LIB = dotty-library_${DOTTY_VERSION}
IVY = /Users/jamie/.ivy2/cache

CP = ${IVY}/ch.epfl.lamp/${DOTTY_LIB}/jars/${DOTTY_LIB}-${TAIL}.jar:$\
${IVY}/ch.epfl.lamp/scala-library/jars/scala-library-${TAIL}.jar:$\
${IVY}/org.scala-lang/scala-library/jars/scala-library-${SCALA_LIB_VERSION}.jar

MYCC = dotr -cp ${CP}:${JAR} mmc.Parser

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
	${PARSER} -i -time -sep -p

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
	${CC} -g -c $*.c -o $*.bc

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

dist:	SymbTable.c ast.c mmc.c Makefile C.flex C.y  ast.h
	tar cvfz mmc.tgz SymbTable.c ast.c mmc.c Makefile C.flex C.y \
		ast.h
