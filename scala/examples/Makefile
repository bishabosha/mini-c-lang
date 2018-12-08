OBJS = foreign.bc
SRCS = foreign.c
CC = ${GRAAL_CC}
LINK = ${LLVM_LINK}
LLVM_HOME = /usr/local/opt/llvm/bin
OPT = ${LLVM_HOME}/opt
GRAALVM_HOME = /Library/Java/JavaVirtualMachines/graalvm-ce-1.0.0-rc10/Contents/Home
GRAAL_CC = clang -I${GRAALVM_HOME}/jre/languages/llvm -emit-llvm -O1
GRAAL_CC_SANDBOXED = ${GRAALVM_HOME}/bin/clang-sandboxed
DOTTY_VERSION = 0.11
LLVM_LINK = ${LLVM_HOME}/llvm-link
TARGET = src/main/resources/foreign
JAR = target/scala-${DOTTY_VERSION}/examples_${DOTTY_VERSION}-0.1.0.jar

all:  ${TARGET}

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

${TARGET}: ${OBJS}
	${LINK} -o ${TARGET} ${OBJS}
	${OPT} -mem2reg -adce -argpromotion -constmerge -globaldce -globalopt -O3 ${TARGET} -o=${TARGET}