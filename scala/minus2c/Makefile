OBJS = lex.yy.bc C.tab.bc SymbTable.bc ast.bc mycc.bc
SRCS = lex.yy.c C.tab.c SymbTable.c ast.c mycc.c
GUFF = C.tab.c C.tab.h lex.yy.c C.output
CC = ${GRAAL_CC}
BISON = /usr/local/opt/bison@3.1/bin/bison
LINK = ${LLVM_LINK}
GRAAL_CC = /usr/local/opt/llvm@6/bin/clang -I/usr/local/include -I/Library/Java/JavaVirtualMachines/graalvm-ce-1.0.0-rc7/Contents/Home/jre/languages/llvm -resource-dir /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/10.0.0 -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.14.sdk -O0 -emit-llvm
LLVM_LINK = /usr/local/opt/llvm@6/bin/llvm-link
TARGET = src/main/resources/mycclib
CP = /Users/jamie/.ivy2/cache/ch.epfl.lamp/dotty-library_0.10/jars/dotty-library_0.10-0.10.0-RC1.jar:/Users/jamie/.ivy2/cache/ch.epfl.lamp/scala-library/jars/scala-library-0.10.0-RC1.jar:/Users/jamie/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.7.jar:target/scala-0.10/classes/
JAR = target/scala-0.10/dotty-simple_0.10-0.1.0.jar
PARSER = cat testfile | scala -cp ${CP} ${JAR}

all:  ${TARGET}

pkg: ${TARGET}
	sbt package

parse:
	${PARSER} 

run:
	${PARSER} -i

timed:
	${PARSER} -t -i

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
