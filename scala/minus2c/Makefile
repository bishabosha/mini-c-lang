OBJS = lex.yy.bc C.tab.bc symbol_table.bc ast.bc mycc.bc
SRCS = lex.yy.c C.tab.c symbol_table.c ast.c mycc.c
GUFF = C.tab.c C.tab.h lex.yy.c C.output
CC = ${GRAAL_CC}
LINK = ${LLVM_LINK}
GRAAL_CC = /usr/local/opt/llvm@6/bin/clang -I/usr/local/include -I/Library/Java/JavaVirtualMachines/graalvm-ce-1.0.0-rc7/Contents/Home/jre/languages/llvm -resource-dir /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/10.0.0 -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.14.sdk -O0 -emit-llvm
LLVM_LINK = /usr/local/opt/llvm@6/bin/llvm-link
TARGET = mycclib

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

mycclib: C.tab.c lex.yy.c ${OBJS}
	${LINK} -o mycclib ${OBJS} 

lex.yy.c: C.flex
	flex C.flex

C.tab.c: C.y
	bison -d -t -v C.y

depend:	
	${CC} -M $(SRCS) > .deps
	cat Makefile .deps > makefile

dist:	symbol_table.c ast.c mycc.c Makefile C.flex C.y  ast.h
	tar cvfz mycc.tgz symbol_table.c ast.c mycc.c Makefile C.flex C.y \
		ast.h
