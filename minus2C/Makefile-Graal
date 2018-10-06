OBJS = lex.yy.bc C.tab.bc symbol_table.bc nodes.bc main.bc
SRCS = lex.yy.c C.tab.c symbol_table.c nodes.c main.c
GUFF = C.tab.c C.tab.h lex.yy.c C.output
CC = ${GRAAL_CC}
LINK = ${GRAAL_LINK}
GRAAL_CC = clang -O1 -emit-llvm
GRAAL_LINK = /usr/local/opt/llvm/bin/llvm-link
TARGET = mycc

all: mycc

clean:
	rm ${OBJS}
	rm ${GUFF}

clean_target:
	rm ${TARGET}

%.bc:
	${CC} -g -c $*.c

mycc: C.tab.c lex.yy.c ${OBJS}
	${LINK} -o mycc ${OBJS} 

lex.yy.c: C.flex
	flex C.flex

C.tab.c: C.y
	bison -d -t -v C.y

# .c.bc:
# 	${CC} -g -c $*.c

depend:	
	${CC} -M $(SRCS) > .deps
	cat Makefile .deps > makefile

dist:	symbol_table.c nodes.c main.c Makefile C.flex C.y nodes.h token.h
	tar cvfz mycc.tgz symbol_table.c nodes.c main.c Makefile C.flex C.y \
		nodes.h token.h
