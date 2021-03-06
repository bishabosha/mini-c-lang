D   [0-9]
L   [a-zA-Z_]
H   [a-fA-F0-9]
E   [Ee][+-]?{D}+
FS  (f|F|l|L)
IS  (u|U|l|L)*

%{
#include <stdio.h>
#include "C.tab.h"
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <polyglot.h>
#include "ast.h"

Ast* string_literal_new(char*);
extern Ast* get_identifier(const char*);
Ast* constant_new(char*);
Ast* lasttok;

void count(void);
void comment(void);
%}

%%

"/*"        { comment(); }

"auto"      { count(); return(AUTO); }
"break"     { count(); return(BREAK); }
"continue"  { count(); return(CONTINUE); }
"else"      { count(); return(ELSE); }
"extern"    { count(); return(EXTERN); }
"if"        { count(); return(IF); }
"int"       { count(); return(INT); }
"function"  { count(); return(FUNCTION); }
"return"    { count(); return(RETURN); }
"void"      { count(); return(VOID); }
"while"     { count(); return(WHILE); }

{L}({L}|{D})*       { count(); lasttok = get_identifier(yytext);  return(IDENTIFIER); }
{D}+{IS}?           { count(); lasttok = constant_new(yytext);   return(CONSTANT); }
L?'(\\.|[^\\'])+'   { count(); lasttok = constant_new(yytext);   return(CONSTANT); }
L?\"(\\.|[^\\"])*\" { count(); lasttok = string_literal_new(yytext);  return(STRING_LITERAL); }

"<="    { count(); return(LE_OP); }
">="    { count(); return(GE_OP); }
"=="    { count(); return(EQ_OP); }
"!="    { count(); return(NE_OP); }
";"     { count(); return(';'); }
"{"     { count(); return('{'); }
"}"     { count(); return('}'); }
","     { count(); return(','); }
":"     { count(); return(':'); }
"="     { count(); return('='); }
"("     { count(); return('('); }
")"     { count(); return(')'); }
"!"     { count(); return('!'); }
"-"     { count(); return('-'); }
"+"     { count(); return('+'); }
"*"     { count(); return('*'); }
"/"     { count(); return('/'); }
"%"     { count(); return('%'); }
"<"     { count(); return('<'); }
">"     { count(); return('>'); }

[ \t\v\n\f]     { count(); }
.               { return(EMPTY); }

%%

int yywrap(void) {
    return(1);
}


void comment(void) {
    char c, c1;

loop:
    while ((c = input()) != '*' && c != 0) putchar(c);

    if ((c1 = input()) != '/' && c != 0) {
        unput(c1);
        goto loop;
    }

    if (c != 0) putchar(c1);
}


int column = 0;

void count() {
    int i;
    for (i = 0; yytext[i] != '\0'; i++)
        if (yytext[i] == '\n')
            column = 0;
        else if (yytext[i] == '\t')
            column += 8 - (column % 8);
        else
            column++;
}

Ast *Singleton_new(int type) {
    Ast *ans = (Ast *)malloc(sizeof(Ast));
    ans->tag = SINGLETON;
    ans->tpe = type;
    return ans;
}

Ast *TokenString_new(int type, char * lexeme) {
    TokenString *ans = (TokenString *)malloc(sizeof(TokenString));
    ans->ast.tag = TOKEN_STRING;
    ans->ast.tpe = type;
    ans->lexeme = lexeme;
    return (Ast *)ans;
}

Ast *TokenInt_new(int type, int value) {
    TokenInt *ans = (TokenInt*)malloc(sizeof(TokenInt));
    ans->ast.tag = TOKEN_INT;
    ans->ast.tpe = type;
    ans->value = value;
    return (Ast *)ans;
}

Ast *string_literal_new(char *s) {
    int len = strlen(s);
    char * lexeme = (char *)malloc(len);
    strcpy(lexeme, s);
    memmove(lexeme, lexeme+1, len-2);
    lexeme[len-2]='\0';
    return (Ast *)TokenString_new(STRING_LITERAL, lexeme);
}

Ast *constant_new(char *s) {
    int n = *s!='\'' ? atoi(s) : *(s+1);
    return (Ast *)TokenInt_new(CONSTANT, n);
}
