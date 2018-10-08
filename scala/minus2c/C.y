%{
#include "ast.h"
#define YYSTYPE Ast*
#define YYDEBUG 1
extern Ast *int_token, *void_token, *function_token, *lasttok;
Ast *ans;
%}

%token IDENTIFIER CONSTANT STRING_LITERAL
%token LE_OP GE_OP EQ_OP NE_OP

%token EXTERN AUTO
%token INT VOID FUNCTION
%token APPLY LEAF

%token  IF ELSE WHILE CONTINUE BREAK RETURN

%start goal
%%

goal    :  translation_unit { ans = $$ = $1;}
        ;

primary_expression
	: IDENTIFIER 			{ $$ = lasttok; }
	| CONSTANT 				{ $$ = lasttok; }
	| STRING_LITERAL 		{ $$ = lasttok; }
	| '(' expression ')' 	{ $$ = $2; }
	;

postfix_expression
	: primary_expression									{ $$ = $1; }
	| postfix_expression '(' ')'    						{ $$ = Node_new(APPLY, $1, NULL); }
	| postfix_expression '(' argument_expression_list ')' 	{ $$ = Node_new(APPLY, $1, $3); }
	;

argument_expression_list
	: assignment_expression									{ $$ = $1; }
	| argument_expression_list ',' assignment_expression 	{ $$ = Node_new(',', $1, $3); }
	;

unary_expression
	: postfix_expression				{ $$ = $1; }
	| unary_operator unary_expression	{ $$ = Node_new((int)$1, $2, NULL); }
	;

unary_operator
	: '&'		{ $$ = $1; }
	| '*'		{ $$ = $1; }
	| '+'		{ $$ = $1; }
	| '-'		{ $$ = $1; }
	| '!'		{ $$ = $1; }
	;

multiplicative_expression
	: unary_expression									{ $$ = $1; }
	| multiplicative_expression '*' unary_expression 	{ $$ = Node_new('*', $1, $3); }
	| multiplicative_expression '/' unary_expression 	{ $$ = Node_new('/', $1, $3); }
	| multiplicative_expression '%' unary_expression 	{ $$ = Node_new('%', $1, $3); }
	;

additive_expression
	: multiplicative_expression							{ $$ = $1; }
	| additive_expression '+' multiplicative_expression { $$ = Node_new('+', $1, $3); }
	| additive_expression '-' multiplicative_expression { $$ = Node_new('-', $1, $3); }
	;

relational_expression
	: additive_expression								{ $$ = $1; }
	| relational_expression '<' additive_expression		{ $$ = Node_new('<', $1, $3); }
	| relational_expression '>' additive_expression 	{ $$ = Node_new('>', $1, $3); }
	| relational_expression LE_OP additive_expression 	{ $$ = Node_new(LE_OP, $1, $3); }
	| relational_expression GE_OP additive_expression 	{ $$ = Node_new(GE_OP, $1, $3); }
	;

equality_expression
	: relational_expression								{ $$ = $1; }
	| equality_expression EQ_OP relational_expression 	{ $$ = Node_new(EQ_OP, $1, $3); }
	| equality_expression NE_OP relational_expression 	{ $$ = Node_new(NE_OP, $1, $3); }
	;

assignment_expression
	: equality_expression							{ $$ = $1; }
	| unary_expression '=' assignment_expression 	{ $$ = Node_new('=', $1, $3); }
	;

expression
	: assignment_expression					{ $$ = $1; }
	| expression ',' assignment_expression 	{ $$ = Node_new(',', $1, $3); }
	;

declaration
	: declaration_specifiers ';'						{ $$ = $1; }
	| function_definition								{ $$ = $1; }
	| declaration_specifiers init_declarator_list ';' 	{ $$ = Node_new('~', $1, $2); }
	;

declaration_specifiers
	: storage_class_specifier							{ $$ = $1; }
	| storage_class_specifier declaration_specifiers 	{ $$ = Node_new('~', $1, $2); }
	| type_specifier	        						{ $$ = $1; }
	| type_specifier declaration_specifiers 			{ $$ = Node_new('~', $1, $2); }
	;

init_declarator_list
	: init_declarator							{ $$ = $1; }
	| init_declarator_list ',' init_declarator 	{ $$ = Node_new(',', $1, $3); }
	;

init_declarator
	: declarator							{ $$ = $1; }
	| declarator '=' assignment_expression 	{ $$ = Node_new('=', $1, $3); }
	;

storage_class_specifier
	: EXTERN	{ $$ = $1; }
	| AUTO		{ $$ = $1; }
	;

type_specifier
	: VOID		{ $$ = void_token; }
	| INT		{ $$ = int_token; }
	| FUNCTION	{ $$ = function_token; }
	;

declarator
	: pointer direct_declarator	{ $$ = Node_new('~', $1, $2); }
	| direct_declarator			{ $$ = $1; }
	;

direct_declarator
	: IDENTIFIER									{ $$ = lasttok; }
	| '(' declarator ')'							{ $$ = $2; }
    | direct_declarator '(' parameter_list ')' 		{ $$ = Node_new('F', $1, $3); }
	| direct_declarator '(' identifier_list ')'		{ $$ = Node_new('F', $1, $3); }
	| direct_declarator '(' ')'                		{ $$ = Node_new('F', $1, NULL); }
	;

pointer
	: '*'			{ $$ = (Ast*)1; }
	| '*' pointer	{ $$ = (Ast*)((int)$2+1); }
	;

parameter_list
	: parameter_declaration						{ $$ = $1; }
	| parameter_list ',' parameter_declaration 	{ $$ = Node_new(',', $1, $3); }
	;

parameter_declaration
	: declaration_specifiers declarator 		 { $$ = Node_new('~', $1, $2); }
	| declaration_specifiers abstract_declarator { $$ = Node_new('~', $1, $2); }
	| declaration_specifiers					 { $$ = $1; }
	;

identifier_list
	: IDENTIFIER                    	{ $$ = lasttok; }
	| identifier_list ',' IDENTIFIER 	{ $$ = Node_new(',', $1, lasttok); }
	;

abstract_declarator
	: pointer		        				{ $$ = $1; }
	| direct_abstract_declarator    		{ $$ = $1; }
	| pointer direct_abstract_declarator 	{ $$ = Node_new('G', $1, $2); }
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'    					{ $$ = $2; }
	| '(' ')'    					 					{ $$ = NULL; }
	| '(' parameter_list ')'    	 					{ $$ = $2; }
	| direct_abstract_declarator '(' ')'    			{ $$ = Node_new(APPLY, $1, NULL); }
	| direct_abstract_declarator '(' parameter_list ')'	{ $$ = Node_new(APPLY, $1, $3); }
	;

statement
	: compound_statement	{ $$ = $1; }
	| expression_statement	{ $$ = $1; }
	| selection_statement	{ $$ = $1; }
	| iteration_statement	{ $$ = $1; }
	| jump_statement		{ $$ = $1; }
	;

compound_statement
	: '{' '}'                       			{ $$ = NULL; }
	| '{' statement_list '}'					{ $$ = $2; }
	| '{' declaration_list '}'					{ $$ = $2; }
	| '{' declaration_list statement_list '}' 	{ $$ = Node_new(';', $2, $3); }
	;

declaration_list
	: declaration					{ $$ = $1; }
	| declaration_list declaration 	{ $$ = Node_new(';', $1, $2); }
	;

statement_list
	: statement					{ $$ = $1; }
	| statement_list statement 	{ $$ = Node_new(';', $1, $2); }
	;

expression_statement
	: ';'					{ $$ = NULL; }
	| expression ';'		{ $$ = $1; }
	;

selection_statement
	: IF '(' expression ')' statement 					{ $$ = Node_new(IF, $3, $5); }
	| IF '(' expression ')' statement ELSE statement 	{ $$ = Node_new(IF, $3, Node_new(ELSE, $5, $7)); }
	;

iteration_statement
	: WHILE '(' expression ')' statement { $$ = Node_new(WHILE, $3, $5); }
	;

jump_statement
	: CONTINUE ';'          { $$ = Node_new(CONTINUE, NULL, NULL); }
	| BREAK ';'             { $$ = Node_new(BREAK, NULL, NULL); }
	| RETURN ';'	        { $$ = Node_new(RETURN, NULL, NULL); }
	| RETURN expression ';'	{ $$ = Node_new(RETURN, $2, NULL); }
	;

translation_unit
	: external_declaration					{ $$ = $1; }
	| translation_unit external_declaration { $$ = Node_new('~', $1, $2);}
	;

external_declaration
	: function_definition	{ $$ = $1; }
	| declaration           { $$ = $1; }
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement
		{ $$ = Node_new('D', Node_new('d', $1, Node_new('e', $2, $3)), $4); }
	| declaration_specifiers declarator compound_statement
		{ $$ = Node_new('D', Node_new('d', $1, $2), $3); }
	| declarator declaration_list compound_statement
		{ $$ = Node_new('D', Node_new('d', $1, $2), $3); }
	| declarator compound_statement
		{ $$ = Node_new('D', $1, $2); }
	;
%%
#include <stdio.h>

extern char yytext[];
extern int column;

int yyerror(char *s)
{
	fflush(stdout);
	printf("\n%*s\n%*s\n", column, "^", column, s);
}

