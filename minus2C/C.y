%{
#include "nodes.h"
#define YYSTYPE NODE*
#define YYDEBUG 1
extern TOKEN *int_token, *void_token, *function_token, *lasttok;
NODE *ans;
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
	: IDENTIFIER 			{ $$ = make_leaf(lasttok); }
	| CONSTANT 			{ $$ = make_leaf(lasttok); }
	| STRING_LITERAL 		{ $$ = make_leaf(lasttok); }
	| '(' expression ')' 		{ $$ = $2; }
	;

postfix_expression
	: primary_expression		{ $$ = $1; }
	| postfix_expression '(' ')'    { $$ = make_node(APPLY, $1, NULL); }
	| postfix_expression '(' argument_expression_list ')' {
				          $$ = make_node(APPLY, $1, $3); }
	;

argument_expression_list
	: assignment_expression		{ $$ = $1; }
	| argument_expression_list ',' assignment_expression {
          $$ = make_node(',', $1, $3); }
	;

unary_expression
	: postfix_expression		{ $$ = $1; }
	| unary_operator unary_expression { $$ = make_node((int)$1, $2, NULL); }
	;

unary_operator
	: '&'		{ $$ = $1; }
	| '*'		{ $$ = $1; }
	| '+'		{ $$ = $1; }
	| '-'		{ $$ = $1; }
	| '!'		{ $$ = $1; }
	;

multiplicative_expression
	: unary_expression		{ $$ = $1; }
	| multiplicative_expression '*' unary_expression {
                                          $$ = make_node('*', $1, $3); }
	| multiplicative_expression '/' unary_expression {
                                          $$ = make_node('/', $1, $3); }
	| multiplicative_expression '%' unary_expression {
                                          $$ = make_node('%', $1, $3); }
	;

additive_expression
	: multiplicative_expression		{ $$ = $1; }
	| additive_expression '+' multiplicative_expression {
                                          $$ = make_node('+', $1, $3); }
	| additive_expression '-' multiplicative_expression {
                                          $$ = make_node('-', $1, $3); }
	;

relational_expression
	: additive_expression		{ $$ = $1; }
	| relational_expression '<' additive_expression	{
                                          $$ = make_node('<', $1, $3); }
	| relational_expression '>' additive_expression {
                                          $$ = make_node('>', $1, $3); }
	| relational_expression LE_OP additive_expression {
                                          $$ = make_node(LE_OP, $1, $3); }
	| relational_expression GE_OP additive_expression {
                                          $$ = make_node(GE_OP, $1, $3); }
	;

equality_expression
	: relational_expression		{ $$ = $1; }
	| equality_expression EQ_OP relational_expression {
                                          $$ = make_node(EQ_OP, $1, $3); }
	| equality_expression NE_OP relational_expression {
                                          $$ = make_node(NE_OP, $1, $3); }
	;

assignment_expression
	: equality_expression		{ $$ = $1; }
	| unary_expression '=' assignment_expression {
                                          $$ = make_node('=', $1, $3); }
	;

expression
	: assignment_expression		{ $$ = $1; }
	| expression ',' assignment_expression { $$ = make_node(',', $1, $3); }
	;

declaration
	: declaration_specifiers ';'		{ $$ = $1; }
	| function_definition			{ $$ = $1; }
	| declaration_specifiers init_declarator_list ';' {
                                                  $$ = make_node('~', $1, $2); }
	;

declaration_specifiers
	: storage_class_specifier		{ $$ = $1; }
	| storage_class_specifier declaration_specifiers { 
                                                  $$ = make_node('~', $1, $2); }
	| type_specifier	        	{ $$ = $1; }
	| type_specifier declaration_specifiers { $$ = make_node('~', $1, $2); }
	;

init_declarator_list
	: init_declarator		{ $$ = $1; }
	| init_declarator_list ',' init_declarator { $$ = make_node(',', $1, $3); }
	;

init_declarator
	: declarator		{ $$ = $1; }
	| declarator '=' assignment_expression { $$ = make_node('=', $1, $3); }
	;

storage_class_specifier
	: EXTERN	{ $$ = $1; }
	| AUTO		{ $$ = $1; }
	;

type_specifier
	: VOID		{ $$ = make_leaf(void_token); }
	| INT		{ $$ = make_leaf(int_token); }
	| FUNCTION	{ $$ = make_leaf(function_token); }
	;

declarator
	: pointer direct_declarator	{ $$ = make_node('~', $1, $2); }
	| direct_declarator		{ $$ = $1; }
	;

direct_declarator
	: IDENTIFIER		{ $$ = make_leaf(lasttok); }
	| '(' declarator ')'	{ $$ = $2; }
        | direct_declarator '(' parameter_list ')' { $$ = make_node('F', $1, $3); }
	| direct_declarator '(' identifier_list ')'{ $$ = make_node('F', $1, $3); }
	| direct_declarator '(' ')'                { $$ = make_node('F', $1, NULL); }
	;

pointer
	: '*'                   { $$ = (NODE*)1; }
	| '*' pointer           { $$ = (NODE*)((int)$2+1); }
	;

parameter_list
	: parameter_declaration		{ $$ = $1; }
	| parameter_list ',' parameter_declaration { $$ = make_node(',', $1, $3); }
	;

parameter_declaration
	: declaration_specifiers declarator { $$ = make_node('~', $1, $2); }
	| declaration_specifiers abstract_declarator { $$ = make_node('~', $1, $2); }
	| declaration_specifiers	{ $$ = $1; }
	;

identifier_list
	: IDENTIFIER                    { $$ = make_leaf(lasttok); }
	| identifier_list ',' IDENTIFIER {
                                          $$ = make_node(',', $1,
                                                              make_leaf(lasttok)); }
	;

abstract_declarator
	: pointer		        { $$ = $1; }
	| direct_abstract_declarator    { $$ = $1; }
	| pointer direct_abstract_declarator { $$ = make_node('G', $1, $2); }
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'    { $$ = $2; }
	| '(' ')'    { $$ = NULL; }
	| '(' parameter_list ')'    { $$ = $2; }
	| direct_abstract_declarator '(' ')'    { $$ = make_node(APPLY, $1, NULL); }
	| direct_abstract_declarator '(' parameter_list ')'   { $$ = make_node(APPLY, $1, $3); }
	;

statement
	: compound_statement		{ $$ = $1; }
	| expression_statement		{ $$ = $1; }
	| selection_statement		{ $$ = $1; }
	| iteration_statement		{ $$ = $1; }
	| jump_statement		{ $$ = $1; }
	;

compound_statement
	: '{' '}'                       { $$ = NULL; }
	| '{' statement_list '}'	{ $$ = $2; }
	| '{' declaration_list '}'	{ $$ = $2; }
	| '{' declaration_list statement_list '}' { $$ = make_node(';', $2, $3); }
	;

declaration_list
	: declaration			{ $$ = $1; }
	| declaration_list declaration { $$ = make_node(';', $1, $2); }
	;

statement_list
	: statement			{ $$ = $1; }
	| statement_list statement 	{ $$ = make_node(';', $1, $2); }
	;

expression_statement
	: ';'				{ $$ = NULL; }
	| expression ';'		{ $$ = $1; }
	;

selection_statement
	: IF '(' expression ')' statement { $$ = make_node(IF, $3, $5); }
	| IF '(' expression ')' statement ELSE statement
                                          { $$ = make_node(IF, $3,
                                                        make_node(ELSE, $5, $7)); }
	;

iteration_statement
	: WHILE '(' expression ')' statement { $$ = make_node(WHILE, $3, $5); }
	;

jump_statement
	: CONTINUE ';'                  { $$ = make_node(CONTINUE, NULL, NULL); }
	| BREAK ';'                     { $$ = make_node(BREAK, NULL, NULL); }
	| RETURN ';'	                { $$ = make_node(RETURN, NULL, NULL); }
	| RETURN expression ';'		{ $$ = make_node(RETURN, $2, NULL); }
	;

translation_unit
	: external_declaration		{ $$ = $1; }
	| translation_unit external_declaration { $$ = make_node('~', $1, $2);}
	;

external_declaration
	: function_definition            { $$ = $1; }
	| declaration                    { $$ = $1; }
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement {
          $$ = make_node('D', make_node('d', $1, make_node('e', $2, $3)), $4); }
	| declaration_specifiers declarator compound_statement  {
          $$ = make_node('D', make_node('d', $1, $2), $3); }
	| declarator declaration_list compound_statement  {
          $$ = make_node('D', make_node('d', $1, $2), $3); }
	| declarator compound_statement { $$ = make_node('D', $1, $2); }
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

