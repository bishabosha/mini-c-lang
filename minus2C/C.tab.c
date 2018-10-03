/* A Bison parser, made by GNU Bison 1.875.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     IDENTIFIER = 258,
     CONSTANT = 259,
     STRING_LITERAL = 260,
     LE_OP = 261,
     GE_OP = 262,
     EQ_OP = 263,
     NE_OP = 264,
     EXTERN = 265,
     AUTO = 266,
     INT = 267,
     VOID = 268,
     FUNCTION = 269,
     APPLY = 270,
     LEAF = 271,
     IF = 272,
     ELSE = 273,
     WHILE = 274,
     CONTINUE = 275,
     BREAK = 276,
     RETURN = 277
   };
#endif
#define IDENTIFIER 258
#define CONSTANT 259
#define STRING_LITERAL 260
#define LE_OP 261
#define GE_OP 262
#define EQ_OP 263
#define NE_OP 264
#define EXTERN 265
#define AUTO 266
#define INT 267
#define VOID 268
#define FUNCTION 269
#define APPLY 270
#define LEAF 271
#define IF 272
#define ELSE 273
#define WHILE 274
#define CONTINUE 275
#define BREAK 276
#define RETURN 277




/* Copy the first part of user declarations.  */
#line 1 "C.y"

#include "nodes.h"
#define YYSTYPE NODE*
#define YYDEBUG 1
extern TOKEN *int_token, *void_token, *function_token, *lasttok;
NODE *ans;


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 138 "C.tab.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  22
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   380

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  39
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  38
/* YYNRULES -- Number of rules. */
#define YYNRULES  107
/* YYNRULES -- Number of states. */
#define YYNSTATES  171

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   277

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    30,     2,     2,     2,    32,    26,     2,
      23,    24,    27,    28,    25,    29,     2,    31,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    36,
      33,    35,    34,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    37,     2,    38,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short yyprhs[] =
{
       0,     0,     3,     5,     7,     9,    11,    15,    17,    21,
      26,    28,    32,    34,    37,    39,    41,    43,    45,    47,
      49,    53,    57,    61,    63,    67,    71,    73,    77,    81,
      85,    89,    91,    95,    99,   101,   105,   107,   111,   114,
     116,   120,   122,   125,   127,   130,   132,   136,   138,   142,
     144,   146,   148,   150,   152,   155,   157,   159,   163,   168,
     173,   177,   179,   182,   184,   188,   191,   194,   196,   198,
     202,   204,   206,   209,   213,   216,   220,   224,   229,   231,
     233,   235,   237,   239,   242,   246,   250,   255,   257,   260,
     262,   265,   267,   270,   276,   284,   290,   293,   296,   299,
     303,   305,   308,   310,   312,   317,   321,   325
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      40,     0,    -1,    74,    -1,     3,    -1,     4,    -1,     5,
      -1,    23,    51,    24,    -1,    41,    -1,    42,    23,    24,
      -1,    42,    23,    43,    24,    -1,    50,    -1,    43,    25,
      50,    -1,    42,    -1,    45,    44,    -1,    26,    -1,    27,
      -1,    28,    -1,    29,    -1,    30,    -1,    44,    -1,    46,
      27,    44,    -1,    46,    31,    44,    -1,    46,    32,    44,
      -1,    46,    -1,    47,    28,    46,    -1,    47,    29,    46,
      -1,    47,    -1,    48,    33,    47,    -1,    48,    34,    47,
      -1,    48,     6,    47,    -1,    48,     7,    47,    -1,    48,
      -1,    49,     8,    48,    -1,    49,     9,    48,    -1,    49,
      -1,    44,    35,    50,    -1,    50,    -1,    51,    25,    50,
      -1,    53,    36,    -1,    76,    -1,    53,    54,    36,    -1,
      56,    -1,    56,    53,    -1,    57,    -1,    57,    53,    -1,
      55,    -1,    54,    25,    55,    -1,    58,    -1,    58,    35,
      50,    -1,    10,    -1,    11,    -1,    13,    -1,    12,    -1,
      14,    -1,    60,    59,    -1,    59,    -1,     3,    -1,    23,
      58,    24,    -1,    59,    23,    61,    24,    -1,    59,    23,
      63,    24,    -1,    59,    23,    24,    -1,    27,    -1,    27,
      60,    -1,    62,    -1,    61,    25,    62,    -1,    53,    58,
      -1,    53,    64,    -1,    53,    -1,     3,    -1,    63,    25,
       3,    -1,    60,    -1,    65,    -1,    60,    65,    -1,    23,
      64,    24,    -1,    23,    24,    -1,    23,    61,    24,    -1,
      65,    23,    24,    -1,    65,    23,    61,    24,    -1,    67,
      -1,    70,    -1,    71,    -1,    72,    -1,    73,    -1,    37,
      38,    -1,    37,    69,    38,    -1,    37,    68,    38,    -1,
      37,    68,    69,    38,    -1,    52,    -1,    68,    52,    -1,
      66,    -1,    69,    66,    -1,    36,    -1,    51,    36,    -1,
      17,    23,    51,    24,    66,    -1,    17,    23,    51,    24,
      66,    18,    66,    -1,    19,    23,    51,    24,    66,    -1,
      20,    36,    -1,    21,    36,    -1,    22,    36,    -1,    22,
      51,    36,    -1,    75,    -1,    74,    75,    -1,    76,    -1,
      52,    -1,    53,    58,    68,    67,    -1,    53,    58,    67,
      -1,    58,    68,    67,    -1,    58,    67,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,    21,    21,    25,    26,    27,    28,    32,    33,    34,
      39,    40,    45,    46,    50,    51,    52,    53,    54,    58,
      59,    61,    63,    68,    69,    71,    76,    77,    79,    81,
      83,    88,    89,    91,    96,    97,   102,   103,   107,   108,
     109,   114,   115,   117,   118,   122,   123,   127,   128,   132,
     133,   137,   138,   139,   143,   144,   148,   149,   150,   151,
     152,   156,   157,   161,   162,   166,   167,   168,   172,   173,
     179,   180,   181,   185,   186,   187,   188,   189,   193,   194,
     195,   196,   197,   201,   202,   203,   204,   208,   209,   213,
     214,   218,   219,   223,   224,   230,   234,   235,   236,   237,
     241,   242,   246,   247,   251,   253,   255,   257
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENTIFIER", "CONSTANT", "STRING_LITERAL", 
  "LE_OP", "GE_OP", "EQ_OP", "NE_OP", "EXTERN", "AUTO", "INT", "VOID", 
  "FUNCTION", "APPLY", "LEAF", "IF", "ELSE", "WHILE", "CONTINUE", "BREAK", 
  "RETURN", "'('", "')'", "','", "'&'", "'*'", "'+'", "'-'", "'!'", "'/'", 
  "'%'", "'<'", "'>'", "'='", "';'", "'{'", "'}'", "$accept", "goal", 
  "primary_expression", "postfix_expression", "argument_expression_list", 
  "unary_expression", "unary_operator", "multiplicative_expression", 
  "additive_expression", "relational_expression", "equality_expression", 
  "assignment_expression", "expression", "declaration", 
  "declaration_specifiers", "init_declarator_list", "init_declarator", 
  "storage_class_specifier", "type_specifier", "declarator", 
  "direct_declarator", "pointer", "parameter_list", 
  "parameter_declaration", "identifier_list", "abstract_declarator", 
  "direct_abstract_declarator", "statement", "compound_statement", 
  "declaration_list", "statement_list", "expression_statement", 
  "selection_statement", "iteration_statement", "jump_statement", 
  "translation_unit", "external_declaration", "function_definition", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,    40,    41,    44,    38,    42,    43,    45,
      33,    47,    37,    60,    62,    61,    59,   123,   125
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    39,    40,    41,    41,    41,    41,    42,    42,    42,
      43,    43,    44,    44,    45,    45,    45,    45,    45,    46,
      46,    46,    46,    47,    47,    47,    48,    48,    48,    48,
      48,    49,    49,    49,    50,    50,    51,    51,    52,    52,
      52,    53,    53,    53,    53,    54,    54,    55,    55,    56,
      56,    57,    57,    57,    58,    58,    59,    59,    59,    59,
      59,    60,    60,    61,    61,    62,    62,    62,    63,    63,
      64,    64,    64,    65,    65,    65,    65,    65,    66,    66,
      66,    66,    66,    67,    67,    67,    67,    68,    68,    69,
      69,    70,    70,    71,    71,    72,    73,    73,    73,    73,
      74,    74,    75,    75,    76,    76,    76,    76
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     1,     1,     1,     1,     3,     1,     3,     4,
       1,     3,     1,     2,     1,     1,     1,     1,     1,     1,
       3,     3,     3,     1,     3,     3,     1,     3,     3,     3,
       3,     1,     3,     3,     1,     3,     1,     3,     2,     1,
       3,     1,     2,     1,     2,     1,     3,     1,     3,     1,
       1,     1,     1,     1,     2,     1,     1,     3,     4,     4,
       3,     1,     2,     1,     3,     2,     2,     1,     1,     3,
       1,     1,     2,     3,     2,     3,     3,     4,     1,     1,
       1,     1,     1,     2,     3,     3,     4,     1,     2,     1,
       2,     1,     2,     5,     7,     5,     2,     2,     2,     3,
       1,     2,     1,     1,     4,     3,     3,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       0,    56,    49,    50,    52,    51,    53,     0,    61,     0,
     103,     0,    41,    43,     0,    55,     0,     2,   100,    39,
       0,    62,     1,    38,     0,    45,    47,    42,    44,     0,
      87,   107,     0,    39,     0,    54,   101,    57,     0,    40,
       0,   105,     0,     3,     4,     5,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,    91,    83,     7,
      12,    19,     0,    23,    26,    31,    34,    36,     0,    89,
      78,     0,     0,    79,    80,    81,    82,    88,   106,    68,
      60,    67,     0,    63,     0,    46,    47,     3,     0,    15,
      48,   104,     0,     0,    96,    97,    98,     0,     0,     0,
       0,    13,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    92,    85,     0,    84,    90,     0,
      65,    70,    66,    71,    58,     0,    59,     0,     0,     0,
      99,     6,     8,     0,    10,    35,    20,    21,    22,    19,
      24,    25,    29,    30,    27,    28,    32,    33,    37,    86,
      74,     0,     0,    72,     0,    64,    69,     0,     0,     9,
       0,    75,    73,    76,     0,    93,    95,    11,    77,     0,
      94
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,     9,    59,    60,   133,    61,    62,    63,    64,    65,
      66,    67,    68,    30,    11,    24,    25,    12,    13,    14,
      15,    16,    82,    83,    84,   122,   123,    69,    70,    32,
      72,    73,    74,    75,    76,    17,    18,    33
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -112
static const short yypact[] =
{
     351,  -112,  -112,  -112,  -112,  -112,  -112,    22,    -8,    24,
    -112,    10,   210,   210,    17,    19,    13,   351,  -112,  -112,
       8,  -112,  -112,  -112,   -15,  -112,    71,  -112,  -112,   124,
    -112,  -112,    17,  -112,   356,    19,  -112,  -112,    22,  -112,
     301,  -112,    17,   258,  -112,  -112,    30,    35,    57,    64,
     273,   312,  -112,    -8,  -112,  -112,  -112,  -112,  -112,  -112,
      37,    44,   301,    93,   -11,     5,   109,  -112,    61,  -112,
    -112,   153,   181,  -112,  -112,  -112,  -112,  -112,  -112,  -112,
    -112,    32,   115,  -112,   144,  -112,    67,  -112,   301,  -112,
    -112,  -112,   301,   301,  -112,  -112,  -112,    63,   163,   284,
     301,  -112,   301,   301,   301,   301,   301,   301,   301,   301,
     301,   301,   301,   301,  -112,  -112,   209,  -112,  -112,   333,
    -112,    73,  -112,    82,  -112,   210,  -112,   107,   191,   219,
    -112,  -112,  -112,   224,  -112,  -112,  -112,  -112,  -112,  -112,
      93,    93,   -11,   -11,   -11,   -11,     5,     5,  -112,  -112,
    -112,   226,    88,    82,   182,  -112,  -112,   237,   237,  -112,
     301,  -112,  -112,  -112,   228,    96,  -112,  -112,  -112,   237,
    -112
};

/* YYPGOTO[NTERM-NUM].  */
static const short yypgoto[] =
{
    -112,  -112,  -112,  -112,  -112,   -39,  -112,    43,    23,   122,
    -112,   -38,   -36,     9,   -12,  -112,    83,  -112,  -112,    -4,
     -10,    -3,  -111,    -2,  -112,     7,    34,   -68,    77,    66,
     100,  -112,  -112,  -112,  -112,  -112,   142,    87
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -57
static const short yytable[] =
{
      27,    28,    90,    20,   118,    21,    35,    26,   151,    10,
      38,   107,   108,     1,    97,    98,     1,   105,   106,     8,
       1,    39,    81,   101,    22,     1,    10,     2,     3,     4,
       5,     6,    37,     7,    86,     1,     7,     8,   109,   110,
       7,    77,    34,   164,     8,     7,    23,    20,   118,     8,
      21,    77,    98,    92,    29,   119,   128,   129,    93,     8,
      99,   134,   135,   136,   137,   138,   139,   139,   139,   139,
     139,   139,   139,   139,     1,   148,     1,   120,   121,   100,
      77,     2,     3,     4,     5,     6,   113,    19,   113,   165,
     166,    31,    42,    94,     7,    71,   119,   114,     8,   130,
      95,   170,    40,    41,    19,   154,    40,    81,    29,    78,
     156,    35,   162,    81,   169,    20,   121,   111,   112,    91,
     102,    85,   167,   155,   103,   104,   152,    43,    44,    45,
     142,   143,   144,   145,     2,     3,     4,     5,     6,   124,
     125,    46,    81,    47,    48,    49,    50,    51,   140,   141,
      52,    53,    54,    55,    56,   153,    43,    44,    45,    36,
      57,    29,    58,     2,     3,     4,     5,     6,   126,   127,
      46,   116,    47,    48,    49,    50,    51,     0,     0,    52,
      53,    54,    55,    56,    87,    44,    45,   131,   113,    57,
      29,   115,     2,     3,     4,     5,     6,     0,    46,     0,
      47,    48,    49,    50,    88,     0,   163,    52,    89,    54,
      55,    56,    87,    44,    45,   157,   113,    57,    29,   117,
       2,     3,     4,     5,     6,     0,    46,     0,    47,    48,
      49,    50,    88,   146,   147,    52,    89,    54,    55,    56,
      87,    44,    45,   158,   113,    57,    29,   149,   159,   160,
     161,   125,   168,   125,    46,     0,    47,    48,    49,    50,
      88,   -56,     0,    52,    89,    54,    55,    56,   -56,   -56,
     -56,   -56,   -56,    57,    29,     0,    87,    44,    45,     0,
       0,     0,     0,     0,     0,     0,     0,    87,    44,    45,
       0,     0,     0,     0,     0,   -56,    88,     0,     0,    52,
      89,    54,    55,    56,    87,    44,    45,    88,   132,    96,
      52,    89,    54,    55,    56,    43,    44,    45,     0,     0,
       0,     0,     0,     0,    88,     0,     0,    52,    89,    54,
      55,    56,     0,     0,     0,    51,     1,     0,    52,    53,
      54,    55,    56,     2,     3,     4,     5,     6,     0,     0,
       0,     0,     0,     0,     1,     0,   119,   150,     0,    79,
       8,     2,     3,     4,     5,     6,     2,     3,     4,     5,
       6,     0,     0,     0,     7,     0,     0,     0,     8,     0,
      80
};

static const short yycheck[] =
{
      12,    13,    40,     7,    72,     8,    16,    11,   119,     0,
      25,     6,     7,     3,    50,    51,     3,    28,    29,    27,
       3,    36,    34,    62,     0,     3,    17,    10,    11,    12,
      13,    14,    24,    23,    38,     3,    23,    27,    33,    34,
      23,    32,    23,   154,    27,    23,    36,    51,   116,    27,
      53,    42,    88,    23,    37,    23,    92,    93,    23,    27,
      23,    99,   100,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,     3,   113,     3,    81,    81,    35,
      71,    10,    11,    12,    13,    14,    25,     0,    25,   157,
     158,    14,    26,    36,    23,    29,    23,    36,    27,    36,
      36,   169,    35,    26,    17,    23,    35,   119,    37,    32,
       3,   121,    24,   125,    18,   119,   119,     8,     9,    42,
      27,    38,   160,   125,    31,    32,   119,     3,     4,     5,
     107,   108,   109,   110,    10,    11,    12,    13,    14,    24,
      25,    17,   154,    19,    20,    21,    22,    23,   105,   106,
      26,    27,    28,    29,    30,   121,     3,     4,     5,    17,
      36,    37,    38,    10,    11,    12,    13,    14,    24,    25,
      17,    71,    19,    20,    21,    22,    23,    -1,    -1,    26,
      27,    28,    29,    30,     3,     4,     5,    24,    25,    36,
      37,    38,    10,    11,    12,    13,    14,    -1,    17,    -1,
      19,    20,    21,    22,    23,    -1,    24,    26,    27,    28,
      29,    30,     3,     4,     5,    24,    25,    36,    37,    38,
      10,    11,    12,    13,    14,    -1,    17,    -1,    19,    20,
      21,    22,    23,   111,   112,    26,    27,    28,    29,    30,
       3,     4,     5,    24,    25,    36,    37,    38,    24,    25,
      24,    25,    24,    25,    17,    -1,    19,    20,    21,    22,
      23,     3,    -1,    26,    27,    28,    29,    30,    10,    11,
      12,    13,    14,    36,    37,    -1,     3,     4,     5,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,    -1,    -1,    -1,    -1,    37,    23,    -1,    -1,    26,
      27,    28,    29,    30,     3,     4,     5,    23,    24,    36,
      26,    27,    28,    29,    30,     3,     4,     5,    -1,    -1,
      -1,    -1,    -1,    -1,    23,    -1,    -1,    26,    27,    28,
      29,    30,    -1,    -1,    -1,    23,     3,    -1,    26,    27,
      28,    29,    30,    10,    11,    12,    13,    14,    -1,    -1,
      -1,    -1,    -1,    -1,     3,    -1,    23,    24,    -1,     3,
      27,    10,    11,    12,    13,    14,    10,    11,    12,    13,
      14,    -1,    -1,    -1,    23,    -1,    -1,    -1,    27,    -1,
      24
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,     3,    10,    11,    12,    13,    14,    23,    27,    40,
      52,    53,    56,    57,    58,    59,    60,    74,    75,    76,
      58,    60,     0,    36,    54,    55,    58,    53,    53,    37,
      52,    67,    68,    76,    23,    59,    75,    24,    25,    36,
      35,    67,    68,     3,     4,     5,    17,    19,    20,    21,
      22,    23,    26,    27,    28,    29,    30,    36,    38,    41,
      42,    44,    45,    46,    47,    48,    49,    50,    51,    66,
      67,    68,    69,    70,    71,    72,    73,    52,    67,     3,
      24,    53,    61,    62,    63,    55,    58,     3,    23,    27,
      50,    67,    23,    23,    36,    36,    36,    51,    51,    23,
      35,    44,    27,    31,    32,    28,    29,     6,     7,    33,
      34,     8,     9,    25,    36,    38,    69,    38,    66,    23,
      58,    60,    64,    65,    24,    25,    24,    25,    51,    51,
      36,    24,    24,    43,    50,    50,    44,    44,    44,    44,
      46,    46,    47,    47,    47,    47,    48,    48,    50,    38,
      24,    61,    64,    65,    23,    62,     3,    24,    24,    24,
      25,    24,    24,    24,    61,    66,    66,    50,    24,    18,
      66
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrlab1

/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)         \
  Current.first_line   = Rhs[1].first_line;      \
  Current.first_column = Rhs[1].first_column;    \
  Current.last_line    = Rhs[N].last_line;       \
  Current.last_column  = Rhs[N].last_column;
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (cinluded).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylineno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylineno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 21 "C.y"
    { ans = yyval = yyvsp[0];;}
    break;

  case 3:
#line 25 "C.y"
    { yyval = make_leaf(lasttok); ;}
    break;

  case 4:
#line 26 "C.y"
    { yyval = make_leaf(lasttok); ;}
    break;

  case 5:
#line 27 "C.y"
    { yyval = make_leaf(lasttok); ;}
    break;

  case 6:
#line 28 "C.y"
    { yyval = yyvsp[-1]; ;}
    break;

  case 7:
#line 32 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 8:
#line 33 "C.y"
    { yyval = make_node(APPLY, yyvsp[-2], NULL); ;}
    break;

  case 9:
#line 34 "C.y"
    {
				          yyval = make_node(APPLY, yyvsp[-3], yyvsp[-1]); ;}
    break;

  case 10:
#line 39 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 11:
#line 40 "C.y"
    {
          yyval = make_node(',', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 12:
#line 45 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 13:
#line 46 "C.y"
    { yyval = make_node((int)yyvsp[-1], yyvsp[0], NULL); ;}
    break;

  case 14:
#line 50 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 15:
#line 51 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 16:
#line 52 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 17:
#line 53 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 18:
#line 54 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 19:
#line 58 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 20:
#line 59 "C.y"
    {
                                          yyval = make_node('*', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 21:
#line 61 "C.y"
    {
                                          yyval = make_node('/', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 22:
#line 63 "C.y"
    {
                                          yyval = make_node('%', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 23:
#line 68 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 24:
#line 69 "C.y"
    {
                                          yyval = make_node('+', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 25:
#line 71 "C.y"
    {
                                          yyval = make_node('-', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 26:
#line 76 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 27:
#line 77 "C.y"
    {
                                          yyval = make_node('<', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 28:
#line 79 "C.y"
    {
                                          yyval = make_node('>', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 29:
#line 81 "C.y"
    {
                                          yyval = make_node(LE_OP, yyvsp[-2], yyvsp[0]); ;}
    break;

  case 30:
#line 83 "C.y"
    {
                                          yyval = make_node(GE_OP, yyvsp[-2], yyvsp[0]); ;}
    break;

  case 31:
#line 88 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 32:
#line 89 "C.y"
    {
                                          yyval = make_node(EQ_OP, yyvsp[-2], yyvsp[0]); ;}
    break;

  case 33:
#line 91 "C.y"
    {
                                          yyval = make_node(NE_OP, yyvsp[-2], yyvsp[0]); ;}
    break;

  case 34:
#line 96 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 35:
#line 97 "C.y"
    {
                                          yyval = make_node('=', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 36:
#line 102 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 37:
#line 103 "C.y"
    { yyval = make_node(',', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 38:
#line 107 "C.y"
    { yyval = yyvsp[-1]; ;}
    break;

  case 39:
#line 108 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 40:
#line 109 "C.y"
    {
                                                  yyval = make_node('~', yyvsp[-2], yyvsp[-1]); ;}
    break;

  case 41:
#line 114 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 42:
#line 115 "C.y"
    { 
                                                  yyval = make_node('~', yyvsp[-1], yyvsp[0]); ;}
    break;

  case 43:
#line 117 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 44:
#line 118 "C.y"
    { yyval = make_node('~', yyvsp[-1], yyvsp[0]); ;}
    break;

  case 45:
#line 122 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 46:
#line 123 "C.y"
    { yyval = make_node(',', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 47:
#line 127 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 48:
#line 128 "C.y"
    { yyval = make_node('=', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 49:
#line 132 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 50:
#line 133 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 51:
#line 137 "C.y"
    { yyval = make_leaf(void_token); ;}
    break;

  case 52:
#line 138 "C.y"
    { yyval = make_leaf(int_token); ;}
    break;

  case 53:
#line 139 "C.y"
    { yyval = make_leaf(function_token); ;}
    break;

  case 54:
#line 143 "C.y"
    { yyval = make_node('~', yyvsp[-1], yyvsp[0]); ;}
    break;

  case 55:
#line 144 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 56:
#line 148 "C.y"
    { yyval = make_leaf(lasttok); ;}
    break;

  case 57:
#line 149 "C.y"
    { yyval = yyvsp[-1]; ;}
    break;

  case 58:
#line 150 "C.y"
    { yyval = make_node('F', yyvsp[-3], yyvsp[-1]); ;}
    break;

  case 59:
#line 151 "C.y"
    { yyval = make_node('F', yyvsp[-3], yyvsp[-1]); ;}
    break;

  case 60:
#line 152 "C.y"
    { yyval = make_node('F', yyvsp[-2], NULL); ;}
    break;

  case 61:
#line 156 "C.y"
    { yyval = (NODE*)1; ;}
    break;

  case 62:
#line 157 "C.y"
    { yyval = (NODE*)((int)yyvsp[0]+1); ;}
    break;

  case 63:
#line 161 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 64:
#line 162 "C.y"
    { yyval = make_node(',', yyvsp[-2], yyvsp[0]); ;}
    break;

  case 65:
#line 166 "C.y"
    { yyval = make_node('~', yyvsp[-1], yyvsp[0]); ;}
    break;

  case 66:
#line 167 "C.y"
    { yyval = make_node('~', yyvsp[-1], yyvsp[0]); ;}
    break;

  case 67:
#line 168 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 68:
#line 172 "C.y"
    { yyval = make_leaf(lasttok); ;}
    break;

  case 69:
#line 173 "C.y"
    {
                                          yyval = make_node(',', yyvsp[-2],
                                                              make_leaf(lasttok)); ;}
    break;

  case 70:
#line 179 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 71:
#line 180 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 72:
#line 181 "C.y"
    { yyval = make_node('G', yyvsp[-1], yyvsp[0]); ;}
    break;

  case 73:
#line 185 "C.y"
    { yyval = yyvsp[-1]; ;}
    break;

  case 74:
#line 186 "C.y"
    { yyval = NULL; ;}
    break;

  case 75:
#line 187 "C.y"
    { yyval = yyvsp[-1]; ;}
    break;

  case 76:
#line 188 "C.y"
    { yyval = make_node(APPLY, yyvsp[-2], NULL); ;}
    break;

  case 77:
#line 189 "C.y"
    { yyval = make_node(APPLY, yyvsp[-3], yyvsp[-1]); ;}
    break;

  case 78:
#line 193 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 79:
#line 194 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 80:
#line 195 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 81:
#line 196 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 82:
#line 197 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 83:
#line 201 "C.y"
    { yyval = NULL; ;}
    break;

  case 84:
#line 202 "C.y"
    { yyval = yyvsp[-1]; ;}
    break;

  case 85:
#line 203 "C.y"
    { yyval = yyvsp[-1]; ;}
    break;

  case 86:
#line 204 "C.y"
    { yyval = make_node(';', yyvsp[-2], yyvsp[-1]); ;}
    break;

  case 87:
#line 208 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 88:
#line 209 "C.y"
    { yyval = make_node(';', yyvsp[-1], yyvsp[0]); ;}
    break;

  case 89:
#line 213 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 90:
#line 214 "C.y"
    { yyval = make_node(';', yyvsp[-1], yyvsp[0]); ;}
    break;

  case 91:
#line 218 "C.y"
    { yyval = NULL; ;}
    break;

  case 92:
#line 219 "C.y"
    { yyval = yyvsp[-1]; ;}
    break;

  case 93:
#line 223 "C.y"
    { yyval = make_node(IF, yyvsp[-2], yyvsp[0]); ;}
    break;

  case 94:
#line 225 "C.y"
    { yyval = make_node(IF, yyvsp[-4],
                                                        make_node(ELSE, yyvsp[-2], yyvsp[0])); ;}
    break;

  case 95:
#line 230 "C.y"
    { yyval = make_node(WHILE, yyvsp[-2], yyvsp[0]); ;}
    break;

  case 96:
#line 234 "C.y"
    { yyval = make_node(CONTINUE, NULL, NULL); ;}
    break;

  case 97:
#line 235 "C.y"
    { yyval = make_node(BREAK, NULL, NULL); ;}
    break;

  case 98:
#line 236 "C.y"
    { yyval = make_node(RETURN, NULL, NULL); ;}
    break;

  case 99:
#line 237 "C.y"
    { yyval = make_node(RETURN, yyvsp[-1], NULL); ;}
    break;

  case 100:
#line 241 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 101:
#line 242 "C.y"
    { yyval = make_node('~', yyvsp[-1], yyvsp[0]);;}
    break;

  case 102:
#line 246 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 103:
#line 247 "C.y"
    { yyval = yyvsp[0]; ;}
    break;

  case 104:
#line 251 "C.y"
    {
          yyval = make_node('D', make_node('d', yyvsp[-3], make_node('e', yyvsp[-2], yyvsp[-1])), yyvsp[0]); ;}
    break;

  case 105:
#line 253 "C.y"
    {
          yyval = make_node('D', make_node('d', yyvsp[-2], yyvsp[-1]), yyvsp[0]); ;}
    break;

  case 106:
#line 255 "C.y"
    {
          yyval = make_node('D', make_node('d', yyvsp[-2], yyvsp[-1]), yyvsp[0]); ;}
    break;

  case 107:
#line 257 "C.y"
    { yyval = make_node('D', yyvsp[-1], yyvsp[0]); ;}
    break;


    }

/* Line 991 of yacc.c.  */
#line 1775 "C.tab.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("syntax error, unexpected ") + 1;
	  yysize += yystrlen (yytname[yytype]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* Return failure if at end of input.  */
      if (yychar == YYEOF)
        {
	  /* Pop the error token.  */
          YYPOPSTACK;
	  /* Pop the rest of the stack.  */
	  while (yyss < yyssp)
	    {
	      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
	      yydestruct (yystos[*yyssp], yyvsp);
	      YYPOPSTACK;
	    }
	  YYABORT;
        }

      YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
      yydestruct (yytoken, &yylval);
      yychar = YYEMPTY;

    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab2;


/*----------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action.  |
`----------------------------------------------------*/
yyerrlab1:

  /* Suppress GCC warning that yyerrlab1 is unused when no action
     invokes YYERROR.  */
#if defined (__GNUC_MINOR__) && 2093 <= (__GNUC__ * 1000 + __GNUC_MINOR__) \
    && !defined __cplusplus
  __attribute__ ((__unused__))
#endif


  goto yyerrlab2;


/*---------------------------------------------------------------.
| yyerrlab2 -- pop states until the error token can be shifted.  |
`---------------------------------------------------------------*/
yyerrlab2:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      yyvsp--;
      yystate = *--yyssp;

      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 259 "C.y"

#include <stdio.h>

extern char yytext[];
extern int column;

int yyerror(char *s)
{
	fflush(stdout);
	printf("\n%*s\n%*s\n", column, "^", column, s);
}


