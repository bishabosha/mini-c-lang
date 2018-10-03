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




#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;



