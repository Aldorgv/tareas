/* -*- mode: bison; coding: utf-8; make-target: compile; -*-
 *
 */

%debug

%{
#include <stdio.h>
#include <stdlib.h>
#include <ast.h>
#include "parse.h"
%}

%union {
    expression e;
    identifier id;
    int num;
}

%token	<num>		NUMBER
%token	<id>		IDENTIFIER
%token MINUS OPAREN COMMA CPAREN ZEROP IF THEN ELSE LET PROC LETREC EQUALS IN EXIT
%type	<e>		expr

%start top

%%
top:		expr { parsed_expr = $1; YYACCEPT; }
	|	EXIT { exit(0); }
	;
expr:	    	NUMBER { $$ = make_const($1); }
	|	MINUS OPAREN expr COMMA expr CPAREN { $$ = make_diff($3, $5); }
	|	ZEROP OPAREN expr CPAREN { $$ = make_zerop($3); }
	|	IF expr THEN expr ELSE expr { $$ = make_if($2, $4, $6); }
	|	IDENTIFIER { $$ = make_var($1); }
    |   LETREC IDENTIFIER OPAREN IDENTIFIER CPAREN EQUALS expr IN expr { $$ = make_letrec($2, $4, $7, $9);}
	|	LET IDENTIFIER EQUALS expr IN expr { $$ = make_let($2, $4, $6); }
	|   PROC OPAREN IDENTIFIER CPAREN expr { $$ = make_proc($3, $5); }
    |   OPAREN expr expr CPAREN { $$ = make_cexpr($2, $3); }

	;

%%
