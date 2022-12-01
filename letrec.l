/* -*- mode: flex; coding: utf-8; make-target: compile; -*-
 *
 */

%option noyywrap nodefault yylineno noinput nounput always-interactive

%{
#include <stdlib.h>
#include <stdio.h>
#include <ast.h>
#include "parse.h"
#include "letrec.tab.h"
%}

%%

"-"     { return MINUS; }
"("     { return OPAREN; }
","     { return COMMA; }
")"     { return CPAREN; }
"zero?" { return ZEROP; }
"if"    { return IF; }
"then"  { return THEN; }
"else"  { return ELSE; }
"let"   { return LET; }
"letrec"   { return LETREC; }
"proc"  { return PROC; }
"="     { return EQUALS; }
"in"    { return IN; }
":exit"  { return EXIT; }

[a-zA-Z][a-zA-Z0-9]* {
    identifier id = lookup(yytext);
    if (id == NULL) yyerror("Sin memoria para crear identificador: %s", yytext);
    yylval.id = id;
    return IDENTIFIER;
}

[+-]?[0-9]+ {
    yylval.num = atoi(yytext);
    return NUMBER;
}

[ \t\n]

. { yyerror("No se reconoce caracter %c", *yytext); }

%%
