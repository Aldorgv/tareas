/* -*- mode: c; coding: utf-8; make-target: compile; -*-
 * 
 */

#ifndef __LETREC_PARSE_H__
#define __LETREC_PARSE_H__

#include <ast.h>

extern int yylineno;
void yyerror(char *fmt, ...);
int yylex(void);
extern FILE* yyin;

extern expression parsed_expr;

#endif	/* __LETREC_PARSE_H__ */
