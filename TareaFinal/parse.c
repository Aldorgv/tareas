/* -*- mode: c; coding: utf-8; make-target: compile; -*-
 * 
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include "parse.h"
#include "letrec.tab.h"

void yyerror(char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  fprintf(stderr, "%d: error: ", yylineno);
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  exit(EXIT_FAILURE);
}

expression parsed_expr;

expression parse_ast(FILE* in) {
  yyin = in;
  int status = yyparse();
  if (status != 0) return NULL;
  return parsed_expr;
}
