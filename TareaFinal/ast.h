/* -*- mode: c; coding: utf-8; make-target: compile; -*-
 *
 */

#ifndef __LETREC_AST_H__
#define __LETREC_AST_H__

#include <stdio.h>
#include <stdbool.h>

/*******************/
/* IDENTIFICADORES */
/*******************/

typedef struct identifier_t* identifier;

const char* name_of(identifier id);
identifier lookup(char* name);

/***************/
/* EXPRESIONES */
/***************/

typedef struct expression_t* expression;

expression make_const(int value);
bool is_const(expression e);
int const_value(expression e);

expression make_diff(expression left, expression right);
bool is_diff(expression e);
expression diff_left(expression e);
expression diff_right(expression e);

expression make_zerop(expression arg);
bool is_zerop(expression e);
expression zerop_arg(expression e);

expression make_if(expression condi, expression thenb, expression elseb);
bool is_if(expression e);
expression if_condi(expression e);
expression if_thenb(expression e);
expression if_elseb(expression e);

expression make_var(identifier id);
bool is_var(expression e);
identifier var_id(expression e);

expression make_let(identifier id, expression val, expression body);
bool is_let(expression e);
identifier let_id(expression e);
expression let_val(expression e);
expression let_body(expression e);

expression make_proc(identifier id, expression body);
bool is_proc(expression e);
identifier proc_id(expression e);
expression proc_body(expression e);

expression make_cexpr(expression e1, expression e2);
bool is_cexpr(expression e);
expression cexpr_e1(expression e);
expression cexpr_e2(expression e);


expression make_letrec(identifier name, identifier id, expression val, expression body);
bool is_letrec(expression e);
identifier letrec_name(expression e);
identifier letrec_id(expression e);
expression letrec_val(expression e);
expression letrec_body(expression e);
/**********/
/* PARSER */
/**********/

expression parse_ast(FILE* in);

#endif	/* __LETREC_AST_H__ */
