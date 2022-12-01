/* -*- mode: c; coding: utf-8; make-target: compile; -*-
 *
 */

#include <stdlib.h>
#include <string.h>
#include <config.h>
#include <ast.h>

/*********************/
/* BUFFER DE NOMBRES */
/*********************/

char buffer[buffer_size];
const char* buffer_bound = buffer + buffer_size;
char* next_str = buffer;

char* store_str(const char* source) {
  char* target = next_str;

  while (*source != '\0') {
    if (next_str >= buffer_bound) return NULL;
    *next_str++ = *source++;
  }

  *next_str++ = '\0';

  return target;
}

/*******************/
/* IDENTIFICADORES */
/*******************/

struct identifier_t {
  char* name;
};

const char* name_of(struct identifier_t* id) {
  return id->name;
}

struct identifier_t table[table_size];
const struct identifier_t* id_boundary = table+table_size;

size_t idhash(const char* name) {
  size_t h = 0;
  char c;
  while ((c = *name++) != '\0') h = 31 * h + c;
  return h;
}

struct identifier_t* lookup(char* name) {
  struct identifier_t* id = &table[idhash(name) % table_size];

  for (size_t probes = 0; probes < table_size; probes++) {
    if (id->name != NULL && strcmp(id->name, name) == 0)
      return id;

    if (id->name == NULL) {
      id->name = store_str(name);
      if (id->name == NULL) break;
      return id;
    }

    if (++id >= id_boundary) id = table;
  }

  return NULL;
}

/***************/
/* EXPRESIONES */
/***************/

enum expr_type {
  CONST_EXPR,
  DIFF_EXPR,
  ZEROP_EXPR,
  IF_EXPR,
  VAR_EXPR,
  LET_EXPR,
  PROC_EXPR,
  CALL_EXPR,
  LETREC_EXPR
};

typedef int const_expr;

struct diff_expr {
  struct expression_t* left;
  struct expression_t* right;
};

struct zerop_expr {
  struct expression_t* arg;
};

struct if_expr {
  struct expression_t* condi;
  struct expression_t* thenb;
  struct expression_t* elseb;
};

typedef struct identifier_t* var_expr;

struct let_expr {
  struct identifier_t* id;
  struct expression_t* val;
  struct expression_t* body;
};
struct letrec_expr {
  struct identifier_t* name;
  struct identifier_t* id;
  struct expression_t* val;
  struct expression_t* body;
};
struct proc_expr {
  struct identifier_t* id;
  struct expression_t* body;
};

struct cexpr_expr {
  struct expression_t* e1;
  struct expression_t* e2;
};
//*************************************UNION*************************************
struct expression_t {
  enum expr_type variant;
  union {
    const_expr the_const;
    struct diff_expr the_diff;
    struct zerop_expr the_zerop;
    struct if_expr the_if;
    var_expr the_var;
    struct let_expr the_let;
    struct letrec_expr the_letrec;
    struct proc_expr the_proc;
    struct cexpr_expr the_cexpr;
  };
};

/* Buffer de expresiones */
struct expression_t exprs[exprs_size];
const struct expression_t* expr_bound = exprs + exprs_size;
struct expression_t* next_expr = exprs;

struct expression_t* store_expr(enum expr_type variant) {
  if (next_expr >= expr_bound) return NULL;
  next_expr->variant = variant;
  return next_expr++;
}

/* Constante numérica */

struct expression_t* make_const(int value) {
  struct expression_t* e = store_expr(CONST_EXPR);
  if (e == NULL) return NULL;
  e->the_const = value;
  return e;
}

bool is_const(struct expression_t* e) {
  return e->variant == CONST_EXPR;
}

int const_value(struct expression_t* e) {
  return e->the_const;
}

/* Resta */

struct expression_t* make_diff(struct expression_t* left, struct expression_t* right) {
  struct expression_t* e = store_expr(DIFF_EXPR);
  if (e == NULL) return NULL;
  e->the_diff.left = left;
  e->the_diff.right = right;
  return e;
}

bool is_diff(struct expression_t* e) {
  return e->variant == DIFF_EXPR;
}

struct expression_t* diff_left(struct expression_t* e) {
  return e->the_diff.left;
}

struct expression_t* diff_right(struct expression_t* e) {
  return e->the_diff.right;
}

/* Predicado cero */

struct expression_t* make_zerop(struct expression_t* arg) {
  struct expression_t* e = store_expr(ZEROP_EXPR);
  if (e == NULL) return NULL;
  e->the_zerop.arg = arg;
  return e;
}

bool is_zerop(struct expression_t* e) {
  return e->variant == ZEROP_EXPR;
}

struct expression_t* zerop_arg(struct expression_t* e) {
  return e->the_zerop.arg;
}

/* Condicional if */

struct expression_t* make_if(struct expression_t* condi, struct expression_t* thenb, struct expression_t* elseb) {
  struct expression_t* e = store_expr(IF_EXPR);
  if (e == NULL) return NULL;
  e->the_if.condi = condi;
  e->the_if.thenb = thenb;
  e->the_if.elseb = elseb;
  return e;
}

bool is_if(struct expression_t* e) {
  return e->variant == IF_EXPR;
}

struct expression_t* if_condi(struct expression_t* e) {
  return e->the_if.condi;
}

struct expression_t* if_thenb(struct expression_t* e) {
  return e->the_if.thenb;
}

struct expression_t* if_elseb(struct expression_t* e) {
  return e->the_if.elseb;
}

/* Variable */

struct expression_t* make_var(struct identifier_t* id) {
  struct expression_t* e = store_expr(VAR_EXPR);
  if (e == NULL) return NULL;
  e->the_var = id;
  return e;
}

bool is_var(struct expression_t* e) {
  return e->variant == VAR_EXPR;
}

struct identifier_t* var_id(struct expression_t* e) {
  return e->the_var;
}

/* Vinculación de valores let */

struct expression_t* make_let(struct identifier_t* id, struct expression_t* val, struct expression_t* body) {
  struct expression_t* e = store_expr(LET_EXPR);
  if (e == NULL) return NULL;
  e->the_let.id = id;
  e->the_let.val = val;
  e->the_let.body = body;
  return e;
}
struct expression_t* make_letrec(struct identifier_t* name, struct identifier_t* id, struct expression_t* val, struct expression_t* body) {
  struct expression_t* e = store_expr(LETREC_EXPR);
  if (e == NULL) return NULL;
  e->the_letrec.name = name;
  e->the_letrec.id = id;
  e->the_letrec.val = val;
  e->the_letrec.body = body;
  return e;
}
bool is_let(struct expression_t* e) {
  return e->variant == LET_EXPR;
}
bool is_letrec(struct expression_t* e) {
  return e->variant == LETREC_EXPR;
}

struct identifier_t* let_id(struct expression_t* e) {
  return e->the_let.id;
}

struct expression_t* let_val(struct expression_t* e) {
  return e->the_let.val;
}

struct expression_t* let_body(struct expression_t* e) {
  return e->the_let.body;
}

struct identifier_t* letrec_name(struct expression_t* e) {
  return e->the_letrec.name;
}

struct identifier_t* letrec_id(struct expression_t* e) {
  return e->the_letrec.id;
}

struct expression_t* letrec_val(struct expression_t* e) {
  return e->the_letrec.val;
}

struct expression_t* letrec_body(struct expression_t* e) {
  return e->the_letrec.body;
}
/* PROC */

struct expression_t* make_proc(struct identifier_t* id, struct expression_t* body) {
  struct expression_t* e = store_expr(PROC_EXPR);
  if (e == NULL) return NULL;
  e->the_proc.id = id;
  e->the_proc.body = body;
  return e;
};

bool is_proc(struct expression_t* e) {
  return e->variant == PROC_EXPR;
}

struct identifier_t* proc_id(struct expression_t* e) {
  return e->the_proc.id;
}

struct expression_t* proc_body(struct expression_t* e) {
  return e->the_proc.body;
}


/* CALL_EXP */

struct expression_t* make_cexpr(struct expression_t* e1, struct expression_t* e2) {
  struct expression_t* e = store_expr(CALL_EXPR);
  if (e == NULL) return NULL;
  e->the_cexpr.e1 = e1;
  e->the_cexpr.e2 = e2;
  return e;
};

bool is_cexpr(struct expression_t* e) {
  return e->variant == CALL_EXPR;
}

struct expression_t* cexpr_e1(struct expression_t* e) {
  return e->the_cexpr.e1;
}

struct expression_t* cexpr_e2(struct expression_t* e) {
  return e->the_cexpr.e2;
}






