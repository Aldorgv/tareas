/* -*- mode: c; coding: utf-8; make-target: compile; -*-
 *
 */

#include <config.h>
#include <ast.h>
#include <driver.h>

#include "parse.h"
#include "letrec.tab.h"


void print_ast(FILE* out, expression e) {
  if (e == NULL) {
    yyerror("ERROR FATAL: EXPRESION ES NULL");
    return;
  }

  if (is_const(e)) {
    fprintf(out, "%d", const_value(e));
  } else if (is_diff(e)) {
    fprintf(out, "-(");
    print_ast(out, diff_left(e));
    fprintf(out, ", ");
    print_ast(out, diff_right(e));
    fprintf(out, ")");
  } else if (is_zerop(e)) {
    fprintf(out, "zero?(");
    print_ast(out, zerop_arg(e));
    fprintf(out, ")");
  } else if (is_if(e)) {
    fprintf(out, "if ");
    print_ast(out, if_condi(e));
    fprintf(out, " then ");
    print_ast(out, if_thenb(e));
    fprintf(out, " else ");
    print_ast(out, if_elseb(e));
  } else if (is_var(e)) {
    fprintf(out, "%s", name_of(var_id(e)));
  } else if (is_let(e)) {
    fprintf(out, "let %s = ", name_of(let_id(e)));
    print_ast(out, let_val(e));
    fprintf(out, " in ");
    print_ast(out, let_body(e));
  } else if (is_proc(e)) {
    fprintf(out, "proc %s = ", name_of(proc_id(e)));
    print_ast(out, proc_body(e));
  } else if (is_cexpr(e)) {
    print_ast(out, cexpr_e1(e));
    print_ast(out, cexpr_e2(e));
  } else if (is_letrec(e)) {
    printf("letrec\n");
  } else {
    yyerror("ERROR FATAL: EXPRESION NO SE RECONOCE");
  }

  return;
}

/*************/
/* SEMÁNTICA */
/*************/

/* Valores expresados */

typedef enum {
  NUMVAL,
  BOOLVAL,
  PROCVAL,

  ERRORVAL
} expval_type;

typedef struct {
  expval_type variant;
  union {
    int number;
    bool truth;
    char* error;
    struct expression_t* expr;
  };
} expval;

expval make_numval(int val) {
  expval x = { .variant = NUMVAL, .number = val };
  return x;
}

expval make_boolval(bool val) {
  expval x = { .variant = BOOLVAL, .truth = val };
  return x;
}
//****************************
expval make_procval(expression  e) {
  expval x = { .variant = PROCVAL, .expr = e };
  return x;
}
//****************************

expval make_errorval(char* error) {
  expval x = { .variant = ERRORVAL, .error = error };
  return x;
}

bool is_numval(expval val) {
  return val.variant == NUMVAL;
}

//****************************
bool is_procval(expval val) {
  return val.variant == PROCVAL;
}
//****************************

bool is_boolval(expval val) {
  return val.variant == BOOLVAL;
}

bool is_errorval(expval val) {
  return val.variant == ERRORVAL;
}

int expval_number(expval val) {
  return val.number;
}

expression expval_proc(expval val) {
  return val.expr;
}

bool expval_truth(expval val) {
  return val.truth;
}

const char* expval_error(expval val) {
  return val.error;
}

void print_expval(expval val) {
  if (is_numval(val)) {
    printf("%d", expval_number(val));
  } else if (is_boolval(val)) {
    printf(expval_truth(val) ? "true" : "false");
  } else if (is_errorval(val)) {
    printf("ERROR: %s", expval_error(val));
  } else if (is_procval(val)) {
    print_ast(stdout, proc_body(val.expr));
  } else {
    printf("FATAL: No se reconoce valor expresado");
  }
}

/* -----------------------------Entorno-------------------------------- */

struct environment_t {
  identifier id;
  identifier name;
  expval val;
  struct environment_t* parent;
};

typedef struct environment_t* environment;

struct environment_t envs[envs_size];
const environment env_bound = envs + envs_size;
environment next_env = envs;


environment extend_env(identifier id, expval val, environment env) {
  if (next_env >= env_bound) return NULL;
  next_env->id = id;
  next_env->val = val;
  next_env->parent = env;
  return next_env++;
}
environment extend_env_rec(identifier name, identifier id, expval val, environment env) {
  if (next_env >= env_bound) return NULL;
  next_env->name = name;
  next_env->id = id;
  next_env->val = make_procval(val.expr);
  next_env->parent = env;
  return next_env++;
}

expval apply_env(environment env, identifier id) {
  while (env != NULL) {
    if (env->id == id) return env->val;
    env = env->parent;
  }
  return make_errorval("Variable libre");
}
/* ------------------------------------------------------------------- */

/*------------------------ Value-of----------------------------------- */

expval value_of(expression e, environment env) {
  if (e == NULL) {
    return make_errorval("Expresion no valida");
  } else if (is_const(e)) {
    /* Valor de constantes numéricas */
    return make_numval(const_value(e));

  } else if (is_diff(e)) {
    /* Valor de restas */
    expval left = value_of(diff_left(e), env);
    if (is_errorval(left)) return left;

    expval right = value_of(diff_right(e), env);
    if (is_errorval(right)) return right;

    if (!is_numval(left) || !is_numval(right))
      return make_errorval("Subexpresion en resta no es numerica");

    return make_numval(expval_number(left) - expval_number(right));

  } else if (is_zerop(e)) {
    /* Valor de predicado zero? */
    expval arg = value_of(zerop_arg(e), env);
    if (is_errorval(arg)) return arg;
    if (!is_numval(arg)) return make_errorval("Subexpresion en zero? no es numerica");

    return make_boolval(expval_number(arg) == 0);

  } else if (is_if(e)) {
    /* Valor de condicionales if */
    expval condi = value_of(if_condi(e), env);
    if (is_errorval(condi)) return condi;
    if (!is_boolval(condi)) return make_errorval("Condicional en if no es booleana");

    if (expval_truth(condi))
      return value_of(if_thenb(e), env);
    else
      return value_of(if_elseb(e), env);

  } else if (is_var(e)) {
    /* Valor de variables */
    return apply_env(env, var_id(e));

  } else if (is_let(e)) {
    /* Valor de expresiones let */
    expval val = value_of(let_val(e), env);
    if (is_errorval(val)) return val;

    environment new_env = extend_env(let_id(e), val, env);
    if (new_env == NULL) return make_errorval("Ya no hay espacio para entornos");

    return value_of(let_body(e), new_env);

  }else if (is_letrec(e)) {

    expval val = value_of(letrec_body(e), env);
    if (is_errorval(val)) return val;
    environment new_env = extend_env(letrec_id(e), val, env);
    struct expression_t* proc = make_proc(letrec_name(e),letrec_val(e));

    return apply_env(new_env,letrec_id(e));
    //return make_procval(proc);

    //letrec double(x) = if zero?(x) then 0 else -((double -(x, 1)), -2) in (double 6)


  }else if (is_proc(e)) {

    return make_procval(e);

  }else if (is_cexpr(e)) {
    /* CALL_EXP*/

    environment new_env = extend_env(proc_id(cexpr_e1(e)), value_of(proc_body(cexpr_e1(e)),env), env);
    if (new_env == NULL) return make_errorval("Ya no hay espacio para entornos");

    return value_of(cexpr_e2(e), new_env);

// (proc(x1) -(1,2) -(x1,2))
  } else {
    return make_errorval("Expresion no válida");
  }
}
/* ------------------------------------------------------------------- */
/*************************/
/* INTERFAZ DE EJECUCIÓN */
/*************************/

int run_expr(FILE* in) {
  expression e = parse_ast(in);
  if (e == NULL) return 1;

  printf("\nSe leyo: ");
  print_ast(stdout, e);

  printf("\n\nSu valor es: ");
  print_expval(value_of(e, NULL));
  printf("\n");

  return 0;
}

int repl(FILE* in) {
  while (true) {
    printf("\n> ");
    run_expr(in);
    fflush(in);
  }

  return 0;
}
