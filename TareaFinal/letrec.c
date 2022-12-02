/* -*- mode: c; coding: utf-8; make-target: compile; -*-
 *
 */

#include <stdio.h>

#include <ast.h>
#include <driver.h>

int main(int argc, char** argv) {
  ++argv;
  --argc;
  
  FILE* in;

  printf("Intérprete LETREC\n");
  
  if (argc > 0) {
    in = fopen(argv[0], "r");
    return run_expr(in);
  }
  in = stdin;
  return repl(in);
}
