/* -*- mode: c; coding: utf-8; make-target: compile; -*-
 * 
 */

#ifndef __LETREC_CONFIG_H__
#define __LETREC_CONFIG_H__

/* Tamaño para el buffer de nombres (cantidad máxima de caracteres) */
#define buffer_size 1048576	/* 2^20 */

/* Tamaño para el buffer de símbolos (cantidad máxima de identificadores) */
#define table_size 4096		/* 2^12 */

/* Tamaño para el buffer de expresiones (cantidad máxima de expresiones) */
#define exprs_size 1024		/* 2^10 */

/* Tamaño para el buffer de entornos (cantidad máxima de vinculaciones) */
#define envs_size 256		/* 2^8 */

#endif	/* __LETREC_CONFIG_H__ */
