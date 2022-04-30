/**
 * @file primitive-procs.h
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Primitive procedure (primitive) type.
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#pragma once

struct value_t;

struct primitive_procedure_t {
  virtual value_t execute(const value_t& args) = 0;
};

extern primitive_procedure_t* add_primitive_proc;
extern primitive_procedure_t* sub_primitive_proc;
extern primitive_procedure_t* mul_primitive_proc;
extern primitive_procedure_t* int_eq_primitive_proc;

extern primitive_procedure_t* not_primitive_proc;

extern primitive_procedure_t* cons_primitive_proc;
extern primitive_procedure_t* car_primitive_proc;
extern primitive_procedure_t* cdr_primitive_proc;

extern primitive_procedure_t* eq_test_primitive_proc;
extern primitive_procedure_t* eqv_test_primitive_proc;

extern primitive_procedure_t* number_test_primitive_proc;
extern primitive_procedure_t* pair_test_primitive_proc;
extern primitive_procedure_t* bool_test_primitive_proc;
extern primitive_procedure_t* string_test_primitive_proc;
extern primitive_procedure_t* symbol_test_primitive_proc;
extern primitive_procedure_t* null_test_primitive_proc;

extern primitive_procedure_t* print_primitive_proc;
extern primitive_procedure_t* input_primitive_proc;

extern primitive_procedure_t* gensym_primitive_proc;

extern primitive_procedure_t* error_primitive_proc;
extern primitive_procedure_t* apply_primitive_proc;
