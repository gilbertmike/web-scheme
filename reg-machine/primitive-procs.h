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
