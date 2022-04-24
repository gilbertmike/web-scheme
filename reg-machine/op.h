#pragma once

#include <vector>

#include "types.h"

struct op_t {
  virtual value_t execute(const std::vector<value_t>& args) = 0;
};

op_t* str_to_op(std::string_view op_str);

extern op_t* add_op;
extern op_t* sub_op;
extern op_t* int_eq_op;

extern op_t* cons_op;
extern op_t* car_op;
extern op_t* cdr_op;

extern op_t* primitive_procedure_test_op;   // TODO
extern op_t* apply_primitive_procedure_op;  // TODO

extern op_t* make_compiled_procedure_op;  // TODO
extern op_t* compiled_procedure_env_op;   // TODO

extern op_t* extend_environment_op;        // TODO
extern op_t* lookup_variable_value_op;     // TODO
extern op_t* compiled_procedure_entry_op;  // TODO
extern op_t* define_variable_op;           // TODO
