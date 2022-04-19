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
