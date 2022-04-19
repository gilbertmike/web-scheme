#include "op.h"

#include <algorithm>

typedef std::vector<value_t> arg_list_t;

op_t* str_to_op(std::string_view op_str) {
  if (op_str == "+") {
    return add_op;
  } else if (op_str == "-") {
    return sub_op;
  } else if (op_str == "=") {
    return int_eq_op;
  } else if (op_str == "cons") {
    return cons_op;
  } else if (op_str == "car") {
    return car_op;
  } else if (op_str == "cdr") {
    return cdr_op;
  }

  throw std::runtime_error("unknown primitive operation");
}

struct add_op_t : op_t {
  value_t execute(const arg_list_t& args) {}
};

op_t* add_op = new add_op_t;

struct sub_op_t : op_t {
  value_t execute(const arg_list_t& args) {}
};

op_t* sub_op = new sub_op_t;

struct int_eq_op_t : op_t {
  value_t execute(const arg_list_t& args) {}
};

op_t* int_eq_op = new int_eq_op_t;

struct cons_op_t : op_t {
  value_t execute(const arg_list_t& args) {}
};

op_t* cons_op = new cons_op_t;

struct car_op_t : op_t {
  value_t execute(const arg_list_t& args) {}
};

op_t* car_op = new car_op_t;

struct cdr_op_t : op_t {
  value_t execute(const arg_list_t& args) {}
};

op_t* cdr_op = new cdr_op_t;
