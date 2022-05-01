/**
 * @file op.cpp
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Implementation of supported primitive operations.
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#include "op.h"

#include <algorithm>
#include <cassert>
#include <iostream>

#include "compiled-proc.h"
#include "env.h"
#include "primitive-procs.h"

typedef std::vector<value_t> arg_list_t;

op_t* str_to_op(std::string_view op_str) {
  if (op_str == "+") {
    return add_op;
  } else if (op_str == "-") {
    return sub_op;
  } else if (op_str == "=") {
    return int_eq_op;
  } else if (op_str == "true?") {
    return true_test_op;
  } else if (op_str == "false?") {
    return false_test_op;
  } else if (op_str == "cons") {
    return cons_op;
  } else if (op_str == "car") {
    return car_op;
  } else if (op_str == "cdr") {
    return cdr_op;
  } else if (op_str == "list") {
    return list_op;
  } else if (op_str == "primitive-procedure?") {
    return primitive_procedure_test_op;
  } else if (op_str == "apply-primitive-procedure") {
    return apply_primitive_procedure_op;
  } else if (op_str == "make-compiled-procedure") {
    return make_compiled_procedure_op;
  } else if (op_str == "compiled-procedure-env") {
    return compiled_procedure_env_op;
  } else if (op_str == "compiled-procedure-entry") {
    return compiled_procedure_entry_op;
  } else if (op_str == "extend-environment") {
    return extend_environment_op;
  } else if (op_str == "lookup-variable-value") {
    return lookup_variable_value_op;
  } else if (op_str == "set-variable-value!") {
    return set_variable_op;
  } else if (op_str == "define-variable!") {
    return define_variable_op;
  }

  throw std::runtime_error(
      std::string("unknown primitive operation ").append(op_str));
}

struct add_op_t : op_t {
  value_t execute(const arg_list_t& args) override {
    int64_t result = 0;
    for (auto& arg : args) {
      result += arg.as<int64_t>();
    }
    return result;
  }
};

op_t* add_op = new add_op_t;

struct sub_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() >= 1);
    int64_t result = args.at(0).as<int64_t>();
    for (auto it = args.begin() + 1; it != args.end(); ++it) {
      result -= (*it).as<int64_t>();
    }
    return result;
  }
};

op_t* sub_op = new sub_op_t;

struct int_eq_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 2);
    bool equal = args.at(0).as<int64_t>() == args.at(1).as<int64_t>();
    return equal;
  }
};

op_t* int_eq_op = new int_eq_op_t;

struct pair_test_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 1);
    return args.at(0).has<pair_t*>();
  }
};

op_t* pair_test_op = new pair_test_op_t;

struct true_test_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 1);
    return !args.at(0).has<bool>() || args.at(0).as<bool>();
  }
};

op_t* true_test_op = new true_test_op_t;

struct false_test_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 1);
    return args.at(0).has<bool>() && !args.at(0).as<bool>();
  }
};

op_t* false_test_op = new false_test_op_t;

struct cons_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 2);
    value_t car = args.at(0);
    value_t cdr = args.at(1);
    return new pair_t(std::move(car), std::move(cdr));
  }
};

op_t* cons_op = new cons_op_t;

struct car_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 1);
    return args.at(0).as<pair_t*>()->car;
  }
};

op_t* car_op = new car_op_t;

struct cdr_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 1);
    return args.at(0).as<pair_t*>()->cdr;
  }
};

op_t* cdr_op = new cdr_op_t;

struct list_op_t : op_t {
  value_t execute(const arg_list_t& args) { return pair_t::make_list(args); }
};

op_t* list_op = new list_op_t;

struct primitive_procedure_test_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 1);
    return args.at(0).has<primitive_procedure_t*>();
  }
};

op_t* primitive_procedure_test_op = new primitive_procedure_test_op_t;

struct apply_primitive_procedure_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 2);
    return args.at(0).as<primitive_procedure_t*>()->execute(args.at(1));
  }
};

op_t* apply_primitive_procedure_op = new apply_primitive_procedure_op_t;

struct make_compiled_procedure_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 2);
    label_t entry = args.at(0).as<label_t>();
    env_t* env = args.at(1).as<env_t*>();
    return new compiled_procedure_t(entry, env);
  }
};

op_t* make_compiled_procedure_op = new make_compiled_procedure_op_t;

struct compiled_procedure_env_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 1);
    auto proc = args.at(0).as<compiled_procedure_t*>();
    return proc->env;
  }
};

op_t* compiled_procedure_env_op = new compiled_procedure_env_op_t;

struct compiled_procedure_entry_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 1);
    auto proc = args.at(0).as<compiled_procedure_t*>();
    return proc->entry;
  }
};

op_t* compiled_procedure_entry_op = new compiled_procedure_entry_op_t;

struct extend_environment_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 3);
    const value_t& names = args.at(0);
    env_t* env = args.at(2).as<env_t*>();
    return env->extend_environment(names, args.at(1));
  }
};

op_t* extend_environment_op = new extend_environment_op_t;

struct lookup_variable_value_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 2);
    quoted_t name = args.at(0).as<quoted_t>();
    env_t* env = args.at(1).as<env_t*>();
    return env->lookup_var_value(name);
  }
};

op_t* lookup_variable_value_op = new lookup_variable_value_op_t;

struct define_variable_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 3);
    quoted_t name = args.at(0).as<quoted_t>();
    value_t val = args.at(1);
    env_t* env = args.at(2).as<env_t*>();
    env->define_global_variable(name, val);
    return unassigned_t();
  }
};

op_t* define_variable_op = new define_variable_op_t;

struct set_variable_op_t : op_t {
  value_t execute(const arg_list_t& args) {
    assert(args.size() == 3);
    quoted_t name = args.at(0).as<quoted_t>();
    value_t val = args.at(1);
    env_t* env = args.at(2).as<env_t*>();
    env->set_variable(name, val);
    return unassigned_t();
  }
};

op_t* set_variable_op = new set_variable_op_t;