/**
 * @file primitive-procs.cpp
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Implementation of primitive procedures in machine ops.
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#include "primitive-procs.h"

#include <cassert>
#include <sstream>

#include "machine.h"
#include "op.h"
#include "parser.h"

struct add_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());

    int64_t result = 0;
    pair_t* cur = args.as<pair_t*>();
    for (; cur->cdr.has<pair_t*>(); cur = cur->cdr.as<pair_t*>()) {
      result += cur->car.as<int64_t>();
    }
    result += cur->car.as<int64_t>();

    return result;
  }
};

primitive_procedure_t* add_primitive_proc = new add_procedure_t;

struct sub_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());

    pair_t* cur = args.as<pair_t*>();
    int64_t result = cur->car.as<int64_t>();
    if (cur->cdr.has<unassigned_t>()) {
      return -result;
    } else if (cur->cdr.has<pair_t*>()) {
      cur = cur->cdr.as<pair_t*>();
      for (; cur->cdr.has<pair_t*>(); cur = cur->cdr.as<pair_t*>()) {
        result -= cur->car.as<int64_t>();
      }
      result -= cur->car.as<int64_t>();
    }

    return result;
  }
};

primitive_procedure_t* sub_primitive_proc = new sub_procedure_t;

struct mul_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());

    int64_t result = 1;
    pair_t* cur = args.as<pair_t*>();
    for (; cur->cdr.has<pair_t*>(); cur = cur->cdr.as<pair_t*>()) {
      result *= cur->car.as<int64_t>();
    }
    result *= cur->car.as<int64_t>();

    return result;
  }
};

primitive_procedure_t* mul_primitive_proc = new mul_procedure_t;

struct int_eq_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());
    pair_t* args_pair = args.as<pair_t*>();
    assert(args_pair->cdr.has<pair_t*>());
    return args_pair->car.as<int64_t>() ==
           args_pair->cdr.as<pair_t*>()->car.as<int64_t>();
  }
};

primitive_procedure_t* int_eq_primitive_proc = new int_eq_procedure_t;

template <typename T>
struct type_test_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());
    pair_t* args_pair = args.as<pair_t*>();
    assert(args_pair->cdr.has<unassigned_t>());
    return args_pair->car.has<T>();
  }
};

primitive_procedure_t* number_test_primitive_proc =
    new type_test_procedure_t<int64_t>();
primitive_procedure_t* pair_test_primitive_proc =
    new type_test_procedure_t<pair_t*>();
primitive_procedure_t* bool_test_primitive_proc =
    new type_test_procedure_t<bool>();
primitive_procedure_t* string_test_primitive_proc =
    new type_test_procedure_t<string_t>();
primitive_procedure_t* symbol_test_primitive_proc =
    new type_test_procedure_t<quoted_t>();
primitive_procedure_t* null_test_primitive_proc =
    new type_test_procedure_t<unassigned_t>();

struct print_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());
    pair_t* args_pair = args.as<pair_t*>();
    std::ostream& output = *machine_t::current().output;
    output << args_pair->car;
    bool endl = true;
    if (args_pair->cdr.has<pair_t*>()) {
      args_pair = args_pair->cdr.as<pair_t*>();
      if (args_pair->car.has<bool>()) endl = args_pair->car.as<bool>();
    }
    if (endl) output << std::endl;
    return unassigned_t();
  }
};

primitive_procedure_t* print_primitive_proc = new print_procedure_t;

struct input_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<unassigned_t>());
    const auto& callback = machine_t::current().input;
    if (callback == nullptr) {
      throw std::runtime_error("no input callback specified");
    }
    std::istringstream input_raw{callback()};
    token_list_t tokens = tokenize(input_raw);
    token_list_t::iterator start = tokens.begin();
    return parse_object(start);
  }
};

primitive_procedure_t* input_primitive_proc = new input_procedure_t;

inline const value_t& one_arg(const value_t& args) {
  assert(args.has<pair_t*>());
  const pair_t* first_pair = args.as<pair_t*>();
  assert(first_pair->cdr.has<unassigned_t>());
  return first_pair->car;
}

inline std::pair<const value_t&, const value_t&> two_args(const value_t& args) {
  assert(args.has<pair_t*>());
  const pair_t* first_pair = args.as<pair_t*>();
  assert(first_pair->cdr.has<pair_t*>());
  const pair_t* second_pair = first_pair->cdr.as<pair_t*>();
  assert(second_pair->cdr.has<unassigned_t>());
  return {first_pair->car, second_pair->car};
}

struct car_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    const value_t& pair = one_arg(args);
    assert(pair.has<pair_t*>());
    return pair.as<pair_t*>()->car;
  }
};

primitive_procedure_t* car_primitive_proc = new car_procedure_t;

struct cdr_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    const value_t& pair = one_arg(args);
    assert(pair.has<pair_t*>());
    return pair.as<pair_t*>()->cdr;
  }
};

primitive_procedure_t* cdr_primitive_proc = new cdr_procedure_t;

struct cons_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    const auto& [car, cdr] = two_args(args);
    return new pair_t(value_t{car}, value_t{cdr});
  }
};

primitive_procedure_t* cons_primitive_proc = new cons_procedure_t;