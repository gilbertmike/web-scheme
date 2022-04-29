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

#include "machine.h"
#include "op.h"

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
    if (cur->cdr.has<int64_t>()) {
      return result - cur->cdr.as<int64_t>();
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