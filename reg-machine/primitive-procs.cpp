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

#include <assert.h>

#include "op.h"

value_t add_procedure_t::execute(const value_t& args) {
  assert(pair_test_op->execute({args}).as<bool>());

  int64_t result = 0;
  pair_t* cur = args.as<pair_t*>();
  for (; cur->cdr.has<pair_t*>(); cur = cur->cdr.as<pair_t*>()) {
    result += cur->car.as<int64_t>();
  }
  result += cur->car.as<int64_t>();

  return result;
}

value_t sub_procedure_t::execute(const value_t& args) {
  assert(pair_test_op->execute({args}).as<bool>());

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

value_t mul_procedure_t::execute(const value_t& args) {
  assert(pair_test_op->execute({args}).as<bool>());

  int64_t result = 1;
  pair_t* cur = args.as<pair_t*>();
  for (; cur->cdr.has<pair_t*>(); cur = cur->cdr.as<pair_t*>()) {
    result *= cur->car.as<int64_t>();
  }
  result *= cur->car.as<int64_t>();

  return result;
}
