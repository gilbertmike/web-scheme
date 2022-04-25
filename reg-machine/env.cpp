/**
 * @file env.cpp
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Implementation of environment primitive type.
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#include "env.h"

#include <cassert>
#include <string>

env_t::env_t() : mapping() {}

env_t::env_t(const env_t& other) : mapping(other.mapping) {}

env_t* env_t::extend_environment(const value_t& names, pair_t* values) {
  env_t* extended_env = new env_t(*this);
  if (!names.has<pair_t*>()) [[unlikely]] {
    assert(names.has<quoted_t>());
    extended_env->define_variable(names.as<quoted_t>(), values);
  } else {
    pair_t* list = names.as<pair_t*>();
    while (true) {
      extended_env->define_variable(list->car.as<quoted_t>(), values->car);
      if (list->cdr.has<pair_t*>()) {
        list = list->cdr.as<pair_t*>();
        values = values->cdr.as<pair_t*>();
      } else {
        if (list->cdr.has<quoted_t>()) [[unlikely]] {
          extended_env->define_variable(list->cdr.as<quoted_t>(), values->cdr);
        }
        break;
      }
    }
  }
  return extended_env;
}

void env_t::define_variable(const quoted_t& varname, const value_t& value) {
  mapping.insert_or_assign(varname.value, value);
}

value_t env_t::lookup_var_value(const quoted_t& varname) {
  return mapping.at(varname.value);
}

void env_t::mark_children() {
  for (auto& [key, value] : mapping) {
    value.mark_children();
  }
}
