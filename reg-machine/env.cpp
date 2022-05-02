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
#include <iostream>
#include <string>

env_t::env_t() : mapping(), parent(nullptr) {}

env_t* env_t::extend_environment(const value_t& names, const value_t& values) {
  env_t* extended_env = new env_t;
  extended_env->parent = this;
  if (values.has<unassigned_t>() && names.has<unassigned_t>()) {
    return extended_env;
  }
  if (!names.has<pair_t*>()) [[unlikely]] {
    assert(names.has<quoted_t>());
    extended_env->define_variable(names.as<quoted_t>(), values);
  } else {
    pair_t* rest_names = names.as<pair_t*>();
    assert(values.has<pair_t*>());
    pair_t* rest_values = values.as<pair_t*>();
    while (true) {
      extended_env->define_variable(rest_names->car.as<quoted_t>(),
                                    rest_values->car);
      if (rest_names->cdr.has<pair_t*>()) {
        rest_names = rest_names->cdr.as<pair_t*>();
        rest_values = rest_values->cdr.as<pair_t*>();
      } else {
        if (rest_names->cdr.has<quoted_t>()) [[unlikely]] {
          extended_env->define_variable(rest_names->cdr.as<quoted_t>(),
                                        rest_values->cdr);
        } else {
          assert(rest_names->cdr.has<unassigned_t>());
          assert(rest_values->cdr.has<unassigned_t>());
        }
        break;
      }
    }
  }
  return extended_env;
}

void env_t::define_variable(const quoted_t& varname, const value_t& value) {
  mapping.insert_or_assign(varname.id, value);
}

void env_t::define_global_variable(const quoted_t& varname, const value_t& value) {
  env_t* root = this;
  while (root->parent) root = root->parent;
  root->define_variable(varname, value);
}

void env_t::set_variable(const quoted_t& varname, const value_t& value) {
  env_t* cur = this;
  while (cur) {
    if (auto it = cur->mapping.find(varname.id); it != cur->mapping.end()) {
      it->second = value;
      return;
    }
    cur = cur->parent;
  }
  std::cerr << "Unbound variable: " << varname.value << std::endl;
  throw std::runtime_error("unbound variable");
}

value_t env_t::lookup_var_value(const quoted_t& varname) {
  env_t* cur = this;
  while (cur) {
    if (auto it = cur->mapping.find(varname.id); it != cur->mapping.end()) {
      return it->second;
    }
    cur = cur->parent;
  }
  std::cerr << "Unbound variable: " << varname.value << std::endl;
  throw std::runtime_error("unbound variable");
}

void env_t::mark_children() {
  for (auto& [key, value] : mapping) value.mark();
  if (parent) parent->mark();
}
