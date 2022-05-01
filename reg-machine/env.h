/**
 * @file env.h
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Environment primitive type.
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#pragma once

#include "types.h"

class env_t : public garbage_collected_t {
 public:
  env_t();

  env_t* extend_environment(const value_t& names, const value_t& values);

  void define_variable(const quoted_t& varname, const value_t& value);
  void define_global_variable(const quoted_t& varname, const value_t& value);
  void set_variable(const quoted_t& varname, const value_t& value);

  value_t lookup_var_value(const quoted_t& varname);

  void mark_children() override;

 private:
  std::unordered_map<int, value_t> mapping;
  env_t* parent;
};
