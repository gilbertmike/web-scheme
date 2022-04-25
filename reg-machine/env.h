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

class env_t : garbage_collected_t {
 public:
  env_t();
  env_t(const env_t& other);

  env_t* extend_environment(const value_t& names, pair_t* values);

  void define_variable(const quoted_t& varname, const value_t& value);

  value_t lookup_var_value(const quoted_t& varname);

  void mark_children();

 private:
  std::map<std::string, value_t> mapping;
};
