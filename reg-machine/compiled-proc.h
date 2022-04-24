/**
 * @file compiled-proc.h
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Compiled procedure primitive.
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#pragma once

#include "env.h"
#include "types.h"

struct compiled_procedure_t {
  env_t* env;
  label_t entry;

  compiled_procedure_t(const label_t& entry, env_t* env);
};
