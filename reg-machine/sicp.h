/**
 * @file sicp.h
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief SICP specific register machine setup.
 * @version 0.1
 * @date 2022-04-25
 *
 * @copyright Copyright (c) 2022
 *
 */
#pragma once

#include <map>

#include "env.h"
#include "primitive-procs.h"
#include "types.h"

std::map<std::string, value_t> sicp_compiler_registers() {
  env_t* env = new env_t;
  env->define_variable(quoted_t{"="}, int_eq_primitive_proc);
  env->define_variable(quoted_t{"+"}, add_primitive_proc);
  env->define_variable(quoted_t{"-"}, sub_primitive_proc);
  env->define_variable(quoted_t{"*"}, mul_primitive_proc);

  std::map<std::string, value_t> regs;
  regs.insert({"env", env});
  regs.insert({"proc", unassigned_t()});
  regs.insert({"argl", unassigned_t()});
  regs.insert({"val", unassigned_t()});
  regs.insert({"continue", unassigned_t()});

  return regs;
}
