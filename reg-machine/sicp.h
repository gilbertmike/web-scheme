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

  env->define_variable(quoted_t{"not"}, not_primitive_proc);

  env->define_variable(quoted_t{"eq?"}, eq_test_primitive_proc);
  env->define_variable(quoted_t{"eqv?"}, eqv_test_primitive_proc);

  env->define_variable(quoted_t{"car"}, car_primitive_proc);
  env->define_variable(quoted_t{"cdr"}, cdr_primitive_proc);
  env->define_variable(quoted_t{"cons"}, cons_primitive_proc);

  env->define_variable(quoted_t{"number?"}, number_test_primitive_proc);
  env->define_variable(quoted_t{"pair?"}, pair_test_primitive_proc);
  env->define_variable(quoted_t{"boolean?"}, bool_test_primitive_proc);
  env->define_variable(quoted_t{"string?"}, string_test_primitive_proc);
  env->define_variable(quoted_t{"symbol?"}, symbol_test_primitive_proc);
  env->define_variable(quoted_t{"null?"}, null_test_primitive_proc);

  env->define_variable(quoted_t{"error"}, error_primitive_proc);
  env->define_variable(quoted_t{"apply"}, apply_primitive_proc);
  // For internal use by CPS programs only.
  env->define_variable(quoted_t{"%halt"}, halt_primitive_proc);

  env->define_variable(quoted_t{"gensym"}, gensym_primitive_proc);
  env->define_variable(quoted_t{"generate-uninterned-symbol"}, gensym_primitive_proc);

  env->define_variable(quoted_t{"input"}, input_primitive_proc);
  env->define_variable(quoted_t{"display"}, print_primitive_proc);
  env->define_variable(quoted_t{"write-line"}, print_primitive_proc);
  env->define_variable(quoted_t{"print"}, print_primitive_proc);
  env->define_variable(quoted_t{"pp"}, print_primitive_proc);

  std::map<std::string, value_t> regs;
  regs.insert({"env", env});
  regs.insert({"proc", unassigned_t()});
  regs.insert({"argl", unassigned_t()});
  regs.insert({"val", unassigned_t()});
  regs.insert({"continue", unassigned_t()});

  return regs;
}
