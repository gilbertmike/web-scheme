#include "instr.h"

#include <algorithm>
#include <iostream>
#include <vector>

#include "machine.h"

std::vector<value_t> get_arg_values(
    const std::vector<std::variant<reg_t*, value_t>>&);

void assign_reg_instr_t::execute(machine_t& machine) { dst->set(src->get()); }

void assign_const_instr_t::execute(machine_t& machine) { dst->set(src); }

void assign_op_instr_t::execute(machine_t& machine) {
  auto args = get_arg_values(this->args);
  auto result = op->execute(args);
  // std::cout << "Assigned " << dst->get_name() << ": " << result << std::endl;
  dst->set(result);
}

void assign_label_instr_t::execute(machine_t& machine) { dst->set(label); }

void perform_instr_t::execute(machine_t& machine) {
  auto args = get_arg_values(this->args);
  op->execute(args);
}

void test_instr_t::execute(machine_t& machine) {
  auto args = get_arg_values(this->args);
  auto res = op->execute(args);
  machine.flag.set(res);
}

void branch_instr_t::execute(machine_t& machine) {
  if (machine.flag.get().as<bool>()) {
    machine.pc.set(dst);
  }
}

void goto_label_instr_t::execute(machine_t& machine) { machine.pc.set(dst); }

void goto_reg_instr_t::execute(machine_t& machine) {
  machine.pc.set(dst->get());
}

void save_instr_t::execute(machine_t& machine) {
  machine.stack.push(src->get());
}

void restore_instr_t::execute(machine_t& machine) {
  dst->set(machine.stack.pop());
}

template <class>
inline constexpr bool always_false_v = false;

// We need to clone a list so that the GC can track it.
// The const values in the assembly are nice, but they are "static values" and
// managed by the global GC (essentially an arena allocator). If they are used
// directly, while there is no problem with marking, the machine GC will not
// unmark them at the end, so at the next GC some managed objects down the line
// (this can achived with set-cdr!) may not be marked and incorrectly freed,
// leading to all sorts of nasty bugs.

// Alternatively, of course we can let GC track all list constants and make
// them persistent root elements, but then the root set's size is somehow
// proportional to the program size, which doesn't feel right anyway.
value_t clone_value(value_t in) {
  if (in.has<pair_t*>()) {
    pair_t* pair = in.as<pair_t*>();
    return new pair_t{clone_value(pair->car), clone_value(pair->cdr)};
  }
  return in;
}

std::vector<value_t> get_arg_values(
    const std::vector<std::variant<reg_t*, value_t>>& args) {
  std::vector<value_t> value_args;
  value_args.reserve(args.size());

  std::transform(args.begin(), args.end(), std::back_inserter(value_args),
                 [](auto&& arg) -> value_t {
                   value_t value = std::visit(
                       [](auto&& arg) -> value_t {
                         using T = std::decay_t<decltype(arg)>;
                         if constexpr (std::is_same_v<T, reg_t*>) {
                           return arg->get();
                         } else if constexpr (std::is_same_v<T, value_t>) {
                           return clone_value(arg);
                         } else {
                           __builtin_unreachable();
                         }
                       },
                       arg);

                   return value;
                 });

  return value_args;
}