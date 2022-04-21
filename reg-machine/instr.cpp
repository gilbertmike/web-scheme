#include "instr.h"

#include <algorithm>
#include <vector>

#include "machine.h"

std::vector<value_t> get_arg_values(
    const std::vector<std::variant<reg_t*, value_t>>&);

void assign_reg_instr_t::execute(machine_t& machine) { dst->set(src->get()); }

void assign_const_instr_t::execute(machine_t& machine) { dst->set(src); }

void assign_op_instr_t::execute(machine_t& machine) {
  auto args = get_arg_values(this->args);
  dst->set(op->execute(args));
}

void assign_label_instr_t::execute(machine_t& machine) {
  dst->set(new object_t(label));
}

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
  if (machine.flag.get()->as<bool>()) {
    machine.pc.set(new object_t(dst));
  }
}

void goto_label_instr_t::execute(machine_t& machine) {
  machine.pc.set(new object_t(dst));
}

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
                           return arg;
                         } else {
                           __builtin_unreachable();
                         }
                       },
                       arg);

                   return value;
                 });

  return value_args;
}