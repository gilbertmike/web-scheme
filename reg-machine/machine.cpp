#include "machine.h"

#include "types.h"

reg_t::reg_t() : value(unassigned_t()) {}

reg_t::reg_t(const value_t& value) : value(value) {}

void reg_t::set(const value_t& new_value) { value = new_value; }

value_t reg_t::get() const { return value; }

void reg_t::set_name(const std::string& name) { this->name = name; }

std::string_view reg_t::get_name() const { return name; }

stack_t::stack_t() {}

void stack_t::push(const value_t& value) { stack.emplace(value); }

value_t stack_t::pop() {
  auto top = stack.top();
  stack.pop();
  return top;
}

machine_t::machine_t(int rfile_size)
    : pc(label_t{0}), flag(false), instructions(), rfile(rfile_size) {}

machine_t::machine_t(int rfile_size, std::vector<instr_t::u_ptr>&& instructions)
    : pc(label_t{0}),
      flag(false),
      instructions(std::move(instructions)),
      rfile(rfile_size) {}

void machine_t::start() {
  while (true) {
    uintptr_t cur_instr_idx = pc.get().as<label_t>().dst;
    if (cur_instr_idx >= instructions.size()) {
      break;
    }

    pc.set(label_t{cur_instr_idx + 1});

    instructions.at(cur_instr_idx)->execute(*this);
  }
}
