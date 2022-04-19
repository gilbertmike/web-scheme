#include "machine.h"

reg_t::reg_t() : value(unassgined) {}

reg_t::reg_t(value_t value) : value(value) {}

void reg_t::set(value_t new_value) { value = new_value; }

value_t reg_t::get() const { return value; }

stack_t::stack_t() {}

void stack_t::push(value_t value) { stack.emplace(value); }

value_t stack_t::pop() {
  auto top = stack.top();
  stack.pop();
  return top;
}

machine_t::machine_t(int rfile_size)
    : pc(value_t(0)), flag(value_t(false)), instructions(), rfile(rfile_size) {}

machine_t::machine_t(int rfile_size, std::vector<instr_t::u_ptr>&& instructions)
    : pc(value_t(0)),
      flag(value_t(false)),
      instructions(std::move(instructions)),
      rfile(rfile_size) {}

void machine_t::start() { pc.get(); }
