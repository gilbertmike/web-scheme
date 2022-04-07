#include <cstdint>
#include <stack>
#include <vector>

#include "instr.h"

enum valuetype_t : uint8_t { INTEGER, BOOL, PAIR, EMPTY };

/**
 * A register value. For example, this could be a number or a typed pointer.
 **/
class value_t {
 public:
  value_t(uintptr_t value, valuetype_t type) : value(value), type(type) {}
  value_t(int value) : value_t((uintptr_t)value, valuetype_t::INTEGER) {}
  value_t(bool value) : value_t((uintptr_t)value, valuetype_t::BOOL) {}

  valuetype_t get_type() const { return type; }

 private:
  uintptr_t value;
  valuetype_t type;
};

class reg_t {
 public:
  reg_t(const value_t& value) : value(value) {}

  void set(const value_t& new_value) { value = new_value; }

  const value_t& get() const { return value; }

 private:
  value_t value;
};

class stack_t {
 public:
  stack_t();

  void push(const value_t& value) { stack.emplace(value); }

  value_t pop() {
    auto top = stack.top();
    stack.pop();
    return top;
  }

 private:
  std::stack<value_t> stack;
};

class machine_t {
 public:
  machine_t(const std::vector<instr_t>& instructions)
      : pc(value_t(0)), flag(value_t(false)), instructions(instructions) {}

 private:
  reg_t pc;
  reg_t flag;
  stack_t stack;
  std::vector<instr_t> instructions;
};
