#include <cstdint>
#include <stack>
#include <vector>

#include "instr.h"
#include "types.h"

class reg_t {
 public:
  reg_t(value_t value) : value(value) {}

  void set(value_t new_value) { value = new_value; }

  value_t get() const { return value; }

 private:
  value_t value;
};

class stack_t {
 public:
  stack_t();

  void push(value_t value) { stack.emplace(value); }

  value_t pop() {
    auto top = stack.top();
    stack.pop();
    return top;
  }

 private:
  std::stack<value_t> stack;
};

struct machine_t {
  machine_t(int rfile_size, const std::vector<instr_t>& instructions)
      : pc(value_t(0)),
        flag(value_t(false)),
        instructions(instructions),
        rfile(rfile_size) {}

  void start_execution() { pc.get(); }

  reg_t pc;
  reg_t flag;
  stack_t stack;
  std::vector<instr_t> instructions;

  std::vector<reg_t> rfile;
};
