#pragma once

#include <cstdint>
#include <stack>
#include <vector>

#include "instr.h"
#include "types.h"

class reg_t {
 public:
  reg_t();
  reg_t(value_t value);

  void set(value_t new_value);

  value_t get() const;

 private:
  value_t value;
};

class stack_t {
 public:
  stack_t();

  void push(value_t value);

  value_t pop();

 private:
  std::stack<value_t> stack;
};

struct machine_t {
  machine_t(int rfile_size);
  machine_t(int rfile_size, std::vector<instr_t::u_ptr>&& instructions);

  static machine_t construct_from_string(std::string_view text);

  void start();

  reg_t pc;
  reg_t flag;
  stack_t stack;
  std::vector<instr_t::u_ptr> instructions;

  std::vector<reg_t> rfile;
};
