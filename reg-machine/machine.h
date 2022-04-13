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

class machine_t {
  machine_t(int rfile_size, const std::vector<instr_t>& instructions);

  static machine_t construct_from_string(std::string_view text);

  void start();

  reg_t pc;
  reg_t flag;
  stack_t stack;
  std::vector<instr_t> instructions;

  std::vector<reg_t> rfile;
};
