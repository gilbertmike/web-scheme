#pragma once

#include <cstdint>
#include <functional>
#include <stack>
#include <vector>

#include "instr.h"
#include "types.h"

class reg_t {
 public:
  reg_t();
  reg_t(const value_t& value);

  void set(const value_t& new_value);

  value_t get() const;

  void set_name(const std::string& name);

  std::string_view get_name() const;

 private:
  value_t value;
  std::string name = "[unnamed]";
};

class stack_t {
 public:
  stack_t();

  void push(const value_t& value);
  value_t pop();
  bool empty() const;

 private:
  std::stack<value_t> stack;
};

struct machine_t {
  machine_t(int rfile_size);
  machine_t(int rfile_size, std::vector<instr_t::u_ptr>&& instructions);

  static machine_t construct_from_string(std::string_view text);

  static bool any_running();
  static machine_t& current();
  void set_current();
  void yield_current();

  void set_output(std::ostream& output) { this->output = &output; }
  void set_input(std::function<std::string()> input) { this->input = input; }

  void start();
  void do_gc();

  reg_t pc;
  reg_t flag;
  stack_t stack;
  std::ostream* output;
  std::function<std::string()> input;
  std::vector<instr_t::u_ptr> instructions;
  garbage_collector_t gc;

  std::vector<reg_t> rfile;
};
