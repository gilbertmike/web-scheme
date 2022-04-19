#pragma once

#include <memory>
#include <string>
#include <vector>

#include "op.h"

struct reg_t;
struct machine_t;

/*** Base instructions ***/

struct instr_t {
  typedef std::unique_ptr<instr_t> u_ptr;
};

struct assign_reg_instr_t : instr_t {
  reg_t* dst;
  reg_t* src;
};

struct assign_const_instr_t : instr_t {
  reg_t* dst;
  value_t src;
};

struct assign_op_instr_t : instr_t {
  reg_t* dst;
  op_t* op;
  std::vector<std::variant<reg_t*, value_t>> args;
};

struct perform_instr_t : instr_t {
  op_t* op;
  std::vector<std::variant<reg_t*, value_t>> args;
};

struct test_instr_t : instr_t {
  op_t* op;
  std::vector<std::variant<reg_t*, value_t>> args;
};

/*** Label instructions ***/

struct branch_instr_t : instr_t {
  label_t dst;
};

struct goto_label_instr_t : instr_t {
  label_t dst;
};

struct assign_label_instr_t : instr_t {
  reg_t* dst;
  label_t label;
};

struct goto_reg_instr_t : instr_t {
  reg_t* dst;
};

/*** Stack instructions ***/

struct save_instr_t : instr_t {
  reg_t* src;
};

struct restore_instr_t : instr_t {
  reg_t* dst;
};
