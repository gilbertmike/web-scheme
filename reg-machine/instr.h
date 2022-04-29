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

  virtual ~instr_t() {};
  virtual void execute(machine_t& machine) = 0;
};

struct assign_reg_instr_t : instr_t {
  reg_t* dst;
  reg_t* src;

  assign_reg_instr_t(reg_t* dst, reg_t* src) : dst(dst), src(src) {}

  void execute(machine_t& machine) override;
};

struct assign_const_instr_t : instr_t {
  reg_t* dst;
  value_t src;

  assign_const_instr_t(reg_t* dst, value_t&& src)
      : dst(dst), src(std::move(src)) {}

  void execute(machine_t& machine) override;
};

struct assign_op_instr_t : instr_t {
  reg_t* dst;
  op_t* op;
  std::vector<std::variant<reg_t*, value_t>> args;

  assign_op_instr_t(reg_t* dst, op_t* op,
                    std::vector<std::variant<reg_t*, value_t>>&& args)
      : dst(dst), op(op), args(std::move(args)) {}

  void execute(machine_t& machine) override;
};

struct perform_instr_t : instr_t {
  op_t* op;
  std::vector<std::variant<reg_t*, value_t>> args;

  perform_instr_t(op_t* op, std::vector<std::variant<reg_t*, value_t>>&& args)
      : op(op), args(std::move(args)) {}

  void execute(machine_t& machine) override;
};

struct test_instr_t : instr_t {
  op_t* op;
  std::vector<std::variant<reg_t*, value_t>> args;

  test_instr_t(op_t* op, std::vector<std::variant<reg_t*, value_t>>&& args)
      : op(op), args(std::move(args)) {}

  void execute(machine_t& machine) override;
};

/*** Label instructions ***/

struct branch_instr_t : instr_t {
  label_t dst;

  branch_instr_t(label_t dst) : dst(dst) {}

  void execute(machine_t& machine) override;
};

struct goto_label_instr_t : instr_t {
  label_t dst;

  goto_label_instr_t(label_t dst) : dst(dst) {}

  void execute(machine_t& machine) override;
};

struct assign_label_instr_t : instr_t {
  reg_t* dst;
  label_t label;

  assign_label_instr_t(reg_t* dst, label_t label) : dst(dst), label(label) {}

  void execute(machine_t& machine) override;
};

struct goto_reg_instr_t : instr_t {
  reg_t* dst;

  goto_reg_instr_t(reg_t* dst) : dst(dst) {}

  void execute(machine_t& machine) override;
};

/*** Stack instructions ***/

struct save_instr_t : instr_t {
  reg_t* src;

  save_instr_t(reg_t* src) : src(src) {}

  void execute(machine_t& machine) override;
};

struct restore_instr_t : instr_t {
  reg_t* dst;

  restore_instr_t(reg_t* dst) : dst(dst) {}

  void execute(machine_t& machine) override;
};
