/**
 * @file primitive-procs.h
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Primitive procedure (primitive) type.
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#pragma once

struct value_t;

struct primitive_procedure_t {
  virtual value_t execute(const value_t& args) = 0;
};

struct add_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) override;
};

struct sub_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) override;
};

struct mul_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) override;
};
