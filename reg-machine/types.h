/**
 * @file types.h
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Types of objects a register can store.
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#pragma once

#include <cassert>
#include <cstdint>
#include <map>
#include <unordered_map>
#include <ostream>
#include <stdexcept>
#include <string>
#include <variant>

#include "gc.h"

struct compiled_procedure_t;
struct env_t;
struct pair_t;
struct primitive_procedure_t;

struct quoted_t {
  static quoted_t generate_uninterned(const std::string&);

  quoted_t(const std::string&);
  std::string value;
  int id;
};

struct label_t {
  uintptr_t dst;
};

struct string_t {
  std::string value;
};

// Unassigned variables.
struct unassigned_t {};

// These are for eq?.
bool operator==(const quoted_t&, const quoted_t&);
bool operator==(const label_t&, const label_t&);
bool operator==(const string_t&, const string_t&);
bool operator==(const unassigned_t&, const unassigned_t&);

// Variables contain pointer to an object.
struct value_t {
  std::variant<compiled_procedure_t*, env_t*, primitive_procedure_t*, pair_t*,
               string_t, quoted_t, label_t, int64_t, bool, unassigned_t>
      value;

  value_t();
  value_t(const value_t& other);
  value_t(value_t& other);
  value_t(value_t&& other);
  template <typename T>
  value_t(T&& val) : value(std::forward<T>(val)) {}

  value_t& operator=(const value_t& other);

  template <typename T>
  T as() const {
    return std::get<T>(value);
  }

  template <typename T>
  bool has() const {
    return std::holds_alternative<T>(value);
  }

  friend std::ostream& operator<<(std::ostream& out, const value_t& value);

  garbage_collected_t* as_garbage_collected() const;
  void mark();
};

// Equivalent to eq?;
bool operator==(const value_t&, const value_t&);

struct pair_t : public garbage_collected_t {
  value_t car;
  value_t cdr;

  pair_t() : car(unassigned_t()), cdr(unassigned_t()) {}
  pair_t(value_t&& car, value_t&& cdr) : car(car), cdr(cdr) {}

  static value_t make_list(const std::vector<value_t>& vals) {
    if (vals.size() < 1) return unassigned_t();
    pair_t* init_pair = new pair_t;
    pair_t* cur_pair = init_pair;
    cur_pair->car = vals.at(0);
    for (auto it = vals.begin() + 1; it != vals.end(); ++it) {
      cur_pair->cdr = new pair_t;
      cur_pair = cur_pair->cdr.as<pair_t*>();
      cur_pair->car = *it;
    }
    return init_pair;
  }

  static pair_t* make_improper_list(const std::vector<value_t>& vals,
                                    const value_t& end) {
    assert(!vals.empty());
    pair_t* init_pair = new pair_t;
    pair_t* cur_pair = init_pair;
    cur_pair->car = vals.at(0);
    for (auto it = vals.begin() + 1; it != vals.end(); ++it) {
      cur_pair->cdr = new pair_t;
      cur_pair = cur_pair->cdr.as<pair_t*>();
      cur_pair->car = *it;
    }
    cur_pair->cdr = end;
    return init_pair;
  }

  void mark_children() override {
    car.mark();
    cdr.mark();
  }
};
