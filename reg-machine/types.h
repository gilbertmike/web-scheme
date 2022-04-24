#pragma once

#include <cstdint>
#include <map>
#include <stdexcept>
#include <string>
#include <variant>

#include "gc.h"

struct env_t;

struct pair_t;

struct quoted_t {
  std::string value;
};

struct label_t {
  uintptr_t dst;
};

// unassigned variables
struct unassigned_t {};

bool operator==(const unassigned_t&, const unassigned_t&);

// variables contain pointer to an object
struct value_t {
  std::variant<pair_t*, quoted_t, label_t, int64_t, bool, unassigned_t> value;

  value_t() : value(unassigned_t()) {}
  value_t(const value_t& other) : value(other.value) {}
  value_t(value_t& other)  // otherwise it's inferred to be the templated
                           // version
      : value(other.value) {}
  value_t(value_t&& other) : value(std::move(other.value)) {}
  template <typename T>
  value_t(T&& val) : value(std::forward<T>(val)) {}

  value_t& operator=(const value_t& other) {
    value = other.value;
    return *this;
  }

  template <typename T>
  T as() const {
    return std::get<T>(value);
  }

  void mark_children() {
    std::visit(
        [](auto&& arg) {
          if constexpr (std::is_pointer_v<decltype(arg)>) {
            arg->mark_children();
          }
        },
        value);
  }
};

struct env_t : garbage_collected_t {
  std::map<std::string, value_t> mapping;

  env_t() : mapping() {}

  void extend_environment() {}

  void define_variable(const std::string& varname, value_t value) {
    mapping.at(varname) = value;
  }

  value_t lookup_var_value(const std::string& varname) {
    return mapping.at(varname);
  }

  void mark_children() {
    for (auto& [key, value] : mapping) {
      value.mark_children();
    }
  }
};

struct pair_t : garbage_collected_t {
  value_t car;
  value_t cdr;

  pair_t(value_t&& car, value_t&& cdr) : car(car), cdr(cdr) {}

  static pair_t* make_list(std::vector<value_t>& vals) {
    pair_t* init_pair = new pair_t(unassigned_t(), unassigned_t());
    pair_t* cur_pair = init_pair;
    for (auto& val : vals) {
      cur_pair->car = val;
      cur_pair->cdr = new pair_t(unassigned_t(), unassigned_t());
      cur_pair = cur_pair->cdr.as<pair_t*>();
    }
    return init_pair;
  }

  void mark_children() {
    car.mark_children();
    cdr.mark_children();
  }
};
