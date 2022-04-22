#pragma once

#include <cstdint>
#include <stdexcept>
#include <string>
#include <variant>

#include "gc.h"

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

struct pair_t : garbage_collected_t {
  value_t car;
  value_t cdr;

  pair_t(value_t&& car, value_t&& cdr) : car(car), cdr(cdr) {}

  void mark_children() {
    car.mark_children();
    cdr.mark_children();
  }
};
