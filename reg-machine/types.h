#pragma once

#include <cstdint>
#include <stdexcept>
#include <string>
#include <variant>

#include "gc.h"

struct object_t;

// variables contain pointer to an object
typedef object_t* value_t;

// unassigned variables
extern object_t* unassgined;

struct pair_t {
  value_t car;
  value_t cdr;
};

struct quoted_t {
  std::string value;
};

struct label_t {
  uintptr_t dst;
};

struct object_t : garbage_collected_t {
  std::variant<int64_t, bool, pair_t, quoted_t, label_t> value;

  object_t() : value() {}

  template <typename T>
  object_t(T&& value) : value(std::forward(value)) {}

  void mark_children() {
    std::visit(
        [](auto&& arg) {
          using T = std::decay_t<decltype(arg)>;
          if constexpr (std::is_same_v<T, int64_t>) {
            /* do nothing */
          } else if constexpr (std::is_same_v<T, bool>) {
            /* do nothing */
          } else if constexpr (std::is_same_v<T, pair_t>) {
            arg.car->mark_children();
            arg.cdr->mark_children();
          } else if constexpr (std::is_same_v<T, quoted_t>) {
            /* do nothing */
          } else if constexpr (std::is_same_v<T, label_t>) {
            /* do nothing */
          } else {
            throw std::runtime_error("not all object variant handled");
          }
        },
        value);
  };
};
