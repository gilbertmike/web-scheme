#pragma once

#include <cstdint>
#include <variant>

#include "gc.h"

struct object_t;

// variables contain pointer to an object
typedef object_t* value_t;

struct pair_t {
  value_t car;
  value_t cdr;
};

// unassigned variables
extern object_t* unassgined;

struct object_t : garbage_collected_t {
  std::variant<int64_t, pair_t, bool> value;

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
          } else {
          }
        },
        value);
  };
};
