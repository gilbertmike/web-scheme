#include "types.h"

bool operator==(const unassigned_t&, const unassigned_t&) { return true; }

value_t::value_t() : value(unassigned_t()) {}
value_t::value_t(const value_t& other) : value(other.value) {}
value_t::value_t(value_t& other) : value(other.value) {}
value_t::value_t(value_t&& other) : value(std::move(other.value)) {}

value_t& value_t::operator=(const value_t& other) {
  value = other.value;
  return *this;
}

void value_t::mark_children() {
  std::visit(
      [](auto&& arg) {
        if constexpr (std::is_pointer_v<decltype(arg)>) {
          arg->mark_children();
        }
      },
      value);
}
