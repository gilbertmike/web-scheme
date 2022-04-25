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

std::ostream& operator<<(std::ostream& out, value_t& value) {
  std::visit(
      [&](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, int64_t>) {
          out << arg;
        } else if constexpr (std::is_same_v<T, bool>) {
          out << arg;
        } else if constexpr (std::is_same_v<T, pair_t*>) {
          out << "(" << arg->car << " . " << arg->cdr << ")";
        } else if constexpr (std::is_same_v<T, quoted_t>) {
          out << arg.value;
        } else if constexpr (std::is_same_v<T, compiled_procedure_t*>) {
          out << "#[compiled procedure]";
        } else if constexpr (std::is_same_v<T, primitive_procedure_t*>) {
          out << "#[primitive procedure]";
        } else if constexpr (std::is_same_v<T, env_t*>) {
          out << "#[environment]";
        } else if constexpr (std::is_same_v<T, unassigned_t>) {
          out << "#unassigned";
        } else if constexpr (std::is_same_v<T, label_t>) {
          out << "#[instruction at " << arg.dst << "]";
        }
      },
      value.value);
  return out;
}