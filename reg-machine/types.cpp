#include "types.h"

#include <algorithm>
#include <mutex>
#include <unordered_map>
#include <unordered_set>

bool operator==(const label_t& a, const label_t& b) { return a.dst == b.dst; }
bool operator==(const quoted_t& a, const quoted_t& b) { return a.id == b.id; }
bool operator==(const string_t& a, const string_t& b) {
  // Avoid character-by-character comparison here.
  return a.value.c_str() == b.value.c_str();
}
bool operator==(const unassigned_t&, const unassigned_t&) { return true; }
bool operator==(const value_t& a, const value_t& b) {
  return a.value == b.value;
}

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

std::ostream& print(std::ostream& out, const value_t& value,
                    std::unordered_set<void*>& seen);

void try_print_list(std::ostream& out, const pair_t* cur,
                    std::unordered_set<void*>& seen) {
  out << "(";
  while (cur->cdr.has<pair_t*>()) {
    print(out, cur->car, seen) << " ";
    cur = cur->cdr.as<pair_t*>();
  }
  print(out, cur->car, seen);
  if (!cur->cdr.has<unassigned_t>()) {
    print(out << " . ", cur->cdr, seen);
  }
  out << ")";
}

std::ostream& print(std::ostream& out, const value_t& value,
                    std::unordered_set<void*>& seen) {
  std::visit(
      [&](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, int64_t>) {
          out << arg;
        } else if constexpr (std::is_same_v<T, bool>) {
          out << (arg ? "#t" : "#f");
        } else if constexpr (std::is_same_v<T, pair_t*>) {
          if (seen.find(arg) != seen.end()) {
            out << "#[recursive pair detected]";
          } else {
            seen.insert(arg);
            try_print_list(out, arg, seen);
            seen.erase(arg);
          }
        } else if constexpr (std::is_same_v<T, quoted_t>) {
          out << arg.value;
        } else if constexpr (std::is_same_v<T, string_t>) {
          out << '"' << arg.value << '"';
        } else if constexpr (std::is_same_v<T, compiled_procedure_t*>) {
          out << "#[compiled procedure]";
        } else if constexpr (std::is_same_v<T, primitive_procedure_t*>) {
          out << "#[primitive procedure]";
        } else if constexpr (std::is_same_v<T, env_t*>) {
          out << "#[environment]";
        } else if constexpr (std::is_same_v<T, unassigned_t>) {
          out << "()";
        } else if constexpr (std::is_same_v<T, label_t>) {
          out << "#[instruction at " << arg.dst << "]";
        }
      },
      value.value);
  return out;
}

std::ostream& operator<<(std::ostream& out, const value_t& value) {
  std::unordered_set<void*> seen;
  return print(out, value, seen);
}

int quoted_next_id = 0;
std::unordered_map<std::string, int> quoted_interner{};
std::mutex quoted_interner_mutex;

quoted_t::quoted_t(const std::string& value) {
  this->value = value;
  std::transform(this->value.begin(), this->value.end(), this->value.begin(),
                 [](auto ch) { return std::tolower(ch); });
  quoted_interner_mutex.lock();
  if (auto it = quoted_interner.find(this->value);
      it != quoted_interner.end()) {
    id = it->second;
  } else {
    quoted_interner.insert({this->value, quoted_next_id});
    id = quoted_next_id++;
  }
  quoted_interner_mutex.unlock();
}

quoted_t quoted_t::generate_uninterned(const std::string& base) {
  // Finds the smallest number x such that base + x has not been interned, in
  // logarithmic time.
  int lower_bound = 0;
  int upper_bound = 0;
  int step = 1;
  quoted_interner_mutex.lock();
  while (true) {
    const std::string temp = base + std::to_string(upper_bound);
    if (auto it = quoted_interner.find(temp); it == quoted_interner.end()) {
      break;
    } else {
      lower_bound = upper_bound + 1;
      upper_bound += step;
      step <<= 1;
    }
  }
  while (lower_bound < upper_bound) {
    const int mid = (lower_bound + upper_bound) / 2;
    const std::string temp = base + std::to_string(mid);
    if (auto it = quoted_interner.find(temp); it == quoted_interner.end()) {
      upper_bound = mid;
    } else {
      lower_bound = mid + 1;
    }
  }
  quoted_interner_mutex.unlock();
  return quoted_t{base + std::to_string(lower_bound)};
}