#include "instr.h"

#include <algorithm>
#include <vector>

template <class>
inline constexpr bool always_false_v = false;

std::vector<value_t> get_arg_values(
    const std::vector<std::variant<reg_t*, value_t>>& args) {
  std::vector<value_t> value_args;
  value_args.reserve(args.size());

  std::transform(args.begin(), args.end(), std::back_inserter(value_args),
                 [](auto&& arg) -> value_t {
                   value_t value = std::visit(
                       [](auto&& arg) -> value_t {
                         using T = decltype(arg);
                         if constexpr (std::is_same_v<T, reg_t*>) {
                           return arg.get();
                         } else if constexpr (std::is_same_v<T, pair_t>) {
                           return arg;
                         } else {
                           __builtin_unreachable();
                         }
                       },
                       arg);

                   return value;
                 });

  return value_args;
}