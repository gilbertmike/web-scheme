/**
 * @file primitive-procs.cpp
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Implementation of primitive procedures in machine ops.
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#include "primitive-procs.h"

#include <cassert>
#include <iostream>
#include <sstream>

#include "compiled-proc.h"
#include "instr.h"
#include "machine.h"
#include "op.h"
#include "parser.h"

struct add_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());

    int64_t result = 0;
    pair_t* cur = args.as<pair_t*>();
    for (; cur->cdr.has<pair_t*>(); cur = cur->cdr.as<pair_t*>()) {
      result += cur->car.as<int64_t>();
    }
    result += cur->car.as<int64_t>();

    return result;
  }
};

primitive_procedure_t* add_primitive_proc = new add_procedure_t;

struct sub_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());

    pair_t* cur = args.as<pair_t*>();
    int64_t result = cur->car.as<int64_t>();
    if (cur->cdr.has<unassigned_t>()) {
      return -result;
    } else if (cur->cdr.has<pair_t*>()) {
      cur = cur->cdr.as<pair_t*>();
      for (; cur->cdr.has<pair_t*>(); cur = cur->cdr.as<pair_t*>()) {
        result -= cur->car.as<int64_t>();
      }
      result -= cur->car.as<int64_t>();
    }

    return result;
  }
};

primitive_procedure_t* sub_primitive_proc = new sub_procedure_t;

struct mul_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());

    int64_t result = 1;
    pair_t* cur = args.as<pair_t*>();
    for (; cur->cdr.has<pair_t*>(); cur = cur->cdr.as<pair_t*>()) {
      result *= cur->car.as<int64_t>();
    }
    result *= cur->car.as<int64_t>();

    return result;
  }
};

primitive_procedure_t* mul_primitive_proc = new mul_procedure_t;

struct int_eq_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());
    pair_t* args_pair = args.as<pair_t*>();
    assert(args_pair->cdr.has<pair_t*>());
    return args_pair->car.as<int64_t>() ==
           args_pair->cdr.as<pair_t*>()->car.as<int64_t>();
  }
};

primitive_procedure_t* int_eq_primitive_proc = new int_eq_procedure_t;

template <typename T>
struct type_test_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());
    pair_t* args_pair = args.as<pair_t*>();
    assert(args_pair->cdr.has<unassigned_t>());
    return args_pair->car.has<T>();
  }
};

primitive_procedure_t* number_test_primitive_proc =
    new type_test_procedure_t<int64_t>();
primitive_procedure_t* pair_test_primitive_proc =
    new type_test_procedure_t<pair_t*>();
primitive_procedure_t* bool_test_primitive_proc =
    new type_test_procedure_t<bool>();
primitive_procedure_t* string_test_primitive_proc =
    new type_test_procedure_t<string_t>();
primitive_procedure_t* symbol_test_primitive_proc =
    new type_test_procedure_t<quoted_t>();
primitive_procedure_t* null_test_primitive_proc =
    new type_test_procedure_t<unassigned_t>();

struct print_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<pair_t*>());
    pair_t* args_pair = args.as<pair_t*>();
    std::ostream& output = *machine_t::current().output;
    output << args_pair->car;
    bool endl = true;
    if (args_pair->cdr.has<pair_t*>()) {
      args_pair = args_pair->cdr.as<pair_t*>();
      if (args_pair->car.has<bool>()) endl = args_pair->car.as<bool>();
    }
    if (endl) output << std::endl;
    if (&output != &std::cout) {
      std::cout << args_pair->car;
      if (endl) std::cout << std::endl;
    }
    return unassigned_t();
  }
};

primitive_procedure_t* print_primitive_proc = new print_procedure_t;

struct input_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    assert(args.has<unassigned_t>());
    const auto& callback = machine_t::current().input;
    if (callback == nullptr) {
      throw std::runtime_error("no input callback specified");
    }
    std::istringstream input_raw{callback()};
    token_list_t tokens = tokenize(input_raw);
    token_list_t::iterator start = tokens.begin();
    return parse_object(start);
  }
};

primitive_procedure_t* input_primitive_proc = new input_procedure_t;

inline const value_t& one_arg(const value_t& args) {
  assert(args.has<pair_t*>());
  const pair_t* first_pair = args.as<pair_t*>();
  assert(first_pair->cdr.has<unassigned_t>());
  return first_pair->car;
}

inline std::pair<const value_t&, const value_t&> two_args(const value_t& args) {
  assert(args.has<pair_t*>());
  const pair_t* first_pair = args.as<pair_t*>();
  assert(first_pair->cdr.has<pair_t*>());
  const pair_t* second_pair = first_pair->cdr.as<pair_t*>();
  assert(second_pair->cdr.has<unassigned_t>());
  return {first_pair->car, second_pair->car};
}

struct car_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    const value_t& pair = one_arg(args);
    assert(pair.has<pair_t*>());
    return pair.as<pair_t*>()->car;
  }
};

primitive_procedure_t* car_primitive_proc = new car_procedure_t;

struct cdr_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    const value_t& pair = one_arg(args);
    assert(pair.has<pair_t*>());
    return pair.as<pair_t*>()->cdr;
  }
};

primitive_procedure_t* cdr_primitive_proc = new cdr_procedure_t;

struct cons_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    const auto& [car, cdr] = two_args(args);
    return new pair_t(value_t{car}, value_t{cdr});
  }
};

primitive_procedure_t* cons_primitive_proc = new cons_procedure_t;

struct error_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    machine_t& current = machine_t::current();
    // Right now we do not distinguish between stdout and stderr.
    std::ostream& output = *current.output;
    output << "ERROR: " << args << std::endl;
    if (&output != &std::cerr) {
      std::cerr << "ERROR: " << args << std::endl;
    }
    // End the program!
    current.pc.set(label_t{current.instructions.size()});
    return unassigned_t();
  }
};

primitive_procedure_t* error_primitive_proc = new error_procedure_t;

struct apply_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    machine_t& current = machine_t::current();
    assert(args.has<pair_t*>());
    const pair_t* first_args = args.as<pair_t*>();
    const value_t& proc = first_args->car;
    value_t argl{unassigned_t()};
    if (first_args->cdr.has<pair_t*>()) {
      std::vector<value_t> temp;
      pair_t* cur = first_args->cdr.as<pair_t*>();
      for (; cur->cdr.has<pair_t*>(); cur = cur->cdr.as<pair_t*>()) {
        temp.push_back(cur->car);
      }
      argl =
          temp.empty() ? cur->car : pair_t::make_improper_list(temp, cur->car);
    }
    if (proc.has<primitive_procedure_t*>()) {
      return proc.as<primitive_procedure_t*>()->execute(argl);
    } else if (proc.has<compiled_procedure_t*>()) {
      assert(proc.has<compiled_procedure_t*>());
      const compiled_procedure_t* comp = proc.as<compiled_procedure_t*>();
      const instr_t* next =
          current.instructions.at(current.pc.get().as<label_t>().dst).get();
      bool should_update_continue =
          !next->is_goto_reg() ||
          ((goto_reg_instr_t*)next)->dst->get_name() != "continue";
      for (reg_t& reg : current.rfile) {
        if (reg.get_name() == "continue") {
          if (should_update_continue) reg.set(current.pc.get());
        } else if (reg.get_name() == "argl") {
          reg.set(argl);
        } else if (reg.get_name() == "proc") {
          reg.set(proc);
        }
      }
      current.pc.set(comp->entry);
    }
    return unassigned_t();
  }
};

primitive_procedure_t* apply_primitive_proc = new apply_procedure_t;

struct eq_test_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    const auto& [a, b] = two_args(args);
    return a == b;
  }
};

primitive_procedure_t* eq_test_primitive_proc = new eq_test_procedure_t;

struct eqv_test_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    const auto& [a, b] = two_args(args);
    return a == b || ((a.has<string_t>() && b.has<string_t>()) &&
                      (a.as<string_t>().value == b.as<string_t>().value));
  }
};

primitive_procedure_t* eqv_test_primitive_proc = new eqv_test_procedure_t;

struct not_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    const value_t& arg = one_arg(args);
    return !arg.has<bool>() || !arg.as<bool>();
  }
};

primitive_procedure_t* not_primitive_proc = new not_procedure_t;

struct gensym_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    const value_t& arg = one_arg(args);
    if (arg.has<quoted_t>()) {
      return quoted_t::generate_uninterned(arg.as<quoted_t>().value);
    } else if (arg.has<string_t>()) {
      return quoted_t::generate_uninterned(arg.as<string_t>().value);
    }
    __builtin_unreachable();
  }
};

primitive_procedure_t* gensym_primitive_proc = new gensym_procedure_t;

struct halt_procedure_t : primitive_procedure_t {
  value_t execute(const value_t& args) {
    machine_t& current = machine_t::current();
    current.pc.set(label_t{current.instructions.size()});
    return args.as<pair_t*>()->car;
  }
};

primitive_procedure_t* halt_primitive_proc = new halt_procedure_t;