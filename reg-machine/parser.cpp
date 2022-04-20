#include "parser.h"

#include <assert.h>

#include <algorithm>
#include <iostream>
#include <istream>
#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "instr.h"
#include "machine.h"

typedef std::map<std::string, reg_t*> regmap_t;
typedef std::map<std::string, uintptr_t> labelmap_t;
typedef std::vector<std::string> token_list_t;

token_list_t tokenize(std::istream& is);

machine_t parse(const token_list_t::iterator& start,
                const token_list_t::iterator& end);

std::map<std::string, uintptr_t> parse_reg_decl(token_list_t::iterator& start);

std::map<std::string, uintptr_t> parse_labels(
    const token_list_t::iterator& start, const token_list_t::iterator& end);

std::vector<instr_t::u_ptr> parse_lines(const token_list_t::iterator& start,
                                        const token_list_t::iterator& end,
                                        const regmap_t& regmap,
                                        const labelmap_t& labelmap);

instr_t::u_ptr parse_assign(token_list_t::iterator& start,
                            const regmap_t& regmap, const labelmap_t& labelmap);
instr_t::u_ptr parse_perform(token_list_t::iterator& start,
                             const regmap_t& regmap);
instr_t::u_ptr parse_test(token_list_t::iterator& start,
                          const regmap_t& regmap);
branch_instr_t::u_ptr parse_branch(token_list_t::iterator& start,
                                   const labelmap_t& labelmap);
instr_t::u_ptr parse_goto(token_list_t::iterator& start, const regmap_t& regmap,
                          const labelmap_t& labelmap);
save_instr_t::u_ptr parse_save(token_list_t::iterator& start,
                               const regmap_t& regmap);
restore_instr_t::u_ptr parse_restore(token_list_t::iterator& start,
                                     const regmap_t& regmap);

std::vector<std::variant<reg_t*, value_t>> parse_operands(
    token_list_t::iterator& start, const regmap_t& regmap);

value_t parse_object(token_list_t::iterator& start);

void skip_whitespace(std::istream& is);

machine_t parse(std::istream& is) {
  auto token_list = tokenize(is);
  return parse(token_list.begin(), token_list.end());
}

token_list_t tokenize(std::istream& is) {
  token_list_t tokens;
  while (is.peek() != EOF) {
    char c = is.get();
    if (std::isspace(c)) continue;

    if (c == '(') {
      tokens.push_back("(");
    } else if (c == ')') {
      tokens.push_back(")");
    } else {
      std::string token;
      while (c != ')' && c != '(' && !std::isspace(c)) {
        token.push_back(c);
        c = is.get();
      }
      tokens.push_back(token);

      if (c == '(') {
        tokens.push_back("(");
      } else if (c == ')') {
        tokens.push_back(")");
      }
    }
  }
  return tokens;
}

machine_t parse(const token_list_t::iterator& tokens,
                const token_list_t::iterator& end) {
  // this moves program_start to after register declaration
  auto program_start = tokens;
  auto regmap_idx = parse_reg_decl(program_start);

  auto labelmap = parse_labels(program_start, end);

  machine_t machine(regmap_idx.size());

  regmap_t regmap;
  for (const auto& [key, value] : regmap_idx) {
    regmap.insert({key, &machine.rfile[value]});
  }

  auto instr_list = parse_lines(program_start, end, regmap, labelmap);
  machine.instructions = std::move(instr_list);

  return machine;
}

std::map<std::string, uintptr_t> parse_reg_decl(token_list_t::iterator& token) {
  std::map<std::string, uintptr_t> regmap;

  assert(token[0] == "(");
  assert(token[1] == "registers");

  uintptr_t pc = 0;
  for (token += 2; (*token) != ")"; ++token) {
    regmap.insert({*token, pc++});
  }

  assert(*token == ")");
  ++token;

  return regmap;
}

std::map<std::string, uintptr_t> parse_labels(
    const token_list_t::iterator& start, const token_list_t::iterator& end) {
  std::map<std::string, uintptr_t> labelmap;
  uintptr_t pc = 0;

  for (auto it = start; it != end; ++it) {
    if (*it == "(") {
      while (*it != ")" && it != end) {
        ++it;
      }
    } else if (*it == ")") {
      /* do nothing */
    } else {
      labelmap.insert({*it, pc});
    }
  }

  return labelmap;
}

std::vector<instr_t::u_ptr> parse_lines(
    const token_list_t::iterator& start, const token_list_t::iterator& end,
    const regmap_t& regmap, const std::map<std::string, uintptr_t>& labelmap) {
  std::vector<instr_t::u_ptr> instr_list;

  for (auto it = start; it != end; ++it) {
    if (it[0] == "(") {
      if (it[1] == "assign") {
        instr_list.emplace_back(parse_assign(it, regmap, labelmap));
      } else if (it[1] == "perform") {
        instr_list.emplace_back(parse_perform(it, regmap));
      } else if (it[1] == "test") {
        instr_list.emplace_back(parse_test(it, regmap));
      } else if (it[1] == "branch") {
        instr_list.emplace_back(parse_branch(it, labelmap));
      } else if (it[1] == "goto") {
        instr_list.emplace_back(parse_goto(it, regmap, labelmap));
      } else if (it[1] == "save") {
        instr_list.emplace_back(parse_save(it, regmap));
      } else if (it[1] == "restore") {
        instr_list.emplace_back(parse_restore(it, regmap));
      } else {
        throw std::runtime_error("error parsing instruction");
      }
    } else {
      /* do nothing */
    }
  }

  return instr_list;
}

instr_t::u_ptr parse_assign(token_list_t::iterator& it, const regmap_t& regmap,
                            const std::map<std::string, uintptr_t>& labelmap) {
  assert(it[0] == "(");
  assert(it[1] == "assign");

  auto dst_name = it[2];
  auto dst_ptr = regmap.at(dst_name);

  assert(it[3] == "(");
  it += 4;

  if (*it == "reg") {
    ++it;
    auto src_name = *it;
    auto src_ptr = regmap.at(*it);

    ++it;
    assert(*it == ")");  // (assign dst (reg src *)* )
    ++it;
    assert(*it == ")");  // (assign dst (reg src) *)*

    return std::make_unique<assign_reg_instr_t>(
        assign_reg_instr_t{.dst = dst_ptr, .src = src_ptr});
  } else if (*it == "label") {
    ++it;
    auto label_name = *it;
    auto label = labelmap.at(label_name);

    ++it;
    assert(*it == ")");  // (assign dst (reg src *)* )
    ++it;
    assert(*it == ")");  // (assign dst (reg src) *)*

    return std::make_unique<assign_label_instr_t>(
        assign_label_instr_t{.dst = dst_ptr, .label = label_t{label}});
  } else if (*it == "const") {
    ++it;
    auto const_val = parse_object(it);

    assert(*it == ")");  // (assign dst (const src *)* )
    ++it;
    assert(*it == ")");  // (assign dst (const src) *)*

    return std::make_unique<assign_const_instr_t>(
        assign_const_instr_t{.dst = dst_ptr, .src = const_val});
  } else if (*it == "op") {
    ++it;
    auto op_name = *it;
    auto op = str_to_op(op_name);

    ++it;
    assert(*it == ")");  // (assign dst (op opname *)* args ...)
    ++it;

    auto args = parse_operands(it, regmap);
    ++it;

    return std::make_unique<assign_op_instr_t>(
        assign_op_instr_t{.dst = dst_ptr, .op = op, .args = args});
  } else {
    throw std::runtime_error("error parsing assign");
  }
}

perform_instr_t::u_ptr parse_perform(token_list_t::iterator& it,
                                     const regmap_t& regmap) {
  assert(*it == "(");
  ++it;
  assert(*it == "perform");
  ++it;
  assert(*it == "(");
  ++it;
  assert(*it == "op");
  ++it;
  auto op_name = *it;
  auto op = str_to_op(op_name);

  ++it;
  assert(*it == ")");  // (assign dst (op opname *)* args ...)
  ++it;

  auto args = parse_operands(it, regmap);

  return std::make_unique<perform_instr_t>(
      perform_instr_t{.op = op, .args = args});
}

test_instr_t::u_ptr parse_test(token_list_t::iterator& it,
                               const regmap_t& regmap) {
  assert(*it == "(");
  ++it;
  assert(*it == "test");
  ++it;
  assert(*it == "(");
  ++it;
  assert(*it == "op");
  ++it;
  auto op_name = *it;
  auto op = str_to_op(op_name);

  ++it;
  assert(*it == ")");  // (assign dst (op opname *)* args ...)
  ++it;

  auto args = parse_operands(it, regmap);
  ++it;

  return std::make_unique<test_instr_t>(test_instr_t{.op = op, .args = args});
}

branch_instr_t::u_ptr parse_branch(token_list_t::iterator& it,
                                   const labelmap_t& labelmap) {
  assert(it[0] == "(");
  assert(it[1] == "branch");
  assert(it[2] == "(");
  assert(it[3] == "label");

  auto labelname = it[4];
  auto label = labelmap.at(labelname);

  it += 6;  // to last paren

  return std::make_unique<branch_instr_t>(
      branch_instr_t{.dst = label_t{label}});
}

instr_t::u_ptr parse_goto(token_list_t::iterator& it, const regmap_t& regmap,
                          const labelmap_t& labelmap) {
  assert(it[0] == "(");
  assert(it[1] == "goto");
  assert(it[2] == "(");

  if (it[3] == "reg") {
    auto regname = it[4];
    auto reg = regmap.at(regname);

    it += 6;  // to last paren

    return std::make_unique<goto_reg_instr_t>(goto_reg_instr_t{.dst = reg});
  } else if (it[3] == "label") {
    auto labelname = it[4];
    auto label = labelmap.at(labelname);

    it += 7;

    return std::make_unique<goto_label_instr_t>(
        goto_label_instr_t{.dst = label_t{label}});
  }

  throw std::runtime_error("error parsing goto");
}

save_instr_t::u_ptr parse_save(token_list_t::iterator& it,
                               const regmap_t& regmap) {
  assert(it[0] == "(");
  assert(it[1] == "save");

  auto regname = it[2];
  auto reg = regmap.at(regname);

  it += 4;

  return std::make_unique<save_instr_t>(save_instr_t{.src = reg});
}

restore_instr_t::u_ptr parse_restore(token_list_t::iterator& it,
                                     const regmap_t& regmap) {
  assert(it[0] == "(");
  assert(it[1] == "restore");

  auto regname = it[2];
  auto reg = regmap.at(regname);

  it += 4;

  return std::make_unique<restore_instr_t>(restore_instr_t{.dst = reg});
}

std::vector<std::variant<reg_t*, value_t>> parse_operands(
    token_list_t::iterator& it, const regmap_t& regmap) {
  std::vector<std::variant<reg_t*, value_t>> operands;

  assert(*it == "(");

  for (; *it != ")"; ++it) {
    if (*it == "(") {
      ++it;
      if (*it == "reg") {
        ++it;
        auto reg_name = *it;
        operands.push_back(regmap.at(reg_name));

        ++it;
        assert(*it == ")");  // (reg arg *)*
        ++it;
      } else if (*it == "const") {
        ++it;
        operands.push_back(parse_object(it));

        assert(*it == ")");  // (const arg *)*
        ++it;
      } else {
        throw std::runtime_error("error parsing operands");
      }
    }
  }

  return operands;
}

value_t parse_object(token_list_t::iterator& it) {
  value_t obj;
  if (it[0] == "empty-list") {
    obj = new object_t(pair_t{.car = unassgined, .cdr = unassgined});
  } else if (it[0].front() == '\'') {
    obj = new object_t(quoted_t{.value = it[0].substr(1)});
  } else if (std::all_of(it[0].begin(), it[0].end(), ::isdigit)) {
    obj = new object_t(static_cast<int64_t>(std::stoll(it[0])));
  } else if (it[0].front() == '#') {
    obj = new object_t(it[0].at(1) == 'f');
  } else {
    throw std::runtime_error("error parsing object");
  }
  ++it;
  return obj;
}

void skip_whitespace(std::istream& is) {
  while (std::isspace(is.peek())) {
    is.get();
  }
}

void test_parser() {
  const char* test_program =
      "(registers a b c)"
      " label-1"
      "  (assign a ( reg b) )  "
      "(assign b (op +) (const 1) (const 2))\n"
      "(assign c (const #f))"
      "(test (op =) (reg b) (const 3))"
      "(branch (label label-2))"
      "(assign a (const empty-list))"
      "label-2"
      "(assign b (const empty-list))"
      "(assign c (op cons) (reg c) (reg b))";

  std::stringstream ss(test_program);
  auto tokens = tokenize(ss);
  auto machine = parse(tokens.begin(), tokens.end());

  assert(machine.rfile.size() == 3);
  assert(machine.instructions.size() == 8);
}
