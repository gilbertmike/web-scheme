/**
 * @file parser.cpp
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Parser and assembler implementation.
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#include "parser.h"

#include <assert.h>

#include <algorithm>
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
                const token_list_t::iterator& end,
                const std::map<std::string, value_t>& regs);

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
                             const regmap_t& regmap,
                             const labelmap_t& labelmap);
instr_t::u_ptr parse_test(token_list_t::iterator& start, const regmap_t& regmap,
                          const labelmap_t& labelmap);
branch_instr_t::u_ptr parse_branch(token_list_t::iterator& start,
                                   const labelmap_t& labelmap);
instr_t::u_ptr parse_goto(token_list_t::iterator& start, const regmap_t& regmap,
                          const labelmap_t& labelmap);
save_instr_t::u_ptr parse_save(token_list_t::iterator& start,
                               const regmap_t& regmap);
restore_instr_t::u_ptr parse_restore(token_list_t::iterator& start,
                                     const regmap_t& regmap);

std::vector<std::variant<reg_t*, value_t>> parse_operands(
    token_list_t::iterator& start, const regmap_t& regmap,
    const labelmap_t& labelmap);

value_t parse_object(token_list_t::iterator& start);

void skip_whitespace(std::istream& is);

machine_t parse(std::istream& is, const std::map<std::string, value_t>& regs) {
  auto token_list = tokenize(is);
  return parse(token_list.begin(), token_list.end(), regs);
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
      while (c != ')' && c != '(' && !std::isspace(c) && c != EOF) {
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
                const token_list_t::iterator& end,
                const std::map<std::string, value_t>& regs) {
  // this moves program_start to after register declaration
  auto program_start = tokens;

  auto labelmap = parse_labels(program_start, end);
  for (auto& [key, value] : labelmap) {
  }

  machine_t machine(regs.size());

  regmap_t regmap;
  int idx = 0;
  for (const auto& [key, value] : regs) {
    machine.rfile[idx].set(value);
    regmap.insert({key, &machine.rfile[idx]});
    ++idx;
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
      ++it;
      for (int open_paren = 1; open_paren > 0; ++it) {
        if (*it == "(") {
          ++open_paren;
        } else if (*it == ")") {
          --open_paren;
        }
      }
      --it;
      ++pc;
    } else {
      assert(*it != ")");
      labelmap.insert({*it, pc});
    }
  }
  labelmap.insert({"halt", pc});

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
        instr_list.emplace_back(parse_perform(it, regmap, labelmap));
      } else if (it[1] == "test") {
        instr_list.emplace_back(parse_test(it, regmap, labelmap));
      } else if (it[1] == "branch") {
        instr_list.emplace_back(parse_branch(it, labelmap));
      } else if (it[1] == "goto") {
        instr_list.emplace_back(parse_goto(it, regmap, labelmap));
      } else if (it[1] == "save") {
        instr_list.emplace_back(parse_save(it, regmap));
      } else if (it[1] == "restore") {
        instr_list.emplace_back(parse_restore(it, regmap));
      } else {
        throw std::runtime_error(
            std::string("error parsing instruction: ").append(it[1]));
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
        assign_reg_instr_t(dst_ptr, src_ptr));
  } else if (*it == "label") {
    ++it;
    auto label_name = *it;
    auto label = labelmap.at(label_name);

    ++it;
    assert(*it == ")");  // (assign dst (reg src *)* )
    ++it;
    assert(*it == ")");  // (assign dst (reg src) *)*

    return std::make_unique<assign_label_instr_t>(
        assign_label_instr_t(dst_ptr, label_t{label}));
  } else if (*it == "const") {
    ++it;
    auto const_val = parse_object(it);

    assert(*it == ")");  // (assign dst (const src *)* )
    ++it;
    assert(*it == ")");  // (assign dst (const src) *)*

    return std::make_unique<assign_const_instr_t>(
        assign_const_instr_t(dst_ptr, std::move(const_val)));
  } else if (*it == "op") {
    ++it;
    auto op_name = *it;
    auto op = str_to_op(op_name);

    ++it;
    assert(*it == ")");  // (assign dst (op opname *)* args ...)
    ++it;

    auto args = parse_operands(it, regmap, labelmap);

    return std::make_unique<assign_op_instr_t>(
        assign_op_instr_t(dst_ptr, op, std::move(args)));
  } else {
    throw std::runtime_error("error parsing assign");
  }
}

perform_instr_t::u_ptr parse_perform(token_list_t::iterator& it,
                                     const regmap_t& regmap,
                                     const labelmap_t& labelmap) {
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

  auto args = parse_operands(it, regmap, labelmap);

  return std::make_unique<perform_instr_t>(
      perform_instr_t(op, std::move(args)));
}

test_instr_t::u_ptr parse_test(token_list_t::iterator& it,
                               const regmap_t& regmap,
                               const labelmap_t& labelmap) {
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

  auto args = parse_operands(it, regmap, labelmap);

  return std::make_unique<test_instr_t>(test_instr_t(op, std::move(args)));
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

  return std::make_unique<branch_instr_t>(branch_instr_t(label_t{label}));
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

    return std::make_unique<goto_reg_instr_t>(goto_reg_instr_t(reg));
  } else if (it[3] == "label") {
    auto labelname = it[4];
    auto label = labelmap.at(labelname);

    it += 6;

    return std::make_unique<goto_label_instr_t>(
        goto_label_instr_t(label_t{label}));
  }

  throw std::runtime_error("error parsing goto");
}

save_instr_t::u_ptr parse_save(token_list_t::iterator& it,
                               const regmap_t& regmap) {
  assert(it[0] == "(");
  assert(it[1] == "save");

  auto regname = it[2];
  auto reg = regmap.at(regname);

  it += 3;

  return std::make_unique<save_instr_t>(save_instr_t(reg));
}

restore_instr_t::u_ptr parse_restore(token_list_t::iterator& it,
                                     const regmap_t& regmap) {
  assert(it[0] == "(");
  assert(it[1] == "restore");

  auto regname = it[2];
  auto reg = regmap.at(regname);

  it += 3;

  return std::make_unique<restore_instr_t>(restore_instr_t(reg));
}

std::vector<std::variant<reg_t*, value_t>> parse_operands(
    token_list_t::iterator& it, const regmap_t& regmap,
    const labelmap_t& labelmap) {
  std::vector<std::variant<reg_t*, value_t>> operands;

  assert(*it == "(");

  for (; *it != ")"; ++it) {
    if (it[0] == "(") {
      if (it[1] == "reg") {
        auto reg_name = it[2];
        operands.push_back(regmap.at(reg_name));

        assert(it[3] == ")");  // (reg arg *)*
        it += 3;
      } else if (it[1] == "const") {
        it += 2;
        operands.push_back(parse_object(it));

        assert(it[0] == ")");  // (const arg *)*
      } else if (it[1] == "label") {
        operands.push_back(label_t{.dst = labelmap.at(it[2])});
        it += 3;
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
    obj = new pair_t(unassigned_t(), unassigned_t());
  } else if (it[0].front() == '\'') {
    obj = quoted_t{.value = it[0].substr(1)};
  } else if (std::all_of(it[0].begin(), it[0].end(), ::isdigit)) {
    obj = static_cast<int64_t>(std::stoll(it[0]));
  } else if (it[0].front() == '#') {
    obj = it[0].at(1) == 'f';
  } else if (it[0] == "(") {  // quoted list doesn't start with a quote
    ++it;
    std::vector<value_t> vals;
    while (it[0] != ")" && it[0] != ".") { 
      vals.push_back(parse_object(it));
    }
    if (it[0] == ")") {
      obj = pair_t::make_list(vals);
    } else {
      ++it;
      assert(it[0] != ")");
      value_t end = parse_object(it);
      assert(it[0] == ")");
      obj = pair_t::make_improper_list(vals, end);
    }
  } else {  // everything else is interpreted as a symbol
    obj = quoted_t{.value = it[0]};
  }
  ++it;
  return obj;
}

void skip_whitespace(std::istream& is) {
  while (std::isspace(is.peek())) {
    is.get();
  }
}
