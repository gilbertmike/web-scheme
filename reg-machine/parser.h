/**
 * @file parser.h
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Parser and assembler
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#pragma once

#include <map>
#include <sstream>

#include "instr.h"

machine_t parse(std::istream& is,
                const std::map<std::string, value_t>& predefined_regs);

typedef std::map<std::string, reg_t*> regmap_t;
typedef std::map<std::string, uintptr_t> labelmap_t;
typedef std::vector<std::string> token_list_t;

token_list_t tokenize(std::istream& is);
value_t parse_object(token_list_t::iterator& it);