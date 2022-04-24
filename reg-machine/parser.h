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

machine_t parse(std::istream& is);
