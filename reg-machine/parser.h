#pragma once

#include <map>
#include <sstream>

#include "instr.h"

machine_t parse(std::istream& is);

void test_parser();
