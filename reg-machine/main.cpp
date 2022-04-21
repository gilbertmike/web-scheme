#include <assert.h>

#include <iostream>

#include "machine.h"
#include "parser.h"

void test_reg_machine() {
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
  auto machine = parse(ss);

  assert(machine.rfile.size() == 3);
  assert(machine.instructions.size() == 8);

  machine.start();

  assert(machine.pc.get()->as<label_t>().dst == 8);
  assert(machine.rfile.at(0).get() == unassgined);
  assert(machine.rfile.at(1).get()->as<pair_t>().car == unassgined);
  assert(machine.rfile.at(1).get()->as<pair_t>().cdr == unassgined);
}

int main(int argc, char* argv[]) {
  test_reg_machine();
  return 0;
}