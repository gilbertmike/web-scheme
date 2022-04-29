#include <assert.h>

#include <fstream>
#include <iostream>

#include "machine.h"
#include "parser.h"
#include "sicp.h"

#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>

// Right now just a stub.
extern "C" {
int EMSCRIPTEN_KEEPALIVE add(int a, int b) { return a + b; }
}
#endif

const std::string TEST_RMA_FILE = "varargs.rma";

void test_reg_machine() {
  std::ifstream test_f(TEST_RMA_FILE);
  auto regs = sicp_compiler_registers();

  auto machine = parse(test_f, regs);

  machine.start();

  assert(machine.pc.get().as<label_t>().dst == machine.instructions.size());

  for (auto &reg : machine.rfile) {
    auto value = reg.get();
    std::cout << reg.get_name() << ": " << value << std::endl;
  }
}

int main(int argc, char *argv[]) {
#ifndef __EMSCRIPTEN__
  test_reg_machine();
#endif
  return 0;
}