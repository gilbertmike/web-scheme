#include <assert.h>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include "machine.h"
#include "parser.h"
#include "sicp.h"

#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>

// Right now just a stub.
extern "C" {
  int EMSCRIPTEN_KEEPALIVE add(int a, int b) { return a + b; }
}

extern "C" {
  EMSCRIPTEN_KEEPALIVE
  // TODO save output as string and return instead of printing to cout
  void test_reg_machine(char* s) {
    std::istringstream is(s);
    auto regs = sicp_compiler_registers();

    auto machine = parse(is, regs);

    machine.start();

    assert(machine.pc.get().as<label_t>().dst == machine.instructions.size());

    for (auto &reg : machine.rfile) {
      auto value = reg.get();
      std::cout << reg.get_name() << ": " << value << std::endl;
    }
  }
}
#endif


int main(int argc, char *argv[]) {
#ifndef __EMSCRIPTEN__
  // test_reg_machine(varargs);
#endif
  return 0;
}