#include <assert.h>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <cstring>

#include "machine.h"
#include "parser.h"
#include "sicp.h"

std::string run_reg_machine(const std::string &input);

#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>

extern "C" {
EMSCRIPTEN_KEEPALIVE
// TODO save output as string and return instead of printing to cout
char *test_reg_machine(char *s) {
  return strdup(run_reg_machine(s).c_str());
}
}
#endif

std::string run_reg_machine(const std::string &input) {
  std::istringstream is{input};
  auto regs = sicp_compiler_registers();
  auto machine = parse(is, regs);

  machine.start();

  assert(machine.pc.get().as<label_t>().dst == machine.instructions.size());
  std::ostringstream result;
  for (auto &reg : machine.rfile) {
    auto value = reg.get();
    result << reg.get_name() << ": " << value << std::endl;
  }
  return result.str();
}

int main(int argc, char *argv[]) {
#ifndef __EMSCRIPTEN__
  // test_reg_machine(varargs);
#endif
  return 0;
}