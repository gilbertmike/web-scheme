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
  std::ostringstream os;
  {
    auto regs = sicp_compiler_registers();
    auto machine = parse(is, regs);
    machine.set_output(os);
    machine.start();

    if (machine.pc.get().as<label_t>().dst != machine.instructions.size()) {
      os << "WARNING: the machine stops in the middle of instructions!" << std::endl;
    }  
    os << "Execution complete! Printing register values..." << std::endl;
    for (auto &reg : machine.rfile) {
      auto value = reg.get();
      os << reg.get_name() << ": " << value << std::endl;
    }
  }
  return os.str();
}

int main(int argc, char *argv[]) {
#ifndef __EMSCRIPTEN__
  // test_reg_machine(varargs);
#endif
  return 0;
}