#include <assert.h>

#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include "machine.h"
#include "parser.h"
#include "sicp.h"

std::string run_reg_machine(
    const std::string &input,
    std::function<std::string()> input_callback = nullptr);

#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>

extern "C" {
EMSCRIPTEN_KEEPALIVE
char *test_reg_machine(char *s, char *(*callback)()) {
  auto wrapped_callback = [=]() {
    char *temp = (*callback)();
    const std::string ret{temp};
    free((void *)temp);
    return ret;
  };
  return strdup(run_reg_machine(s, wrapped_callback).c_str());
}
}
#endif

std::string run_reg_machine(const std::string &input,
                            std::function<std::string()> input_callback) {
  std::istringstream is{input};
  std::ostringstream os;
  {
    auto regs = sicp_compiler_registers();
    auto machine = parse(is, regs);
#ifdef __EMSCRIPTEN__
    machine.set_output(os);
#endif
    machine.set_input(input_callback);
    machine.start();

    if (machine.pc.get().as<label_t>().dst != machine.instructions.size()) {
      os << "WARNING: the machine stops in the middle of instructions!"
         << std::endl;
    }
    os << "===============================================" << std::endl;
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
  std::ifstream file{"../example/compiler.rma"};
  std::stringstream buffer;
  buffer << file.rdbuf();
  std::string output = run_reg_machine(buffer.str(), [&]() {
    std::string temp;
    std::getline(std::cin, temp);
    return temp;
  });
  std::cout << output << std::endl;
#endif
  return 0;
}