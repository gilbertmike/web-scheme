#include <assert.h>

#include <fstream>

#include "machine.h"
#include "parser.h"
#include "sicp.h"

const std::string TEST_RMA_FILE = "fact.rma";

void test_reg_machine() {
  std::ifstream test_f(TEST_RMA_FILE);
  auto regs = sicp_compiler_registers();

  auto machine = parse(test_f, regs);

  machine.start();

  assert(machine.pc.get().as<label_t>().dst == machine.instructions.size());
}

int main(int argc, char* argv[]) {
  test_reg_machine();
  return 0;
}