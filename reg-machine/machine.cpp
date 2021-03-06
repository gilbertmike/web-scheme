#include "machine.h"

#include <iostream>
#include <mutex>
#include <optional>
#include <stack>

#include "types.h"

reg_t::reg_t() : value(unassigned_t()) {}

reg_t::reg_t(const value_t& value) : value(value) {}

void reg_t::set(const value_t& new_value) { value = new_value; }

value_t reg_t::get() const { return value; }

void reg_t::set_name(const std::string& name) { this->name = name; }

std::string_view reg_t::get_name() const { return name; }

stack_t::stack_t() {}

void stack_t::push(const value_t& value) { stack.emplace(value); }

bool stack_t::empty() const { return stack.empty(); }

value_t stack_t::pop() {
  auto top = stack.top();
  stack.pop();
  return top;
}

machine_t::machine_t(int rfile_size)
    : pc(label_t{0}),
      flag(false),
      instructions(),
      rfile(rfile_size),
      output(&std::cout),
      input(nullptr) {}

machine_t::machine_t(int rfile_size, std::vector<instr_t::u_ptr>&& instructions)
    : pc(label_t{0}),
      flag(false),
      instructions(std::move(instructions)),
      rfile(rfile_size),
      output(&std::cout),
      input(nullptr) {}

void machine_t::start() {
  while (true) {
    uintptr_t cur_instr_idx = pc.get().as<label_t>().dst;
    if (cur_instr_idx >= instructions.size()) {
      break;
    }

    pc.set(label_t{cur_instr_idx + 1});
    set_current();
    instructions.at(cur_instr_idx)->execute(*this);
    yield_current();
    if (gc.current_size() > 2000) {
      int old_size = gc.current_size();
      do_gc();
      std::cout << "Performed GC, heap size " << old_size << " -> "
                << gc.current_size() << std::endl;
    }
  }
}

void machine_t::do_gc() {
  std::vector<value_t> temp;
  std::vector<garbage_collected_t*> roots;
  while (!stack.empty()) {
    value_t top = stack.pop();
    if (auto gc = top.as_garbage_collected(); gc) roots.push_back(gc);
    temp.push_back(top);
  }
  while (!temp.empty()) {
    stack.push(temp.back());
    temp.pop_back();
  }
  for (const auto& reg : rfile) {
    if (auto gc = reg.get().as_garbage_collected(); gc) {
      roots.push_back(gc);
    }
  }
  gc.collect(roots);
}

static machine_t* running = nullptr;
static std::mutex gil;

bool machine_t::any_running() { return running; }

machine_t& machine_t::current() {
  assert(running);
  return *running;
}

void machine_t::set_current() {
  gil.lock();
  running = this;
}

void machine_t::yield_current() {
  assert(this == running);
  running = nullptr;
  gil.unlock();
}