#include "gc.h"

#include <iostream>

#include "machine.h"

garbage_collected_t::garbage_collected_t() : marked(false) {
  if (machine_t::any_running()) {
    machine_t::current().gc.track(this);
  } else {
    garbage_collector.track(this);
  }
}

garbage_collected_t::~garbage_collected_t() {}

void garbage_collected_t::mark() {
  if (marked) return;
  marked = true;
  mark_children();
}

void garbage_collected_t::unmark() { marked = false; }

bool garbage_collected_t::is_marked() const { return marked; }

garbage_collector_t::garbage_collector_t() : heap() {}

int garbage_collector_t::current_size() const {
  return heap.size();
}

void garbage_collector_t::collect(std::vector<garbage_collected_t*>& roots) {
  for (auto& root : roots) root->mark();
  // Sweep
  std::vector<garbage_collected_t*> new_heap;
  for (auto it = heap.begin(); it != heap.end(); ++it) {
    garbage_collected_t* obj = *it;
    if (obj->is_marked()) {
      obj->unmark();
      new_heap.push_back(obj);
    } else {
      delete obj;
    }
  }
  for (auto& root : roots) root->unmark();
  heap = std::move(new_heap);
}

void garbage_collector_t::track(garbage_collected_t* obj) {
  heap.push_back(obj);
}

void garbage_collector_t::untrack(garbage_collected_t* obj) {
  if (auto it = std::find(heap.cbegin(), heap.cend(), obj); it != heap.cend()) {
    heap.erase(it);
  }
}

garbage_collector_t garbage_collector;
