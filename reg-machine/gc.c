#include "gc.h"

garbage_collected_t::garbage_collected_t() : marked(false) {
  garbage_collector.add_gc_object(this);
}

void garbage_collected_t::mark() {
  marked = true;
  mark_children();
}

garbage_collector_t::garbage_collector_t() : heap() {}

template <typename InputIt>
void garbage_collector_t::collect(InputIt begin, InputIt end) {
  // mark
  for (auto root_it = begin; root_it != end; ++root_it) {
    (*root_it)->mark();
  }

  // sweep
  std::vector<garbage_collected_t*> new_heap;
  for (auto it = heap.begin(); it != heap.end(); ++it) {
    garbage_collected_t* obj = *it;
    if (obj->marked) {
      obj->marked = false;
      new_heap.push_back(obj);
    } else {
      delete obj;
    }
  }
  heap = std::move(new_heap);
}

void garbage_collector_t::add_gc_object(garbage_collected_t* obj) {
  heap.push_back(obj);
}

garbage_collector_t garbage_collector;
