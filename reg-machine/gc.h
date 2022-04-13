#pragma once

#include <vector>

class garbage_collected_t {
 public:
  garbage_collected_t();

  void mark();

  virtual void mark_children() = 0;

 private:
  bool marked;
};

class garbage_collector_t {
 public:
  garbage_collector_t();

  template <typename InputIt>
  void collect(InputIt begin, InputIt end);

 private:
  friend garbage_collected_t::garbage_collected_t();
  void add_gc_object(garbage_collected_t*);

 private:
  std::vector<garbage_collected_t*> heap;
};

extern garbage_collector_t garbage_collector;
