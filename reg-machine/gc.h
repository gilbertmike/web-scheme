#pragma once

#include <vector>

class garbage_collected_t {
 public:
  garbage_collected_t();

  virtual ~garbage_collected_t();

  void mark();
  void unmark();
  bool is_marked() const;

  virtual void mark_children() = 0;

 private:
  bool marked;
};

class garbage_collector_t {
 public:
  garbage_collector_t();

  void collect(std::vector<garbage_collected_t*>& roots);
  void track(garbage_collected_t*);
  // VERY SLOW! DON'T CALL IF YOU HAVE TO.
  void untrack(garbage_collected_t*);

  int current_size() const;

 private:
  friend garbage_collected_t::garbage_collected_t();

 private:
  std::vector<garbage_collected_t*> heap;
};

extern garbage_collector_t garbage_collector;
