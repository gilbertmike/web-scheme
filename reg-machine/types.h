#include <cstdint>

// objects in Scheme
enum type_t { UNASSIGNED, INTEGER, PAIR, BOOLEAN };

struct object_t {
  typedef object_t* const cptr;

  type_t type;

  union {
    int64_t integer;
    pair_t* pair;
    bool boolean;
  };
};

// variables contain pointer to an object
typedef object_t* value_t;

struct pair_t {
  object_t* car;
  pair_t* cdr;
};

extern object_t* unassgined;
