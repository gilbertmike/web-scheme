SRCS = env.cpp gc.cpp instr.cpp machine.cpp op.cpp types.cpp parser.cpp main.cpp primitive-procs.cpp
HDRS = env.h gc.h instr.h machine.h op.h types.h parser.h primitive-procs.h

OBJS = $(SRCS:.cpp=.o)
WASM_OBJS = $(SRCS:.cpp=.owasm)

CXXFLAGS = -std=c++17

all: $(OBJS)
	$(CXX) $(CXXFLAGS) $(OBJS) -o reg-machine.out

%.o: %.cpp
	$(CXX) -c $(CXXFLAGS) $< -o $@

clean:
	rm *.o *.out

all-wasm: $(WASM_OBJS)
	emcc $(CXXFLAGS) $(WASM_OBJS) -o reg-machine.outwasm

%.owasm: %.cpp
	emcc -c $(CXXFLAGS) $< -o $@

clean-wasm:
	rm *.owasm *.outwasm
