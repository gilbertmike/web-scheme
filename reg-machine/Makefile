SRCS = gc.cpp instr.cpp machine.cpp op.cpp types.cpp parser.cpp main.cpp
HDRS = gc.h instr.h machine.h op.h types.h parser.h

OBJS = $(SRCS:.cpp=.o)

CXXFLAGS = -std=c++17

all: $(OBJS)
	$(CXX) $(CXXFLAGS) $(OBJS) -o reg-machine.out

%.o: %.c
	$(CXX) -c $(CXXFLAGS) $< -o $@

clean:
	rm *.o *.out
