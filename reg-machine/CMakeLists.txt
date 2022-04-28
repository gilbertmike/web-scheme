cmake_minimum_required(VERSION 3.14)
project(web_scheme CXX)
set(CMAKE_CXX_STANDARD 17)
add_executable(web_scheme
    compiled-proc.cpp env.cpp gc.cpp instr.cpp machine.cpp op.cpp 
    types.cpp parser.cpp main.cpp primitive-procs.cpp
)