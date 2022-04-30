cmake_minimum_required(VERSION 3.14)
project(web_scheme CXX)
set(CMAKE_CXX_STANDARD 17)
add_executable(web_scheme
    compiled-proc.cpp env.cpp gc.cpp instr.cpp machine.cpp op.cpp 
    types.cpp parser.cpp main.cpp primitive-procs.cpp
)

if(EMSCRIPTEN)
    target_link_options(web_scheme PRIVATE
        -sMODULARIZE -sWASM=1 -sEXPORTED_RUNTIME_METHODS=ccall,cwrap,addFunction,allocateUTF8
        -sENVIRONMENT=web -sEXPORT_NAME="wasmInterface" -sALLOW_TABLE_GROWTH -O3)
    add_custom_command(TARGET web_scheme POST_BUILD
        COMMAND "${CMAKE_COMMAND}" -E copy 
            "$<TARGET_FILE_DIR:web_scheme>/web_scheme.js"
            "${CMAKE_SOURCE_DIR}/../demo/src/wasm/web_scheme.js"
    )
    add_custom_command(TARGET web_scheme POST_BUILD
        COMMAND "${CMAKE_COMMAND}" -E copy 
            "$<TARGET_FILE_DIR:web_scheme>/web_scheme.wasm"
            "${CMAKE_SOURCE_DIR}/../demo/src/wasm/web_scheme.wasm"
    )
endif()