/**
 * @file compiled-proc.cpp
 * @author Michael Gilbert (gilbertm@mit.edu)
 * @brief Compiled procedure implementation
 * @version 0.1
 * @date 2022-04-24
 *
 * @copyright Copyright (c) 2022
 *
 */
#include "compiled-proc.h"

compiled_procedure_t::compiled_procedure_t(const label_t& entry, env_t* env)
    : entry(entry), env(env) {}
