# Web-Scheme

![license](https://img.shields.io/badge/license-GPL-red)

An implementation of Scheme for WebAssembly.

This project started out as our group project for MIT [6.945](https://groups.csail.mit.edu/mac/users/gjs/6.945/) (Large scale symbolic systems), Spring 2022.

## Running the demo
If you want to run the demo locally,

```
cd demo
npm install
npm run start
```
Navigate to http://localhost.8080/.

Alternatively, visit https://danglingpointer.fun/web-scheme/, which may not catch up with the latest developments (you still need to run the compiler locally in MIT Scheme, until the compiler bootstraps.)

To use the demo, either upload pre-compiled `.rma` files from the `examples` folder or copy and paste in register machine code.

Press the `Run register machine` button to run the code.

## Development and building

### The register machine in C++

The source code for the register machine is under the `reg-machine` directory. `cd` into it and:
* to build to WASM (requires Emscripten installed): 
```bash
emcmake cmake -Bwasm-build . && make -C wasm-build
```
* to build locally: 
```bash
cmake -Bbuild . && make -C build
```

The code is written in C++17 so you need a relatively new compiler. We haven't tested with MSVC. 

### The compiler in Scheme

The compiler is written under [MIT Scheme](https://www.gnu.org/software/mit-scheme/) 11.2. MIT Scheme does not support Windows, so you may need to run it in WSL. MIT students may also consider Athena, which contains MIT Scheme 10 that might work.

The source code is in `compiler/`. To load the compiler, execute from the MIT Scheme REPL:

```scheme
(load ".../web-scheme/compiler/load.scm")
```
If you just want to compile, use `compile-to` defined in `load.scm`. For example, to compile a program to a file named `test.rma`, use
```scheme
(call-with-output-file
    "test.rma"
  (lambda (file)
    (compile-to
     '(letrec ((fact
                (lambda (n)
                  (if (= n 0) 1 (* n (fact (- n 1)))))))
        (fact (input)))
     #f file)))
```

To try compiling the compiler stage itself or more compilicated programs, refer to `example/compile-compiler.scm`.

Familiarity with SICP section 5 is expected to understand the compiler output.

The ultimate plan is to enable the compiler to bootstrap!

## License
This project is licensed under GPL3, mainly because we used the GPL-licensed codebase for the book *Software Design for Flexibility* for generic procedures. A quick and dirty drop-in replacement of SDF generic procedures can be found in `example/compatibility-layer.scm`, so we might remove the dependency and use MIT license in the future.

## Is it any good?

**Yes.**
