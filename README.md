# web-scheme
An implementation of Scheme for WebAssembly.

This project started out as our group project for MIT [6.945](https://groups.csail.mit.edu/mac/users/gjs/6.945/) (Large scale symbolic systems), Spring 2022.

## Running the demo

```
cd demo
npm install
npm run start
```
Navigate to http://localhost.8080/.

To use the demo, either upload pre-compiled `.rma` files from the `examples` folder or copy and paste in register machine code.

Press the `Run register machine` button to run the code.

## Development and building

### The register machine in C++

The source files for the register machine is under the `reg-machine` directory. `cd` into it and:
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

The compiler is written under [MIT Scheme](https://www.gnu.org/software/mit-scheme/) 11.2. The source codes are in `compiler/`. To load the compiler, execute from the MIT Scheme REPL:

```scheme
(load ".../web-scheme/compiler/load.scm")
```

The ultimate plan is to enable to compiler to bootstrap!

## Is it any good?

**Yes.**
