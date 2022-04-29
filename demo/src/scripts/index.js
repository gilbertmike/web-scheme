import '../styles/index.scss';
import { wasmInterface }  from './web_scheme.js';
import wasmBytecode from './web_scheme.wasm';

wasmInterface({
  locateFile(path) {
    return path.endsWith('.wasm') ? wasmBytecode : path;
  },
}).then(module => {
    console.log(module);
});
