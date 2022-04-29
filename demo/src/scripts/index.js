import '../styles/index.scss';
import { wasmInterface }  from './web_scheme.js';
import wasmBytecode from './web_scheme.wasm';
var Promise = require('es6-promise').Promise;

const fileInput = document.getElementById('fileInput');
const uploadBtn = document.getElementById('upload');
const runBtn = document.getElementById('run');
const iTextBox = document.getElementById('input');

function readFileContent(file) {
  const reader = new FileReader();
  return new Promise((resolve, reject) => {
    reader.onload = event => resolve(event.target.result);
    reader.onerror = error => reject(error);
    reader.readAsText(file);
  });
}

// TODO doesn't work yet, TypeError fileInput is null
uploadBtn.addEventListener('click', function(event) {
  readFileContent(fileInput.files[0]).then(content => {
    iTextBox.value = content;
  }).catch(error => console.log(error));
});

wasmInterface({
  locateFile(path) {
    return path.endsWith('.wasm') ? wasmBytecode : path;
  },
}).then(module => {
    runBtn.addEventListener('click', function(event) {
      // TODO save the output to the output text box instead of just printing to console
      // This requires a change in main.cpp
      module.ccall('test_reg_machine', 'none', ['string'], [iTextBox.value]);
    });
});
