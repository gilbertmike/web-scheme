import '../styles/index.scss';
import wasmBytecode from '../wasm/web_scheme.wasm';
import { wasmInterface } from '../wasm/web_scheme.js';
import { Promise } from 'es6-promise';

const fileInput = document.getElementById('file-input');
const runBtn = document.getElementById('run');
const iTextBox = document.getElementById('input');

function readFileContent(file) {
  const reader = new FileReader();
  return new Promise((resolve, reject) => {
    reader.onload = event => resolve(event.target.result);
    reader.onerror = error => reject(error);
    reader.readAsText(file);
    console.log("Uploaded " + file.name);
  });
}

fileInput.onchange = () => {
  readFileContent(fileInput.files[0]).then(content => {
    iTextBox.value = content;
  }).catch(error => console.log(error));
};

wasmInterface({
  locateFile(path) {
    return path.endsWith('.wasm') ? wasmBytecode : path;
  },
}).then(module => {
  runBtn.addEventListener('click', function (event) {
    // TODO save the output to the output text box instead of just printing to console
    // This requires a change in main.cpp
    module.ccall('test_reg_machine', 'none', ['string'], [iTextBox.value]);
  });
});
