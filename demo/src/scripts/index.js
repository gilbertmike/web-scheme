import '../styles/index.scss';
import wasmBytecode from '../wasm/web_scheme.wasm';
import { wasmInterface } from '../wasm/web_scheme.js';
import { Promise } from 'es6-promise';

const fileInput = document.getElementById('file-input');
const runBtn = document.getElementById('run');
const iTextBox = document.getElementById('input');
const oTextBox = document.getElementById('output');

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
  const inputCallback = module.addFunction(() => {
    let input = prompt("Input to the program:");
    return module.allocateUTF8(input);
  }, 'i');
  runBtn.addEventListener('click', function (event) {
    oTextBox.value = module.ccall('test_reg_machine', 'string', ['string', 'number'], [iTextBox.value, inputCallback]);
  });
});
