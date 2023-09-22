import init, { WebHandle } from "./pkg/commutative_diagrams_engine_wasm.js";
init().then(() => {
  const allCanvas = document.querySelectorAll("body > canvas.commdiag");
  allCanvas.forEach(canvas => {
    var handle = new WebHandle();
    handle.start(canvas.id);
  });
})
