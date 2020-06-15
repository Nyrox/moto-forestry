var path = require("path");

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

module.exports = {
  entry: [
    resolve("lib/js/src/Renderer/index.js"),
    resolve("src/Renderer/js/index.js"),
    resolve("src/Renderer/scss/main.scss"),
    "monaco-editor/esm/vs/editor/editor.worker.js",
  ],
  output: {
    filename: "[name].js"
  }
}
