const renderer = require("./Renderer.bs");

if (module.hot) {
    module.hot.accept()
}

renderer.rendererEntry(undefined);