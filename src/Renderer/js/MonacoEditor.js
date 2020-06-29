
const monaco = require("monaco-editor/esm/vs/editor/editor.api")

MonacoEnvironment = {
	getWorkerUrl: function (moduleId, label) {
		return './editor.worker.js';
	}
}

customElements.define("monaco-editor", class extends HTMLElement {
    constructor() {
        super();
        Object.defineProperty(this, "_editorValue", {
            set: function(value) {
                this._maybeInitialValue = value;
                if (!this._editor) return;
                this._editor.setValue(value);
            }
        })

        this.breakpoints = {}
        this.decorations = []
    }

    connectedCallback() {
        let self = this;

        monaco.editor.setTheme("vs-dark");
		this._editor = monaco.editor.create(this, {
			value: this._maybeInitialValue !== undefined ? this._maybeInitialValue : "<waiting>",
            language: 'javascript',
            glyphMargin: true,
            lineNumbersMinChars: 2,
            automaticLayout: true,
        });

        let editor = this._editor;


        editor.onDidChangeModelContent(_ => {
            this.dispatchEvent(new CustomEvent("editorValueChanged"))
        })
        
        let breakpoint = (line) => (
            {
                range: new monaco.Range(line, 1, line, 1),
                options: {
                    glyphMarginClassName: "breakpoint"
                }
            }
        )

        editor.onMouseDown(function(e) {
            if (e.target.type == monaco.editor.MouseTargetType.GUTTER_GLYPH_MARGIN) {
                let line = e.target.position.lineNumber;

                if (self.breakpoints[line]) {
                    delete self.breakpoints[line]
                } else {
                    self.breakpoints[line] = breakpoint(line)
                }

                let newDecorations = Object.keys(self.breakpoints).map((v) => {
                    return self.breakpoints[v]
                });

                self.decorations = editor.deltaDecorations(self.decorations, newDecorations)
                self.dispatchEvent(new CustomEvent("breakpointsChanged"))
            }
        })
    }
})