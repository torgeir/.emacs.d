import * as React from "react"
import { renderToStaticMarkup } from "react-dom/server"

//TODO try this
//(setq +tree-sitter-hl-enabled-modes '(not web-mode typescript-tsx-mode))

const App: React.FunctionComponent = () => <div>The app</div>

process.stdout.write(renderToStaticMarkup(<App />))
