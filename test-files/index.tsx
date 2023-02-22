import * as React from "react"
import { renderToStaticMarkup } from "react-dom/server"

const App: React.FunctionComponent = () => <div>The app</div>

process.stdout.write(renderToStaticMarkup(<App />))
