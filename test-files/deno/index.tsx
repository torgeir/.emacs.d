import * as React from "npm:react"
import * as React2 from "npm:react"
import { renderToStaticMarkup } from "npm:react-dom/server"

function App(): React.JSX.Element {
  return <div>A div</div>
}

const str = renderToStaticMarkup(<App />)

const res = Deno.inspect(str)
console.log(res)

Deno.exit(1)
