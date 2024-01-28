import * as React from "react"

import { renderToStaticMarkup } from "react-dom/server"

const fn = () => 4
var fns = fn() + 3

var a = +"a"
var b: number = a + 3
console.log(b)

// (setq +tree-sitter-hl-enabled-modes '(not web-mode typescript-tsx-mode))
function App(): React.JSX.Element {
  var b = (
    <div>
      <p>wow</p>
      <span></span>
      asdf<span>s</span>
    </div>
  )
  return <div>The app${b}</div>
}

process.stdout.write(renderToStaticMarkup(<App />))
