console.log("works")

console.clear()

const str = Deno.readFileSync("deno.json")
const res = Deno.inspect(str)
console.log(res)

Deno.exit(1)
