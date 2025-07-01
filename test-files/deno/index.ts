console.log("works")

console.clear()

const str = Deno.readFileSync("deno.json")
const res = Deno.inspect(str)
console.log(res)

const a = 1

console.log(a)

Deno.exit(1)
