console.log("works");

console.clear();

const str = Deno.readFileSync("deno.json");
const res = Deno.inspect(str);
console.log(res);

export function add(a: number, b: number) {
  return a + b;
}

const a = 1;

console.log(b);

Deno.exit(1);
