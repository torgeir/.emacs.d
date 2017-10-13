// @flow

let arrs: Array<string> = ["A", "B", "C"];
let arrm: Array<mixed> = [1, true, "three"]
let arrb: Array<boolean> = [true, false, true];

function filter<T> (as: Array<T>, p: (T) => boolean): Array<T> {
  return as.filter(p);
}

function every (as: Array<boolean>): boolean {
  let every = true;
  as.forEach(a => every = every && a);
  return every;
}

let v = every(arrb);
v.valueOf() // boolean

const [first] = filter([1, 2, 3], n => n > 2);
first.valueOf() // number

const [firstS] = filter(["1", "2", "3"], n => n.length == 1);
firstS.valueOf() // string