// @flow

type Action = "Click" | "Another";

function doit (a: Action): number {
  switch(a) {
    case "Click": return 1;
  }
  return 1;
}