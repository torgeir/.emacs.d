function withName () {}
var {a, b} = {};

(function () {

  var o = {
    what: 1,
    whatt: 2,
    whattt: 3
  };

  function sum (ls, acc = 0) {
    if (ls.length == 0) {
      return acc;
    }
    var first = ls[0];
    return sum(ls.slice(1), acc + first);
  }

  var e = new EventEmitter();
  var a = 1;
})();
