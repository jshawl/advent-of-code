const input = require("fs").readFileSync("./input.txt", "utf-8").split("\n");

const trees = ([x, y = 1]) =>
  input.filter((e, i) => (input[i * y] || "")[(i * x) % e.length] === "#")
    .length;

// Part 1
console.log(trees([3]));

// Part 2
console.log([[1], [3], [5], [7], [1, 2]].map(trees).reduce((a, b) => a * b, 1));
