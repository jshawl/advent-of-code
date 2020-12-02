const input = require("fs")
  .readFileSync("./input.txt", "utf-8")
  .split("\n")
  .map(Number);

// Part 1
const findTwoEntries = (arr, i = 0, sum = 2020) => {
  const second = arr.find((el) => el + arr[i] === sum);
  return i === arr.length
    ? []
    : second
    ? [arr[i], second]
    : findTwoEntries(arr, ++i, sum);
};
const entries = findTwoEntries(input);
console.log(entries[0] * entries[1]);

// Part 2
const findThreeEntries = (arr, i = 0, sum = 2020) => {
  const [second, third] = findTwoEntries(arr, i, sum - arr[i]);
  return second ? [arr[i], second, third] : findThreeEntries(arr, ++i);
};
const threeEntries = findThreeEntries(input);
console.log(threeEntries[0] * threeEntries[1] * threeEntries[2]);
