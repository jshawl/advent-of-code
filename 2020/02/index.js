const lineParser = (line) => {
  const [, lower, upper, letter, password] = line.match(
    /([0-9]+)-([0-9]+)\ ([a-z]):\ ([a-z]+)/
  );
  return {
    lower,
    upper,
    letter,
    password,
  };
};
const input = require("fs")
  .readFileSync("./input.txt", "utf-8")
  .split("\n")
  .map(lineParser);

// Part 1
const isPasswordValid = ({ password, upper, lower, letter }) => {
  const occurrences = (password.match(new RegExp(letter, "g")) || []).length;
  return occurrences >= lower && occurrences <= upper;
};
const validPasswords = input.filter(isPasswordValid);
console.log(validPasswords.length);

// Part 2
const isPasswordValidPart2 = ({ password, upper, lower, letter }) =>
  (password[lower - 1] === letter) ^ (password[upper - 1] === letter);
const validPasswordsPart2 = input.filter(isPasswordValidPart2);
console.log(validPasswordsPart2.length);
