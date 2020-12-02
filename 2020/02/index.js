const input = require("fs").readFileSync("./input.txt", "utf-8").split("\n");

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

const isPasswordValid = ({ password, upper, lower, letter }) => {
  const occurrences = (password.match(new RegExp(letter, "g")) || []).length;
  return occurrences >= lower && occurrences <= upper;
};

const validPasswords = input.map(lineParser).filter(isPasswordValid);

console.log(validPasswords.length);
