const input = require("fs")
  .readFileSync("./input.txt", "utf-8")
  .split("\n\n")
  .map((line) =>
    line
      .replace(/\n/g, " ")
      .split(" ")
      .reduce((acc, el) => {
        const [key, value] = el.split(":");
        return { ...acc, [key]: value };
      }, {})
  );

const validPassport = (p) =>
  ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"].every((key) => key in p);

console.log(input.filter(validPassport).length);
