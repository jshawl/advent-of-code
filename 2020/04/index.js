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
// Part 2
const rules = {
  byr: (d) => 1920 <= Number(d) && Number(d) <= 2002,
  iyr: (d) => 2010 <= Number(d) && Number(d) <= 2020,
  eyr: (d) => 2020 <= Number(d) && Number(d) <= 2030,
  hgt: (d) => {
    const [, value, unit] = (d || "").match(/([0-9]+)(cm|in)/) || [];
    if (!value || !unit) return false;
    if (unit === "cm") {
      return 150 <= value <= 193;
    } else {
      return 59 <= value <= 76;
    }
  },
  hcl: (d) => /#[0-9a-f]{6}/.test(d),
  ecl: (d) => ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].includes(d),
  pid: (d) => /[0-9]{9}/.test(d),
};

const isValid = (p) => {
  const out = Object.keys(rules).every((rule) => {
    const v = p[rule] && rules[rule](p[rule]);

    return v;
  });
  if (out) {
    console.log("Valid", Object.keys(p).length);
  }
  return out;
};

console.log(input.filter(isValid).length);
