export function toCapitalizedWords(name: string) {
  const words = name.match(/[A-Za-z][a-z']*/g) || [];
  return capitalize(words.map(lowercase).join(" "));
}

function capitalize(word: string) {
  return word.charAt(0).toUpperCase() + word.substring(1);
}

function lowercase(word: string) {
  return word.charAt(0).toLowerCase() + word.substring(1);
}
