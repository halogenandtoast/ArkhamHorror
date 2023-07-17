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

const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : ''

export function imgsrc(src: string) {
  return `${baseUrl}/img/arkham/${src.replace(/^\//, '')}`
}
