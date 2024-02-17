import ita from '@/digests/ita.json'

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

const baseUrl = import.meta.env.PROD ? "https://assets.arkhamhorror.app" : ''

export function imgsrc(src: string) {
  const language = localStorage.getItem('language') || 'en'
  const path = src.replace(/^\//, '')
  switch (language) {
    case 'it': {
      const exists = ita.includes(path)
      return exists ? `${baseUrl}/img/arkham/ita/${src.replace(/^\//, '')}` : `${baseUrl}/img/arkham/${src.replace(/^\//, '')}`
    }
    default: return `${baseUrl}/img/arkham/${src.replace(/^\//, '')}`
  }
}

export function pluralize(w: string, n: number) {
  return `${n} ${w}${n == 1 ? '' : 's'}`
}
