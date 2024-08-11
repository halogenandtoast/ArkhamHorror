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

export function replaceIcons(body: string) {
  return body.
    replace(/{action}/g, '<span class="action-icon"></span>').
    replace(/{fast}/g, '<span class="fast-icon"></span>').
    replace(/{willpower}/g, '<span class="willpower-icon"></span>').
    replace(/{intellect}/g, '<span class="intellect-icon"></span>').
    replace(/{combat}/g, '<span class="combat-icon"></span>').
    replace(/{agility}/g, '<span class="agility-icon"></span>').
    replace(/{wild}/g, '<span class="wild-icon"></span>').
    replace(/{guardian}/g, '<span class="guardian-icon"></span>').
    replace(/{seeker}/g, '<span class="seeker-icon"></span>').
    replace(/{rogue}/g, '<span class="rogue-icon"></span>').
    replace(/{mystic}/g, '<span class="mystic-icon"></span>').
    replace(/{survivor}/g, '<span class="survivor-icon"></span>').
    replace(/{elderSign}/g, '<span class="elder-sign"></span>').
    replace(/{skull}/g, '<span class="skull-icon"></span>').
    replace(/{curse}/g, '<span class="curse-icon"></span>').
    replace(/{bless}/g, '<span class="bless-icon"></span>')
}

export function investigatorClass(code: string) {
    switch (code) {
      case "01001": return { guardian : true }
      case "01002": return { seeker : true }
      case "01003": return { rogue : true }
      case "01004": return { mystic : true }
      case "01005": return { survivor : true }
      case "01501": return { guardian : true }
      case "01502": return { seeker : true }
      case "01503": return { rogue : true }
      case "01504": return { mystic : true }
      case "01505": return { survivor : true }
      case "02001": return { guardian : true }
      case "02002": return { seeker : true }
      case "02003": return { rogue : true }
      case "02004": return { mystic : true }
      case "02005": return { survivor : true }
      case "03001": return { guardian : true }
      case "03002": return { seeker : true }
      case "03003": return { rogue : true }
      case "03004": return { mystic : true }
      case "03005": return { survivor : true }
      case "03006": return { neutral : true }
      case "04001": return { guardian : true }
      case "04002": return { seeker : true }
      case "04003": return { rogue : true }
      case "04004": return { mystic : true }
      case "04005": return { survivor : true }
      case "05001": return { guardian : true }
      case "05002": return { seeker : true }
      case "05003": return { rogue : true }
      case "05004": return { mystic : true }
      case "05005": return { survivor : true }
      case "05006": return { mystic : true }
      case "06001": return { guardian : true }
      case "06002": return { seeker : true }
      case "06003": return { rogue : true }
      case "06004": return { mystic : true }
      case "06005": return { survivor : true }
      case "07001": return { guardian : true }
      case "07002": return { seeker : true }
      case "07003": return { rogue : true }
      case "07004": return { mystic : true }
      case "07005": return { survivor : true }
      case "08001": return { guardian : true }
      case "08004": return { seeker : true }
      case "08007": return { rogue : true }
      case "08010": return { mystic : true }
      case "08016": return { survivor : true }
      case "09001": return { guardian : true }
      case "09004": return { seeker : true }
      case "09008": return { rogue : true }
      case "09011": return { mystic : true }
      case "09015": return { survivor : true }
      case "09018": return { neutral : true }
      case "10001": return { guardian : true }
      case "10004": return { seeker : true }
      case "10009": return { rogue : true }
      case "10012": return { mystic : true }
      case "10015": return { survivor : true }
      case "60101": return { guardian : true }
      case "60201": return { seeker : true }
      case "60301": return { rogue : true }
      case "60401": return { mystic : true }
      case "60501": return { survivor : true }
      case "89001": return { neutral : true }
      case "98001": return { rogue : true }
      case "98004": return { guardian : true }
      case "98007": return { seeker : true }
      case "98010": return { guardian : true }
      case "98013": return { survivor : true }
      case "98016": return { mystic : true }
      case "98019": return { mystic : true }
      case "99001": return { mystic : true }
      case "90001": return { seeker : true }
      case "90008": return { rogue : true }
      case "90017": return { mystic : true }
      case "90024": return { guardian : true }
      case "90037": return { survivor : true }
      case "90046": return { survivor : true }
      case "90049": return { mystic : true }
      case "90059": return { guardian : true }
      case "90062": return { rogue : true }
      case "90078": return { seeker : true }
      case "90084": return { rogue : true }
      default: return {}
    }
}
