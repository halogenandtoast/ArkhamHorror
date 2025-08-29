import ita from '@/digests/ita.json'
import es from '@/digests/es.json'
import fr from '@/digests/fr.json'
import ko from '@/digests/ko.json'
import zh from '@/digests/zh.json'

import { useSiteSettingsStore } from '@/stores/site_settings'
import { ref, type Ref } from 'vue';

interface ImageHelper {
  root: string
  digests: Set<string>
  data: Map<string, Ref<boolean>>
  loaded: Ref<boolean>
}

const batchSize: number = 1000
const defaultHelper: ImageHelper = { root: '', digests: new Set(), data: new Map(), loaded: ref(true) }
const imgHelper:Map<string, ImageHelper> = new Map<string, ImageHelper>([
  ['it', { root: 'ita', digests: new Set(ita), data: new Map(), loaded: ref(false) }],
  ['fr', { root: 'fr', digests: new Set(fr), data: new Map(), loaded: ref(false) }],
  ['es', { root: 'es', digests: new Set(es), data: new Map(), loaded: ref(false) }],
  ['ko', { root: 'ko', digests: new Set(ko), data: new Map(), loaded: ref(false) }],
  ['zh', { root: 'zh', digests: new Set(zh), data: new Map(), loaded: ref(false) }]
])

export async function checkImageExists(language: string = localStorage.getItem('language') || 'en') {
  if (language === 'en' || !imgHelper.has(language)) return
  
  const helper = imgHelper.get(language) || defaultHelper
  if (!helper.root || helper.loaded.value) return
  
  const store = useSiteSettingsStore()
  const imgList = helper.digests
  const tasks: {path: string, ext: string}[] = []
  
  imgList.forEach((originPath) => {
    const path = originPath.replace(/^\//, '')
    const ext = (path.split('.').pop() || '').toLowerCase()
    
    if (ext !== 'avif' && ext !== 'webp' && !ext.endsWith('png') && !ext.startsWith('jpg')) return
    tasks.push({path, ext})
  })
  
  const runBatch = async () => {
    const batch = tasks.splice(0, batchSize)
    
    await Promise.allSettled(
      batch.map(async ({path, ext}) => {
        const i18nPath = `${store.assetHost}/img/arkham/${helper.root}/${path}`
        
        try {
          const res = await fetch(i18nPath, { method: 'HEAD' })
          const contentType = res.headers.get('Content-Type') || ''
          const isOkay = res.ok && contentType !== 'text/html' && (contentType === 'text/plain' || (ext ? contentType === `image/${ext}` : false))
          helper.data.set(path, ref(isOkay))
        } catch {
          helper.data.set(path, ref(false))
        }
      })
    ).then((results) => {
      results.forEach((result, index) => {
        if (result.status === 'rejected')
          tasks.push(batch[index])
      })
    })
    .finally(async () => {
      if (tasks.length > 0) await runBatch()
      else helper.loaded.value = true
    })
  }
  
  await runBatch()
}

export function toCapitalizedWords(name: string) {
  const words = name.match(/[A-Z]?[a-z']+|[A-Z]/g) || [];
  return capitalize(words.map(lowercase).join(" "));
}

export function toCamelCase(str : string) {
  return str
    .toLowerCase()
    .replace(/\s+(\w)/g, (_, letter) => letter.toUpperCase())
    .replace(/\s+/g, '');
}

export function capitalize(word: string) {
  return word.charAt(0).toUpperCase() + word.substring(1);
}

export function lowercase(word: string) {
  return word.charAt(0).toLowerCase() + word.substring(1);
}

export const baseUrl = import.meta.env.PROD ? "https://assets.arkhamhorror.app" : ''

export function isLocalized(src: string) {
  const language = localStorage.getItem('language') || 'en'
  // Always return true for English since all base images are English
  if (language === 'en') return true

  const helper = imgHelper.get(language) || defaultHelper
  const path = src.replace(/^\//, '')
  const exists = helper.digests.has(path)

  if (exists && helper.root && helper.loaded.value) {
    const canFetch = helper.data.get(path).value || false
    if (canFetch) return true
  }

  return false
}

export function imgsrc(src: string) {
  const store = useSiteSettingsStore()
  const language = localStorage.getItem('language') || 'en'
  const path = src.replace(/^\//, '')
  const fullPath = `${store.assetHost}/img/arkham/${path}`
  
  if (isLocalized(src)) {
    const helper = imgHelper.get(language) || defaultHelper
    const exists = helper.digests.has(path)
    
    if (exists && helper.root && helper.loaded.value) {
      const i18nFullPath = `${store.assetHost}/img/arkham/${helper.root}/${path}`
      const canFetch = helper.data.get(path).value || false
      
      if (canFetch) return i18nFullPath
    }
  }
  
  return fullPath
}

export function pluralize(w: string, n: number) {
  const language = localStorage.getItem('language') || 'en'
  switch (language) {
    case 'ko': {
      return `${w} ${n}`
    }
    case 'zh': {
      return `${n}${w}${n == 1 ? '' : ''}`
    }
    default: return `${n} ${w}${n == 1 ? '' : 's'}`
  }
}

export function formatContent(body:string) {
  return replaceIcons(body).replace(/_([^_]*)_/g, '<strong>$1</strong>').replace(/\*([^\*]*)\*/g, '<i>$1</i>')

}

export function replaceIcons(body: string) {
  return body.
    replace(/{action}/g, '<span class="action-icon"></span>').
    replace(/{fast}/g, '<span class="fast-icon"></span>').
    replace(/{reaction}/g, '<span class="reaction-icon"></span>').
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
    replace(/{autoFail}/g, '<span class="auto-fail"></span>').
    replace(/{skull}/g, '<span class="skull-icon"></span>').
    replace(/{cultist}/g, '<span class="cultist-icon"></span>').
    replace(/{tablet}/g, '<span class="tablet-icon"></span>').
    replace(/{elderThing}/g, '<span class="elder-thing-icon"></span>').
    replace(/{bless}/g, '<span class="bless-icon"></span>').
    replace(/{curse}/g, '<span class="curse-icon"></span>').
    replace(/{frost}/g, '<span class="frost-icon"></span>').
    replace(/{sealA}/g, '<span class="seal-a-icon"></span>').
    replace(/{sealB}/g, '<span class="seal-b-icon"></span>').
    replace(/{sealC}/g, '<span class="seal-c-icon"></span>').
    replace(/{sealD}/g, '<span class="seal-d-icon"></span>').
    replace(/{sealE}/g, '<span class="seal-e-icon"></span>').
    replace(/{perPlayer}/g, '<span class="per-player"></span>')
}

type InvestigatorClass =
  | "guardian"
  | "seeker"
  | "rogue"
  | "mystic"
  | "survivor"
  | "neutral"

type CssClassFlags = Partial<Record<InvestigatorClass, true>>

const CLASS_TO_CODES: Record<InvestigatorClass, Set<string>> = {
  guardian: new Set([
    "01001", "01501", "02001", "03001", "04001", "05001", "06001", "07001",
    "08001", "09001", "10001", "11001", "60101", "90024", "90059", "98004",
    "98010",
  ]),
  seeker: new Set([
    "01002", "01502", "02002", "03002", "04002", "05002", "06002", "07002",
    "08004", "09004", "10004", "11004", "11007", "60201", "90001", "90078",
    "98007",
  ]),
  rogue: new Set([
    "01003", "01503", "02003", "03003", "04003", "05003", "06003", "07003",
    "08007", "09008", "10009", "11011", "60301", "90008", "90062", "90084",
    "98001",
  ]),
  mystic: new Set([
    "01004", "01504", "02004", "03004", "04004", "05004", "05006", "06004",
    "07004", "08010", "09011", "10012", "11008", "11014", "60401", "90017",
    "90049", "98016", "98019", "99001",
  ]),
  survivor: new Set([
    "01005", "01505", "02005", "03005", "04005", "05005", "06005", "07005",
    "08016", "09015", "10015", "11017", "60501", "90037", "90046", "98013",
  ]),
  neutral: new Set([
    "03006", "04244", "05046", "05047", "05048", "05049", "09018", "89001",
  ]),
}

export function investigatorClass(code: string): CssClassFlags {
  const flags: CssClassFlags = {}
  for (const cls of Object.keys(CLASS_TO_CODES) as InvestigatorClass[]) {
    if (CLASS_TO_CODES[cls].has(code)) {
      flags[cls] = true
    }
  }
  return flags
}

export function waitForImagesToLoad(callback: () => void) {
  const images = document.querySelectorAll('img')
  const totalImages = images.length
  let loadedCount = 0

  if (totalImages === 0) {
    callback()
    return
  }

  const checkIfAllLoaded = () => {
    loadedCount++
    if (loadedCount === totalImages) {
      callback()
    }
  };

  images.forEach(image => {
    if (image.complete) {
      checkIfAllLoaded()
    } else {
      image.addEventListener('load', checkIfAllLoaded)
      image.addEventListener('error', checkIfAllLoaded)
    }
  });
}

export function groupBy<T, K extends string | number | symbol>(
  array: T[],
  getKey: (item: T) => K
): Record<K, T[]> {
  return array.reduce((result, item) => {
    const key = getKey(item);
    if (!result[key]) {
      result[key] = [];
    }
    result[key].push(item);
    return result;
  }, {} as Record<K, T[]>);
}

export function localizeArkhamDBBaseUrl() {
  const language = localStorage.getItem('language') || 'en'

  const baseUrl = new URL('https://arkhamdb.com');
  if (language === "en") return baseUrl.origin;

  baseUrl.hostname = `${language}.${baseUrl.hostname}`;
  return baseUrl.origin;
}
