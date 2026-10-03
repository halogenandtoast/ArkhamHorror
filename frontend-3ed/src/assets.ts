import { reactive, ref } from 'vue'

const cdnUrl = 'https://assets.arkhamhorror.app'
const envHost = import.meta.env.VITE_ASSET_HOST
export const assetHost = ref<string>(envHost !== undefined ? envHost : cdnUrl)

// the API's site settings name the asset host the main app uses; a null keeps ours
export async function loadAssetHost() {
  try {
    const r = await fetch('/api/v1/site-settings')
    if (!r.ok) return
    const j = (await r.json()) as { assetHost?: string | null }
    if (j.assetHost != null) assetHost.value = j.assetHost
  } catch {
    // keep the build-time host
  }
}

// every third-edition image goes through here: img('cards/items/x.webp')
export const img = (path: string) => `${assetHost.value}/img/ah3e/${path}`

// Card art is filed by type (monsters/, items/, ...); the catalog says the path
// each code's art sits at, and both faces of a card share it.
export const cardArtPaths = ref<Record<string, string>>({})
/* Archive cards are filed together under archive/ by their printed number, not
by card code and not by what kind of card they turn out to be -- an epic monster
or an artifact printed on an archive card still lives with its numbered siblings. */
const ARCHIVE_CODE = /^(?:feast|echoes|vot|sot|sitd|archive)-(\d{1,3})$/
export const archivePath = (code: string, back = false) => {
  const m = ARCHIVE_CODE.exec(code)
  return m ? `archive/${m[1].padStart(3, '0')}${back ? 'b' : ''}.avif` : null
}
export const cardImg = (code: string, back = false) =>
  img(archivePath(code, back) ?? `cards/${cardArtPaths.value[code] ?? code}${back ? 'b' : ''}.webp`)

// An image that failed to load. The viewer removed the <img> and marked its
// parent `no-art`; here the parent binds `no-art` and the img is v-if'd away.
const broken = reactive(new Set<string>())
export const isBroken = (src: string | null | undefined) => !!src && broken.has(src)
export const markBroken = (src: string | null | undefined) => {
  if (src) broken.add(src)
}
/* The first of these that has not already failed to load, or the last if they all have.
Art for a numbered card is filed one of two ways, so the viewer asks for one, and the
<img>'s error hands it the other -- after which it remembers and goes straight there. */
export const firstGood = (...srcs: (string | null | undefined)[]) => {
  const real = srcs.filter((s): s is string => !!s)
  return real.find((s) => !broken.has(s)) ?? real[real.length - 1] ?? null
}
