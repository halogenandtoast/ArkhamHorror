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

// every third-edition image goes through here: img('cards/x.webp')
export const img = (path: string) => `${assetHost.value}/img/ah3e/${path}`

// An image that failed to load. The viewer removed the <img> and marked its
// parent `no-art`; here the parent binds `no-art` and the img is v-if'd away.
const broken = reactive(new Set<string>())
export const isBroken = (src: string | null | undefined) => !!src && broken.has(src)
export const markBroken = (src: string | null | undefined) => {
  if (src) broken.add(src)
}
