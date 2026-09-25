import { ref, shallowRef } from 'vue'
import { ApiError, errorText, getCatalog, whoami } from '@/api'
import { clearToken, getToken } from '@/authToken'
import type { Catalog, User } from '@/types'

export const user = ref<User | null>(null)
// 'loading' until whoami answers; 'error' is a server we couldn't reach, not a bad token
export const sessionState = ref<'loading' | 'signedIn' | 'signedOut' | 'error'>('loading')
export const sessionError = ref('')

export async function loadSession() {
  if (!getToken()) {
    sessionState.value = 'signedOut'
    return
  }
  sessionState.value = 'loading'
  try {
    user.value = await whoami()
    sessionState.value = 'signedIn'
  } catch (e) {
    user.value = null
    if (e instanceof ApiError && e.status !== 0 && e.status < 500) {
      sessionState.value = 'signedOut'
    } else {
      sessionError.value = errorText(e)
      sessionState.value = 'error'
    }
  }
}

export function signOut() {
  clearToken()
  user.value = null
  sessionState.value = 'signedOut'
}

export const mainSiteUrl =
  import.meta.env.VITE_MAIN_SITE_URL || (import.meta.env.PROD ? 'https://arkhamhorror.app' : 'http://localhost:8080')
export const signInUrl = `${mainSiteUrl}/#/sign-in`

// the catalog is static; fetch it once and share it
export const catalog = shallowRef<Catalog | null>(null)
let catalogLoad: Promise<Catalog> | null = null
export function loadCatalog(): Promise<Catalog> {
  if (catalog.value) return Promise.resolve(catalog.value)
  catalogLoad ??= getCatalog().then(
    (c) => (catalog.value = c),
    (e) => {
      catalogLoad = null
      throw e
    },
  )
  return catalogLoad
}
