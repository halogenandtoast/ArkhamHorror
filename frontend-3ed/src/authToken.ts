// The sign-in token lives in a cookie on the parent domain so every
// arkhamhorror.app subdomain shares the login made on the main site.
// localStorage is the fallback, as on the main site.
const KEY = 'arkham-token'
const MAX_AGE = 60 * 60 * 24 * 365

// e.g. ".arkhamhorror.app" in production; unset locally, where a cookie on
// localhost is already visible to every port
const cookieDomain = import.meta.env.VITE_AUTH_COOKIE_DOMAIN || ''

function cookieAttributes(maxAge: number): string {
  const domain = cookieDomain ? `; Domain=${cookieDomain}` : ''
  const secure = window.location.protocol === 'https:' ? '; Secure' : ''
  return `; Path=/; Max-Age=${maxAge}; SameSite=Lax${domain}${secure}`
}

function readCookie(): string | null {
  const entry = document.cookie.split('; ').find((c) => c.startsWith(`${KEY}=`))
  return entry ? decodeURIComponent(entry.slice(KEY.length + 1)) : null
}

function readLocalStorage(): string | null {
  try {
    return localStorage.getItem(KEY)
  } catch {
    return null
  }
}

export function getToken(): string | null {
  const fromCookie = readCookie()
  if (fromCookie) return fromCookie
  const stored = readLocalStorage()
  if (stored) setToken(stored)
  return stored
}

export function setToken(token: string) {
  document.cookie = `${KEY}=${encodeURIComponent(token)}${cookieAttributes(MAX_AGE)}`
  try {
    localStorage.setItem(KEY, token)
  } catch {
    // storage can be unavailable (private mode); the cookie still holds it
  }
}

export function clearToken() {
  document.cookie = `${KEY}=${cookieAttributes(0)}`
  try {
    localStorage.removeItem(KEY)
  } catch {
    // nothing stored
  }
}
