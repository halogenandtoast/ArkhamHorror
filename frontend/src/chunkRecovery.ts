/* Recovering from a bundle that is no longer on the server.
 *
 * Route components are lazy (`() => import(...)`), so a tab that has been open
 * across a deploy goes on asking for chunk names only its own build ever had.
 * Two server-side layers absorb most of that -- the load balancer pins a browser
 * to one pod for the length of a page load, and nginx falls back to the CDN for
 * a bundle the pod's image does not contain -- but neither can help with a
 * build old enough that its bundles predate those uploads. Reloading can:
 * index.html is served with `max-age=0, must-revalidate`, so the fresh copy
 * names bundles that exist.
 *
 * Everything here is guarded by a timestamp, because a reload loop is a worse
 * failure than the one being recovered from. If the reloaded page fails the same
 * way, the cause is not staleness, and the error is left to surface. */

const RELOAD_MARKER = 'arkham:chunk-reload-at'
const RELOAD_COOLDOWN_MS = 30_000

/* The browsers disagree on the wording, and none of them give a usable error
 * type, so this matches on text. Each line is one engine's phrasing for "a
 * dynamic import did not load". */
const STALE_CHUNK_MESSAGES = [
  /failed to fetch dynamically imported module/i, // Chromium
  /error loading dynamically imported module/i, // Firefox
  /importing a module script failed/i, // Safari
  /unable to preload css/i, // vite's own CSS preload helper
]

/* Unwraps the several shapes an import failure arrives in: a plain Error, a
 * string, or -- for `vite:preloadError` -- an Event carrying the real error on
 * `payload`. */
function messageOf(error: unknown, depth = 0): string {
  if (error === null || error === undefined || depth > 3) return ''
  if (typeof error === 'string') return error
  if (error instanceof Error) return error.message

  const payload = (error as { payload?: unknown }).payload
  if (payload !== undefined && payload !== error) return messageOf(payload, depth + 1)

  const message = (error as { message?: unknown }).message
  return typeof message === 'string' ? message : ''
}

export function isStaleChunkError(error: unknown): boolean {
  const message = messageOf(error)
  return message !== '' && STALE_CHUNK_MESSAGES.some((pattern) => pattern.test(message))
}

function reloadedRecently(): boolean {
  try {
    const at = Number(sessionStorage.getItem(RELOAD_MARKER) ?? '0')
    return Number.isFinite(at) && Date.now() - at < RELOAD_COOLDOWN_MS
  } catch {
    /* sessionStorage throws outright when site data is blocked. With no way to
     * record that a reload has happened, the only answer that cannot loop is
     * "one already did". */
    return true
  }
}

/* Returns whether the error was handled, so a caller can stop treating it as
 * its own problem. */
export function reloadIfStaleChunk(error: unknown): boolean {
  if (!isStaleChunkError(error) || reloadedRecently()) return false

  try {
    sessionStorage.setItem(RELOAD_MARKER, String(Date.now()))
  } catch {
    return false
  }

  window.location.reload()
  return true
}

export function installChunkRecovery(): void {
  /* vite's preload helper raises this instead of throwing, and treats
   * preventDefault as "someone has dealt with it". */
  window.addEventListener('vite:preloadError', (event) => {
    if (reloadIfStaleChunk(event)) event.preventDefault()
  })
}
