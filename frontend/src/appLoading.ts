/* The hard-refresh splash is markup in index.html, not a component: it has to
 * paint before this bundle exists. All the app can do is take it down, which it
 * does once there is something real underneath -- see App.vue.
 *
 * Idempotent, and a no-op if the splash is already gone (it removes itself after
 * 25s so a failure to boot can never trap someone on it). */
declare global {
  interface Window {
    __appLoading?: { hide: () => void }
  }
}

export function hideAppLoading(): void {
  window.__appLoading?.hide()
}
