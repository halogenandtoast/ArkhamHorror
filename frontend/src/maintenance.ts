import { ref, readonly } from 'vue'

/* Maintenance mode, as the client sees it.
 *
 * A module of its own rather than a Pinia store because `api.ts` has to set it
 * from an interceptor and a store would import `api.ts` right back. A plain ref
 * is also what we want at the point of failure: it works before Pinia is
 * installed, which matters because the very first request the app makes can be
 * the one that 503s.
 */
const active = ref(false)
const message = ref<string | null>(null)

export const maintenanceActive = readonly(active)
export const maintenanceMessage = readonly(message)

export function setMaintenance(on: boolean, text?: string | null) {
  active.value = on
  if (on) {
    // Keep the message we already had if this notice carried none; the banner
    // saying nothing is worse than it saying something slightly stale.
    if (text) message.value = text
  } else {
    message.value = null
  }
}

/* The server's 503 body, which `maintenanceMiddleware` shapes as
 * `{ maintenance: true, since, message }`. Checked for the flag rather than for
 * the bare status, so an unrelated 503 from a proxy or a cold start does not
 * black out the site. */
export function maintenanceFromResponse(data: unknown): string | null | undefined {
  if (typeof data !== 'object' || data === null) return undefined
  const body = data as { maintenance?: unknown; message?: unknown }
  if (body.maintenance !== true) return undefined
  return typeof body.message === 'string' ? body.message : null
}
