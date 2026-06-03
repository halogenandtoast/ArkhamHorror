import { api } from '../lib/api.mjs'
import { HARNESS_USER } from '../lib/config.mjs'
import { getState, setState } from '../lib/state.mjs'

// Bootstraps the harness user. On first run, tries to authenticate; if that
// fails, registers a new user with the harness credentials. Caches the token
// in .state.json so subsequent runs skip the round-trip.
export async function ensureLogin({ force = false } = {}) {
  if (!force) {
    const cached = await getState('token')
    if (cached) {
      try {
        await api.whoami(cached)
        return cached
      } catch (e) {
        if (e.status !== 401) throw e
        // fall through and re-auth
      }
    }
  }

  let token
  try {
    const res = await api.authenticate({
      email: HARNESS_USER.email,
      password: HARNESS_USER.password,
    })
    token = res.token
  } catch (e) {
    if (e.status !== 401 && e.status !== 400) throw e
    // No such user, or wrong password. Try to register.
    const res = await api.register(HARNESS_USER)
    token = res.token
  }

  if (!token) throw new Error('Login failed: no token returned')

  await setState('token', token)
  return token
}
