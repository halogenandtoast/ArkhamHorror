import { API_BASE } from './config.mjs'
import { getState } from './state.mjs'

async function request(method, path, { token, body, query } = {}) {
  const url = new URL(`${API_BASE}/${path.replace(/^\//, '')}`)
  if (query) {
    for (const [k, v] of Object.entries(query)) {
      if (v !== undefined && v !== null) url.searchParams.set(k, String(v))
    }
  }

  const headers = { 'Content-Type': 'application/json' }
  if (token) headers['Authorization'] = `Token ${token}`

  const res = await fetch(url, {
    method,
    headers,
    body: body !== undefined ? JSON.stringify(body) : undefined,
  })

  const text = await res.text()
  let parsed = null
  if (text.length > 0) {
    try { parsed = JSON.parse(text) } catch { parsed = text }
  }

  if (!res.ok) {
    const err = new Error(`${method} ${path} -> ${res.status}: ${typeof parsed === 'string' ? parsed : JSON.stringify(parsed)}`)
    err.status = res.status
    err.body = parsed
    throw err
  }
  return parsed
}

async function authedRequest(method, path, opts = {}) {
  const token = opts.token ?? await getState('token')
  if (!token) throw new Error('No auth token — run `harness login` first')
  return request(method, path, { ...opts, token })
}

export const api = {
  // Auth
  register: ({ username, email, password }) =>
    request('POST', 'register', { body: { username, email, password } }),

  authenticate: ({ email, password }) =>
    request('POST', 'authenticate', { body: { email, password } }),

  whoami: (token) => request('GET', 'whoami', { token }),

  // Games
  createGame: (payload) => authedRequest('POST', 'arkham/games', { body: payload }),
  fetchGame: (gameId) => authedRequest('GET', `arkham/games/${gameId}`),
  listGames: () => authedRequest('GET', 'arkham/games'),
  exportGame: (gameId) => authedRequest('GET', `arkham/games/${gameId}/export`),

  // Decks
  listDecks: () => authedRequest('GET', 'arkham/decks'),
  fetchInvestigators: () => authedRequest('GET', 'arkham/investigators'),
  newDeck: ({ deckId, deckName, deckUrl, deckList }) =>
    authedRequest('POST', 'arkham/decks', {
      body: { deckId, deckName, deckUrl, deckList },
    }),
  deleteDeck: (deckId) => authedRequest('DELETE', `arkham/decks/${deckId}`),

  // Answer a question (the normal "choose option N" path)
  answer: (gameId, choice, investigatorId = null) =>
    authedRequest('PUT', `arkham/games/${gameId}`, {
      body: { tag: 'Answer', contents: { choice, investigatorId } },
    }),

  // Debug: inject a raw Message into the game
  raw: (gameId, gameMessage) =>
    authedRequest('PUT', `arkham/games/${gameId}/raw`, {
      body: { gameMessage },
    }),
}
