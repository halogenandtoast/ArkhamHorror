import { readFile, readdir } from 'node:fs/promises'
import { resolve } from 'node:path'
import { api } from '../lib/api.mjs'
import { HARNESS_ROOT } from '../lib/config.mjs'
import { setState } from '../lib/state.mjs'

const DECK_FIXTURE_DIR = resolve(HARNESS_ROOT, 'fixtures/decks')

// Load every cached ArkhamDB deck JSON the harness has scraped.
export async function listFixtureDecks() {
  const files = await readdir(DECK_FIXTURE_DIR)
  return files.filter((f) => f.endsWith('.json'))
}

async function readFixture(filename) {
  const raw = await readFile(resolve(DECK_FIXTURE_DIR, filename), 'utf-8')
  return JSON.parse(raw)
}

// Picks one fixture deck at random and registers it for the current user via
// POST /arkham/decks. Returns the resulting Deck (with the backend's own id).
//
// Some ArkhamDB decks reference investigators or cards the engine doesn't
// support yet — the backend will 4xx on those. We retry with another fixture
// up to `attempts` times before giving up so a single bad deck doesn't break
// the harness.
export async function loadRandomDeck({ attempts = 5, supportedInvestigators } = {}) {
  const files = await listFixtureDecks()
  if (files.length === 0) throw new Error('no fixture decks — run harness/scripts/scrape-arkhamdb.sh first')

  // Reuse the same investigator set across attempts.
  const supported = supportedInvestigators ?? new Set(await api.fetchInvestigators().catch(() => []))

  const tried = new Set()
  const errors = []

  for (let i = 0; i < attempts; i++) {
    const remaining = files.filter((f) => !tried.has(f))
    if (remaining.length === 0) break
    const pick = remaining[Math.floor(Math.random() * remaining.length)]
    tried.add(pick)

    const deckList = await readFixture(pick)
    const investigatorCode = deckList.meta?.alternate_front ?? deckList.investigator_code
    if (supported.size && !supported.has(investigatorCode) && !supported.has(deckList.investigator_code)) {
      errors.push(`${pick}: investigator ${investigatorCode} unsupported`)
      continue
    }

    try {
      const deck = await api.newDeck({
        deckId: String(deckList.id),
        deckName: deckList.name,
        deckUrl: null,
        deckList,
      })
      await setState('deckId', deck.id)
      await setState('deckName', deckList.name)
      return deck
    } catch (e) {
      errors.push(`${pick}: ${e.message}`)
    }
  }

  const err = new Error(`loadRandomDeck failed after ${tried.size} attempts:\n  ${errors.join('\n  ')}`)
  err.errors = errors
  throw err
}
