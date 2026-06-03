import { api } from '../lib/api.mjs'
import { setState } from '../lib/state.mjs'

// Create a game and remember its id as the "current" harness game. Returns the
// game object.
//
// Sensible defaults for a quick smoke test: solo, easy, no decks chosen
// (the resulting game lands in the ChooseDeck state, which is enough to
// verify list/portrait/CTA rendering without ArkhamDB dependencies).
export async function createGame({
  campaignId = null,
  scenarioId = null,
  difficulty = 'Easy',
  playerCount = 1,
  deckIds = null,
  campaignName = 'harness',
  multiplayerVariant = 'Solo',
  includeTarotReadings = false,
  options = [],
} = {}) {
  if (!campaignId && !scenarioId) {
    throw new Error('createGame requires campaignId or scenarioId')
  }

  const game = await api.createGame({
    campaignId,
    scenarioId,
    difficulty,
    playerCount,
    deckIds: deckIds ?? Array(playerCount).fill(null),
    campaignName,
    multiplayerVariant,
    includeTarotReadings,
    options,
  })

  await setState('gameId', game.id)
  return game
}

// Send a raw debug Message to the current game. Thin wrapper for callers that
// want to skip ahead, force state, etc.
export async function sendRaw(gameId, message) {
  return api.raw(gameId, message)
}
