import type { Game } from '@/arkham/types/Game'
import type { StandaloneSetting } from '@/arkham/types/StandaloneSetting'

// A settings PUT returns no game state. Read it after saving so setup can
// advance even when its websocket update was published before we subscribed.
export function createStandaloneSettingsSubmission(actions: {
  playerId: string
  save: (settings: StandaloneSetting[]) => Promise<void>
  fetch: () => Promise<Game>
}) {
  let attempted = false

  return async (settings: StandaloneSetting[]): Promise<Game> => {
    if (attempted) {
      // A failed request may still have committed. Check before retrying: a
      // second settings answer could consume the next parked question.
      const latest = await actions.fetch()
      if (latest.question[actions.playerId]?.tag !== 'PickScenarioSettings') return latest
    }
    attempted = true
    await actions.save(settings)
    return actions.fetch()
  }
}
