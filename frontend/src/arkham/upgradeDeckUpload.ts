import type { ArkhamDbDecklist } from '@/arkham/types/Deck'

export interface UpgradeDeckUploadActions {
  setModel: (deck: ArkhamDbDecklist) => void
  setDeckList: (deck: ArkhamDbDecklist) => void
  setDeckUrl: (url: string | null) => void
  setDeck: (url: string | null) => void
  setDeckInvestigator: (investigatorCode: string) => void
  upgrade: () => void
}

function isUploadableUpgradeDeck(data: unknown): data is ArkhamDbDecklist {
  return (
    typeof data === 'object' &&
    data !== null &&
    'investigator_code' in data &&
    typeof data.investigator_code === 'string' &&
    data.investigator_code.length > 0
  )
}

export function loadUpgradeDeckFromJsonText(
  jsonText: string,
  actions: UpgradeDeckUploadActions,
): boolean {
  try {
    const data = JSON.parse(jsonText) as unknown
    if (!isUploadableUpgradeDeck(data)) return false

    const deckUrl = data.url ?? null

    actions.setModel(data)
    actions.setDeckList(data)
    actions.setDeckUrl(deckUrl)
    actions.setDeck(deckUrl)
    actions.setDeckInvestigator(data.investigator_code)
    actions.upgrade()

    return true
  } catch {
    return false
  }
}
