import type { ArkhamDbDecklist } from '@/arkham/types/Deck'

export interface UpgradeDeckUploadActions {
  setModel: (deck: ArkhamDbDecklist) => void
  setDeckList: (deck: ArkhamDbDecklist) => void
  setDeckUrl: (url: string | null) => void
  setDeck: (url: string | null) => void
  setDeckInvestigator: (investigatorCode: string) => void
  upgrade: () => void
}

/* The fields the server actually requires of a decklist (see ArkhamDBDecklist's FromJSON).
 * Used to vet decks FETCHED from ArkhamDB / arkham.build too: `fetch` does not reject on a
 * 404, so an error body ({"message":"No share was found for this deck"}) would otherwise sail
 * through as a decklist with no investigator and no cards, and only fail later as an opaque
 * 400 from the upgrade endpoint (#5257). Deliberately says nothing about `id`, which is a
 * string on arkham.build and a number on ArkhamDB. */
export function isUsableDecklist(data: unknown): data is ArkhamDbDecklist {
  if (typeof data !== 'object' || data === null || Array.isArray(data)) return false
  const d = data as Record<string, unknown>

  if (typeof d.investigator_name !== 'string') return false
  if (typeof d.investigator_code !== 'string' || d.investigator_code.length === 0) return false

  if (typeof d.slots !== 'object' || d.slots === null || Array.isArray(d.slots)) return false
  if (!Object.values(d.slots as Record<string, unknown>).every((v) => typeof v === 'number')) return false

  return true
}

function isUploadableUpgradeDeck(data: unknown): data is ArkhamDbDecklist {
  if (!isUsableDecklist(data)) return false
  const d = data as unknown as Record<string, unknown>

  if (typeof d.id !== 'string') return false
  if (typeof d.name !== 'string') return false
  if (!('url' in d) || (d.url !== null && typeof d.url !== 'string')) return false

  return true
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
