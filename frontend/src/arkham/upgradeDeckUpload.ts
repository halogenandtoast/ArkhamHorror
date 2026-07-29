import type { ArkhamDbDecklist } from '@/arkham/types/Deck'

export interface UpgradeDeckUploadActions {
  setModel: (deck: ArkhamDbDecklist) => void
  setDeckList: (deck: ArkhamDbDecklist) => void
  setDeckUrl: (url: string | null) => void
  setDeck: (url: string | null) => void
  setDeckInvestigator: (investigatorCode: string) => void
  upgrade: () => void
}

export type UpgradeDeckUploadResult =
  | { ok: true }
  | { ok: false; reason: 'invalidJson' | 'notADecklist' }

/* The fields the server actually requires of a decklist (see ArkhamDBDecklist's FromJSON).
 * Used to vet decks FETCHED from ArkhamDB / arkham.build too: `fetch` does not reject on a
 * 404, so an error body ({"message":"No share was found for this deck"}) would otherwise sail
 * through as a decklist with no investigator and no cards, and only fail later as an opaque
 * 400 from the upgrade endpoint (#5257). Deliberately says nothing about `id`, which is a
 * string on arkham.build and a number on ArkhamDB, nor about `url`, which only exists on a
 * decklist we fetched ourselves -- an exported .json file has neither. */
export function isUsableDecklist(data: unknown): data is ArkhamDbDecklist {
  if (typeof data !== 'object' || data === null || Array.isArray(data)) return false
  const d = data as Record<string, unknown>

  if (typeof d.investigator_code !== 'string' || d.investigator_code.length === 0) return false
  if ('investigator_name' in d && d.investigator_name != null && typeof d.investigator_name !== 'string') return false

  if (typeof d.slots !== 'object' || d.slots === null || Array.isArray(d.slots)) return false
  if (!Object.values(d.slots as Record<string, unknown>).every((v) => typeof v === 'number')) return false

  return true
}

/* An uploaded file is a raw ArkhamDB / arkham.build export, so it carries neither a `url` nor
 * necessarily a string `id` (ArkhamDB's is a number) -- REQUIRING those rejected every real
 * export, and the upload silently did nothing. Normalize to the shape the rest of the client
 * expects instead of refusing the file. */
function normalizeUploadedDecklist(data: unknown): ArkhamDbDecklist | null {
  if (!isUsableDecklist(data)) return null
  const d = data as unknown as Record<string, unknown>

  const normalized = {
    ...(data as ArkhamDbDecklist),
    id: d.id == null ? '' : String(d.id),
    name: typeof d.name === 'string' ? d.name : '',
    url: typeof d.url === 'string' ? d.url : null,
  }

  // Left absent rather than blanked: the server fills in the investigator's real name for a
  // decklist that omits it, and this list is written back to the player's saved deck.
  if (typeof d.investigator_name !== 'string') delete (normalized as { investigator_name?: string }).investigator_name

  return normalized
}

/* The investigator the deck is actually for: a parallel/alternate front lives in meta, which
 * ArkhamDB stores as a json STRING and arkham.build as an object. Matches NewDeck.vue and the
 * server's decklistInvestigatorId. */
function uploadedInvestigatorCode(deck: ArkhamDbDecklist): string {
  const meta = (() => {
    if (deck.meta == null) return null
    if (typeof deck.meta !== 'string') return deck.meta as Record<string, unknown>
    try {
      return JSON.parse(deck.meta || '{}') as Record<string, unknown>
    } catch {
      return null
    }
  })()

  const front = meta?.alternate_front
  return typeof front === 'string' && front.length > 0 ? front : deck.investigator_code
}

export function loadUpgradeDeckFromJsonText(
  jsonText: string,
  actions: UpgradeDeckUploadActions,
): UpgradeDeckUploadResult {
  let data: unknown
  try {
    data = JSON.parse(jsonText) as unknown
  } catch {
    return { ok: false, reason: 'invalidJson' }
  }

  const deck = normalizeUploadedDecklist(data)
  if (!deck) return { ok: false, reason: 'notADecklist' }

  actions.setModel(deck)
  actions.setDeckList(deck)
  actions.setDeckUrl(deck.url)
  actions.setDeck(deck.url)
  actions.setDeckInvestigator(uploadedInvestigatorCode(deck))
  actions.upgrade()

  return { ok: true }
}
