// Laying custom cards over a deck, on the client side.
//
// The server owns the real transformation (Arkham.Custom.Overlay); this is the
// same edit applied locally so the deck page can show what an overlay will do
// while you are still building it.
import { libraryCards } from '@/arkham/customCardLibrary'
import { stripCardCodePrefix } from '@/arkham/customCards'
import type { DeckOverlay } from '@/arkham/types/Deck'

export type { DeckOverlay }

export const emptyOverlay = (): DeckOverlay => ({
  investigator: null,
  swaps: {},
  add: {},
  remove: {},
})

export function overlayIsEmpty(overlay: DeckOverlay | null): boolean {
  if (!overlay) return true
  return (
    !overlay.investigator &&
    Object.keys(overlay.swaps).length === 0 &&
    Object.keys(overlay.add).length === 0 &&
    Object.keys(overlay.remove).length === 0
  )
}

/* Slot keys arrive both bare and with the 'c' the engine prepends, so a lookup
 * has to try both before deciding a card is not in the deck. */
export function slotKey(slots: Record<string, number>, code: string): string | null {
  const bare = stripCardCodePrefix(code).replace(/^c(?!\*)/, '')
  if (slots[bare] !== undefined) return bare
  if (slots[`c${bare}`] !== undefined) return `c${bare}`
  return null
}

/* The cards a custom investigator brings with them, from their `_signatures`. */
export function customSignatures(investigator: string | null): string[] {
  if (!investigator) return []
  const card = libraryCards().find(
    (c) => stripCardCodePrefix(c.def.cardCode) === stripCardCodePrefix(investigator),
  )
  return ((card?.def.meta?._signatures ?? []) as string[]) ?? []
}

/* The deck as the overlay leaves it. Mirrors `applyOverlay` on the server for
 * everything the client can know: the signatures a replaced investigator takes
 * with them are pre-filled into `remove` by whoever built the overlay, and the
 * ones a custom investigator brings are added here. */
export function applyOverlayToSlots(
  slots: Record<string, number>,
  overlay: DeckOverlay | null,
  /* Keep a card that has been taken out entirely, at zero. While an overlay is
   * being edited that is what lets you put it back; the deck as played drops
   * it. */
  keepEmpty = false,
): Record<string, number> {
  if (!overlay) return { ...slots }
  const result: Record<string, number> = { ...slots }

  for (const [from, to] of Object.entries(overlay.swaps)) {
    const key = slotKey(result, from)
    if (key === null) continue
    result[to] = (result[to] ?? 0) + result[key]
    delete result[key]
  }

  for (const code of customSignatures(overlay.investigator)) {
    result[code] = (result[code] ?? 0) + 1
  }

  for (const [code, count] of Object.entries(overlay.add)) {
    result[code] = (result[code] ?? 0) + count
  }

  for (const [code, count] of Object.entries(overlay.remove)) {
    const key = slotKey(result, code)
    if (key === null) continue
    result[key] -= count
    if (result[key] < 0) result[key] = 0
    if (result[key] === 0 && !keepEmpty) delete result[key]
  }

  return result
}
