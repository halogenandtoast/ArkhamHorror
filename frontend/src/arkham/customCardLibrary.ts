// Cards you have made before, kept in this browser so they can be added to any
// game again. Distinct from `customCards.ts`, which is the registry of cards
// live in the *current* game (those come from the server, and include cards
// other players made).
//
// A library entry owns its card code: adding it to a game re-registers that same
// code, so two copies really are two copies of one card rather than two
// unrelated cards that happen to share a name.
import { reactive } from 'vue'
import type { CustomCard } from '@/arkham/customCards'

const STORAGE_KEY = 'arkham:custom-card-library'

export type LibraryCard = CustomCard & { createdAt: number }

const entries = reactive<LibraryCard[]>([])

function persist() {
  localStorage.setItem(STORAGE_KEY, JSON.stringify(entries))
}

function load() {
  try {
    const raw = localStorage.getItem(STORAGE_KEY)
    if (!raw) return
    const parsed = JSON.parse(raw)
    if (Array.isArray(parsed)) entries.push(...parsed)
  } catch {
    // A corrupt or unreadable library is not worth failing the game over.
  }
}

load()

export function libraryCards(): LibraryCard[] {
  return [...entries].sort((a, b) => b.createdAt - a.createdAt)
}

/* Saving can fail: art is inlined as a data URI, so a few dozen cards can fill
 * the storage quota. The caller surfaces that rather than losing the card
 * silently -- it is still added to the game either way. */
export function saveToLibrary(card: CustomCard): { saved: boolean; reason?: string } {
  const existing = entries.findIndex((e) => e.def.cardCode === card.def.cardCode)
  const entry: LibraryCard = { ...card, createdAt: Date.now() }
  if (existing === -1) entries.push(entry)
  else entries.splice(existing, 1, entry)

  try {
    persist()
    return { saved: true }
  } catch (error) {
    if (existing === -1) entries.pop()
    console.error(error)
    return { saved: false, reason: 'The card library is full. Delete a card to make room.' }
  }
}

export function removeFromLibrary(cardCode: string) {
  const index = entries.findIndex((e) => e.def.cardCode === cardCode)
  if (index === -1) return
  entries.splice(index, 1)
  try {
    persist()
  } catch (error) {
    console.error(error)
  }
}
