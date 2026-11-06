import { ref } from 'vue'
import { cardArt } from '@/arkham/cardImages'
import { useDbCardStore, type ArkhamDBCard } from '@/stores/dbCards'

/* Card names and text for the map and scene views.
 *
 * The store only holds the player's own language, and if that language has no card file (or has not
 * finished loading) every lookup misses. Rather than show a raw card code, fall back to the English
 * card list, fetched once and shared. */
const english = ref<Map<string, ArkhamDBCard> | null>(null)
let loading = false

async function loadEnglish() {
  loading = true
  try {
    const response = await fetch('cards/cards_en.json')
    if (!response.ok) return
    const cards = (await response.json()) as ArkhamDBCard[]
    english.value = new Map(cards.map((c) => [c.code, c]))
  } catch {
    // Names are cosmetic; keep showing codes if the file cannot be had.
  } finally {
    loading = false
  }
}

/** The whole card record (name, flavour, text, doom ...), own language first, English as the fallback. */
export function useCardInfo(): (code: string) => ArkhamDBCard | null {
  const dbCards = useDbCardStore()
  return (code: string) => {
    const art = cardArt(code)
    const bare = art.replace(/[a-z]$/, '')
    const own = dbCards.getDbCard(art) ?? dbCards.getDbCard(bare)
    if (own) return own
    if (!english.value && !loading) void loadEnglish()
    return english.value?.get(art) ?? english.value?.get(`${art}a`) ?? english.value?.get(bare) ?? null
  }
}

export function useCardNames(): (code: string) => string {
  const info = useCardInfo()
  return (code: string) => info(code)?.name ?? code
}
