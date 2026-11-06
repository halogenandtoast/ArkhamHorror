import { ref } from 'vue'
import { cardArt } from '@/arkham/cardImages'
import { useDbCardStore } from '@/stores/dbCards'

/* Card names for the map and scene views.
 *
 * The store only holds the player's own language, and if that language has no card file (or has not
 * finished loading) every lookup misses. Rather than show a raw card code, fall back to the English
 * card list, fetched once and shared. */
const english = ref<Map<string, string> | null>(null)
let loading = false

async function loadEnglish() {
  loading = true
  try {
    const response = await fetch('cards/cards_en.json')
    if (!response.ok) return
    const cards = (await response.json()) as { code: string; name: string }[]
    english.value = new Map(cards.map((c) => [c.code, c.name]))
  } catch {
    // Names are cosmetic; keep showing codes if the file cannot be had.
  } finally {
    loading = false
  }
}

export function useCardNames(): (code: string) => string {
  const dbCards = useDbCardStore()
  return (code: string) => {
    const art = cardArt(code)
    const own = dbCards.getDbCard(art)?.name
    if (own) return own
    if (!english.value && !loading) void loadEnglish()
    return english.value?.get(art) ?? english.value?.get(`${art}a`) ?? code
  }
}
