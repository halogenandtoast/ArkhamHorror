/* Finding a card in a library that has grown past a screenful.
 *
 * One filter object covers both halves of the builder -- the list of sets and
 * the panel inside a set -- because the thing being narrowed is the same thing
 * either way: cards. The text box and the facets are ANDed; the values picked
 * within one facet are ORed, which is what "Guardian or Mystic" means when both
 * chips are lit.
 *
 * Nothing here knows about Vue or i18n. The facet headings are the component's
 * to translate; the values name themselves, since a trait and a class are
 * printed on the card exactly as they read here.
 */
import { CARD_TYPES, cardTypeLabel, type CustomCard } from '@/arkham/customCards'

export type FacetKey = 'types' | 'classes' | 'traits' | 'levels'

export const FACET_KEYS: FacetKey[] = ['types', 'classes', 'traits', 'levels']

export type CardFilter = {
  query: string
  types: string[]
  classes: string[]
  traits: string[]
  levels: string[]
}

export type FacetValue = { value: string; label: string; count: number }
export type Facet = { key: FacetKey; values: FacetValue[] }

export const emptyCardFilter = (): CardFilter => ({
  query: '',
  types: [],
  classes: [],
  traits: [],
  levels: [],
})

/* How many chips are lit. The query is not one of them: it has its own box with
 * its own clear button, and counting it would make the badge on the facets
 * button disagree with the chips it opens. */
export const chosenFacetCount = (filter: CardFilter): number =>
  FACET_KEYS.reduce((n, key) => n + filter[key].length, 0)

export const filterIsEmpty = (filter: CardFilter): boolean =>
  !filter.query.trim() && chosenFacetCount(filter) === 0

/* The order the classes are printed in, so the chips do not reshuffle
 * themselves as a set gains cards. A class the list does not know -- one
 * written through the raw block -- falls in after them, alphabetically. */
const CLASS_ORDER = ['Guardian', 'Seeker', 'Rogue', 'Mystic', 'Survivor', 'Neutral']

/* What a card offers to a facet. A card with no level offers nothing to the
 * level facet rather than a zero: an encounter card is not a level 0 card, and
 * showing it as one would hide the player cards that are. */
export function facetValuesOf(card: CustomCard, key: FacetKey): string[] {
  switch (key) {
    case 'types':
      return [card.def.cardType]
    case 'classes':
      return card.def.classSymbols ?? []
    case 'traits':
      return card.def.cardTraits ?? []
    case 'levels':
      return card.def.level === null || card.def.level === undefined ? [] : [String(card.def.level)]
  }
}

export function facetValueLabel(key: FacetKey, value: string): string {
  if (key === 'types') return cardTypeLabel(value)
  if (key === 'levels') return value
  return value
}

/* Everything about a card that typing at it should find: what it is called,
 * what it is, what it is made of, and the number it is filed under. Built on
 * demand rather than cached -- it is a handful of strings per card, and a cache
 * would have to be invalidated on every keystroke in the editor. */
function searchText(card: CustomCard): string {
  const def = card.def
  return [
    def.name.title,
    def.name.subtitle,
    ...(def.cardTraits ?? []),
    ...(def.classSymbols ?? []),
    cardTypeLabel(def.cardType),
    def.level === null || def.level === undefined ? '' : `level ${def.level}`,
    typeof def.meta?.number === 'string' ? def.meta.number : '',
    def.cardCode,
  ]
    .filter(Boolean)
    .join(' ')
    .toLowerCase()
}

/* Every word has to land somewhere, so "ancient tome" finds the card whose
 * title is one word and whose trait is the other. */
export function matchesQuery(card: CustomCard, query: string): boolean {
  const words = query.trim().toLowerCase().split(/\s+/).filter(Boolean)
  if (!words.length) return true
  const haystack = searchText(card)
  return words.every((word) => haystack.includes(word))
}

const matchesFacet = (card: CustomCard, key: FacetKey, chosen: string[]) =>
  !chosen.length || facetValuesOf(card, key).some((value) => chosen.includes(value))

/* `except` is how a facet counts its own values: the count beside "Mystic" is
 * what picking Mystic would leave, so every filter counts except the one the
 * chip belongs to. */
export function matchesFilter(card: CustomCard, filter: CardFilter, except?: FacetKey): boolean {
  if (!matchesQuery(card, filter.query)) return false
  return FACET_KEYS.every((key) => key === except || matchesFacet(card, key, filter[key]))
}

export const filterCards = <T extends CustomCard>(cards: T[], filter: CardFilter): T[] =>
  filterIsEmpty(filter) ? cards : cards.filter((card) => matchesFilter(card, filter))

function sortFacet(key: FacetKey, values: FacetValue[]): FacetValue[] {
  if (key === 'types') {
    const order = CARD_TYPES.map((t) => t.value as string)
    return values.sort((a, b) => order.indexOf(a.value) - order.indexOf(b.value))
  }
  if (key === 'classes') {
    const rank = (value: string) => {
      const at = CLASS_ORDER.indexOf(value)
      return at === -1 ? CLASS_ORDER.length : at
    }
    return values.sort((a, b) => rank(a.value) - rank(b.value) || a.value.localeCompare(b.value))
  }
  if (key === 'levels') return values.sort((a, b) => Number(a.value) - Number(b.value))
  return values.sort((a, b) => a.label.localeCompare(b.label))
}

/* The chips to show, and what each one would leave. A value nobody has any more
 * -- the last Mystic card deleted while Mystic was lit -- stays on the list at
 * zero rather than vanishing, because a filter you cannot see is a filter you
 * cannot turn off. */
export function facetsFor(cards: CustomCard[], filter: CardFilter): Facet[] {
  return FACET_KEYS.map((key) => {
    const counts = new Map<string, number>()
    for (const value of filter[key]) counts.set(value, 0)
    for (const card of cards) {
      if (!matchesFilter(card, filter, key)) continue
      for (const value of facetValuesOf(card, key)) {
        counts.set(value, (counts.get(value) ?? 0) + 1)
      }
    }
    const values = [...counts].map(([value, count]) => ({
      value,
      label: facetValueLabel(key, value),
      count,
    }))
    return { key, values: sortFacet(key, values) }
  }).filter((facet) => facet.values.length > 1 || filter[facet.key].length > 0)
}

/* Turning one chip on or off, without the caller having to know the shape. */
export function toggleFacet(filter: CardFilter, key: FacetKey, value: string) {
  const at = filter[key].indexOf(value)
  if (at === -1) filter[key].push(value)
  else filter[key].splice(at, 1)
}

export function clearFacets(filter: CardFilter) {
  for (const key of FACET_KEYS) filter[key] = []
}

export function clearFilter(filter: CardFilter) {
  filter.query = ''
  clearFacets(filter)
}
