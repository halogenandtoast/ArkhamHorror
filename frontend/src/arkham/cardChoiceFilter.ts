export interface CardChoiceCandidate {
  index: number
  code: string
  name: string
  subtitle: string
  classes: string[]
  type: string
  level: number | null
  traits: string[]
  text: string
  pack: string
  custom: boolean
}

export interface CardChoiceFilters {
  query: string
  className: string
  type: string
  level: string
  trait: string
  pack: string
  includeCustom: boolean
  sort: string
}

export function filterCardChoices<T extends CardChoiceCandidate>(
  cards: T[],
  filters: CardChoiceFilters,
): T[] {
  const terms = filters.query.trim().toLocaleLowerCase().split(/\s+/).filter(Boolean)
  const filtered = cards.filter((card) => {
    if (card.custom && !filters.includeCustom) return false
    if (filters.className && !card.classes.includes(filters.className)) return false
    if (filters.type && card.type !== filters.type) return false
    if (filters.level && String(card.level ?? '') !== filters.level) return false
    if (filters.trait && !card.traits.includes(filters.trait)) return false
    if (filters.pack && card.pack !== filters.pack) return false
    const searchable = [card.name, card.subtitle, card.code, card.text, ...card.traits]
      .join(' ')
      .toLocaleLowerCase()
    return terms.every((term) => searchable.includes(term))
  })
  if (filters.sort === 'original') return filtered
  return filtered.sort((a, b) => {
    if (filters.sort === 'level') {
      const byLevel = (a.level ?? Infinity) - (b.level ?? Infinity)
      if (byLevel) return byLevel
    }
    return a.name.localeCompare(b.name) || a.code.localeCompare(b.code) || a.index - b.index
  })
}
