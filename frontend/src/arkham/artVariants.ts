import { shallowRef } from 'vue'
import type { CardDef } from '@/arkham/types/CardDef'

type Variants = Record<string, string>
const variantsByArt = shallowRef(new Map<string, Variants>())
const originalsByVariant = shallowRef(new Map<string, string>())

// Index by printed art, not entity id: this also covers payloads containing only
// an Art field, resolved agendas, previews, and sources after leaving play.
export function registerArtVariants(cards: CardDef[]): void {
  const index = new Map(variantsByArt.value)
  for (const card of cards) {
    if (card.artVariants && Object.keys(card.artVariants).length) {
      index.set(card.art.replace(/^c/, ''), card.artVariants)
    }
    if (card.backArtVariants && Object.keys(card.backArtVariants).length) {
      const back = `${card.art.replace(/^c/, '').replace(/[aceg]$/, '')}b`
      index.set(back, card.backArtVariants)
    }
  }
  variantsByArt.value = index
  const originals = new Map<string, string>()
  for (const [original, variants] of index) {
    for (const art of Object.values(variants)) originals.set(art.replace(/^c/, ''), original)
  }
  originalsByVariant.value = originals
}

// Image URLs may name an art-only printing with no rules/database entry.
export function originalArt(art: string): string {
  return originalsByVariant.value.get(art) ?? art
}

/* The Revised Core Set reprints the original's encounter cards under numbers
 * 500 higher, but only the few whose art actually changed have a picture of
 * their own -- and those are exactly the ones a card declares as a variant. Any
 * other number in that range is a reprint of the same picture, so it reads the
 * original's image rather than asking for one that was never published. */
const REVISED_CORE_ENCOUNTER = { min: 1604, max: 1682, offset: 500 }

export function reprintedArt(art: string): string {
  const match = art.match(/^(\d+)([a-z]*)$/)
  if (!match) return art

  const number = parseInt(match[1])
  if (number < REVISED_CORE_ENCOUNTER.min || number > REVISED_CORE_ENCOUNTER.max) return art
  if (originalsByVariant.value.has(art)) return art

  return `${String(number - REVISED_CORE_ENCOUNTER.offset).padStart(5, '0')}${match[2]}`
}

// Preference order is significant; an unavailable variant falls through to
// the next choice, then to the original. Never infer an unprovided face.
export function variantArt(art: string, useVariants: readonly string[]): string {
  const variants = variantsByArt.value.get(art)
  for (const variant of useVariants) {
    if (variants?.[variant]) return variants[variant].replace(/^c/, '')
  }
  return art
}
