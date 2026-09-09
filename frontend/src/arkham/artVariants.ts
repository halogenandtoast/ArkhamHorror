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

// Preference order is significant; an unavailable variant falls through to
// the next choice, then to the original. Never infer an unprovided face.
export function variantArt(art: string, useVariants: readonly string[]): string {
  const variants = variantsByArt.value.get(art)
  for (const variant of useVariants) {
    if (variants?.[variant]) return variants[variant].replace(/^c/, '')
  }
  return art
}
