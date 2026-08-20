// Which art a CardDef shows on each side. Shared by CardImage (one side at a
// time, with a flip) and CardDetailsModal (both sides at once).
import { cardImg, imgsrc } from '@/arkham/helpers'
import type { CardDef } from '@/arkham/types/CardDef'

const ENCOUNTER_BACK = 'backs/back_encounter.jpg'
const PLAYER_BACK = 'backs/back_player.jpg'

// A double-sided card whose OWN art is the 'b' side (e.g. an enemy that is the
// back of an agenda: art "…016b", otherSide "…016"). Its front is the other
// side, so that is what we show, and its own art goes on the back.
export function isBackPrimary(card: CardDef): boolean {
  const { otherSide, doubleSided, art } = card
  return !!(doubleSided && otherSide && /b$/.test(art)
    && otherSide.replace(/^c/, '') === art.replace(/b$/, ''))
}

export function cardFrontImage(card: CardDef): string {
  if (card.cardType === 'LocationType' && card.doubleSided) return cardImg(`${card.art}b`)
  if (isBackPrimary(card)) return cardImg(card.otherSide!.replace(/^c/, ''))
  return cardImg(card.art)
}

export function cardBackImage(card: CardDef): string {
  const { cardType, otherSide, doubleSided } = card

  if (isBackPrimary(card)) return cardImg(card.art)
  if (otherSide) return cardImg(otherSide.replace(/^c/, ''))

  // A card we have no def for (an unimplemented placeholder) has no type to
  // derive a back from. Its 'b' art, if any, is the back; callers drop the back
  // when the image turns out not to exist.
  if (!cardType) return cardImg(`${card.art}b`)

  if (['ActType', 'AgendaType', 'ScenarioType', 'InvestigatorType'].includes(cardType))
    return cardImg(`${card.art.replace(/a$/, '')}b`)

  if (cardType === 'LocationType') {
    if (doubleSided) return cardImg(card.art)
    return imgsrc(ENCOUNTER_BACK)
  }

  if (['EnemyType', 'StoryType'].includes(cardType) && doubleSided) return cardImg(`${card.art}b`)

  if (doubleSided) return cardImg(`${card.art.replace(/a$/, '')}b`)

  if (['EnemyType', 'StoryType', 'TreacheryType', 'EncounterAssetType', 'EncounterEventType'].includes(cardType)) {
    if (card.meta?.customBack) return imgsrc(`backs/${card.meta.customBack}`)
    return imgsrc(ENCOUNTER_BACK)
  }

  // Player-type cards (e.g. earned Artifact assets) may also define a custom back.
  if (card.meta?.customBack) return imgsrc(`backs/${card.meta.customBack}`)

  return imgsrc(PLAYER_BACK)
}

// Whether the back is art of its own rather than a generic card back, i.e.
// whether it is worth showing alongside the front.
export function hasCardBackArt(card: CardDef): boolean {
  const back = cardBackImage(card)
  return back !== imgsrc(ENCOUNTER_BACK) && back !== imgsrc(PLAYER_BACK)
}

// A handful of cards store their front art as an 'a' side (13093a) rather than
// under the bare code. Worth one retry before showing a broken image.
export function altFrontImage(src: string): string | null {
  const alt = src.replace(/(\d)\.avif$/, '$1a.avif')
  return alt === src ? null : alt
}
