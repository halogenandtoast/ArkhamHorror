import { cardArt } from '@/arkham/cardImages'

/* Cut-out standees shipped in public/img/scenes/actors, taken from the offline demo. A figure without one is
 * drawn from a crop of its card art instead. */
const SPRITE_BY_CODE: Record<string, string> = {
  '01001': 'roland', '01159': 'rats', '01116': 'priest', '01157': 'umordhoth', '01172': 'nightgaunt', '01175': 'nightgaunt',
}

export function spriteFor(code: string, traits: string): string | null {
  const art = cardArt(code)
  const named = SPRITE_BY_CODE[art]
  if (named) return `img/scenes/actors/${named}.webp`
  if (traits.includes('Cultist')) return 'img/scenes/actors/cultist.webp'
  if (traits.includes('Ghoul')) return 'img/scenes/actors/ghoul.webp'
  return null
}
