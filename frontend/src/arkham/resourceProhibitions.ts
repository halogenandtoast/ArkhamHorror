import { cardArt, sourceCardCode } from '@/arkham/cardImages'
import { logRefName, type DbCards } from '@/arkham/logRefName'
import { toCardContents } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'
import type { Investigator } from '@/arkham/types/Investigator'
import type { Modifier } from '@/arkham/types/Modifier'

/* What stops an investigator gaining resources, and what is doing it.
 *
 * The sibling of clueProhibitions.ts, with one difference worth knowing: the
 * clue lists are built by the server, while these are read straight off the
 * investigator's own `modifiers`, which already reach the client with their
 * source attached. Nothing new had to be sent.
 *
 * Same rule for what belongs here: flat bans only. "You have none" is the count
 * beside it, and a cost you cannot currently afford is not a prohibition.
 *
 * Nullary modifiers decode through the catch-all in types/Modifier.ts, so they
 * arrive as OtherModifier carrying the constructor name.
 */

const CANNOT_GAIN = 'CannotGainResources'
const CANNOT_GAIN_FROM_PLAYER_CARDS = 'CannotGainResourcesFromPlayerCardEffects'

function otherModifiers(investigator: Investigator, tag: string): Modifier[] {
  return (investigator.modifiers ?? []).filter(
    (m) => m.type.tag === 'OtherModifier' && m.type.contents === tag,
  )
}

export interface ResourceProhibition {
  /* Every gain, from any source. */
  all: Modifier[]
  /* Only gains from player card effects -- a scenario's own effects still pay. */
  playerCards: Modifier[]
}

export function resourceProhibition(investigator: Investigator): ResourceProhibition {
  return {
    all: otherModifiers(investigator, CANNOT_GAIN),
    playerCards: otherModifiers(investigator, CANNOT_GAIN_FROM_PLAYER_CARDS),
  }
}

export function isResourceProhibited(p: ResourceProhibition): boolean {
  return p.all.length > 0 || p.playerCards.length > 0
}

/* The outright ban first: it is the one that explains a gain going nowhere
 * whatever caused it, so it is what a player staring at an unchanged pool is
 * most likely asking about. */
function ordered(p: ResourceProhibition): Modifier[] {
  return [...p.all, ...p.playerCards]
}

/* The art id for a modifier's source, as Modifier.vue resolves it: the card it
 * came in on if it has one, otherwise whatever entity the source names. */
function modifierArt(modifier: Modifier, game: Game): string | null {
  if (modifier.card) return cardArt(toCardContents(modifier.card).cardCode)
  return sourceCardCode(modifier.source, game)
}

export function resourceProhibitionImage(
  p: ResourceProhibition,
  game: Game,
): string | undefined {
  for (const modifier of ordered(p)) {
    const art = modifierArt(modifier, game)
    if (art) return art
  }
  return undefined
}

/* An art id is a card code with the leading 'c' stripped and sometimes a suffix
 * added (a face-down 'b', a mutation), so 'logRefName' can resolve most of them
 * -- it strips the same prefix and retries without a trailing side letter. What
 * it cannot resolve comes back as the code itself, which is worse than saying
 * nothing, so those are dropped and the sentence goes nameless. */
function sourceName(modifier: Modifier, game: Game, dbCards: DbCards): string | null {
  const art = modifierArt(modifier, game)
  if (!art) return null
  const name = logRefName({ cardCode: art, name: art }, dbCards)
  return name === art ? null : name
}

function names(modifiers: Modifier[], game: Game, dbCards: DbCards): string[] {
  const resolved = modifiers.flatMap((m) => {
    const name = sourceName(m, game, dbCards)
    return name ? [name] : []
  })
  return [...new Set(resolved)]
}

function sentence(
  modifiers: Modifier[],
  key: string,
  game: Game,
  dbCards: DbCards,
  t: (key: string, named?: Record<string, unknown>) => string,
): string {
  const source = names(modifiers, game, dbCards)
  return source.length === 0
    ? t(`player.${key}`)
    : t(`player.${key}Because`, { source: source.join(', ') })
}

/* One sentence per thing that is forbidden. An outright ban already covers the
 * narrower one, so there is no point listing both. */
export function resourceProhibitionTitle(
  p: ResourceProhibition,
  game: Game,
  dbCards: DbCards,
  t: (key: string, named?: Record<string, unknown>) => string,
): string {
  if (p.all.length > 0) return sentence(p.all, 'cannotGainResources', game, dbCards, t)
  return sentence(p.playerCards, 'cannotGainResourcesFromCards', game, dbCards, t)
}
