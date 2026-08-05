/* The Doom of Arkham Pt II. The three Cthulhu facets are real enemies standing at
 * Cthulhu (Ancient Evil)'s location, and are engaged with the investigators there —
 * "engaged with Cthulhu (Ancient Evil) ... as a single enemy" — so they would
 * otherwise show up in threat areas and in the location's enemy row. They belong on
 * the Cthulhu Board instead, so every display filters them out by card code.
 *
 * Live entity card codes are 'c'-prefixed. Both faces of each facet share a slot:
 * a facet flipped to its Enraged side is a different card code but the same card. */

import type { Placement } from '@/arkham/types/Placement'

export const CTHULHU_BOARD_SLOTS: string[][] = [
  ['c11702', 'c11702b'], // Hoary Wings
  ['c11703', 'c11703b'], // Fierce Visage
  ['c11704', 'c11704b'], // Wicked Claw
]

export const CTHULHU_BOARD_CODES: string[] = CTHULHU_BOARD_SLOTS.flat()

export const isCthulhuBoardEnemy = (cardCode: string): boolean =>
  CTHULHU_BOARD_CODES.includes(cardCode)

/* Whether a facet is currently standing on the board.
 *
 * A defeated enemy is not deleted from `game.enemies` — `RemoveEnemy` only sets its
 * placement to `OutOfPlay` — so a banished facet keeps its card code and would go on
 * occupying its slot forever, showing on the board at the same time as in the victory
 * display. Testing for `OutOfPlay` rather than for a specific in-play placement keeps
 * this agnostic about where a facet actually sits while in play. */
export const isCthulhuBoardEnemyInPlay = (enemy: { cardCode: string; placement: Placement }): boolean =>
  isCthulhuBoardEnemy(enemy.cardCode) && enemy.placement.tag !== 'OutOfPlay'
