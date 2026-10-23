import { computed, inject, toValue, type InjectionKey, type MaybeRefOrGetter, type Ref } from 'vue'
import { cardId, type Card } from '@/arkham/types/Card'
import { choices, type Game } from '@/arkham/types/Game'

export type PendingDraw = {
  investigatorId: string
  card: Card
}

/* An encounter card an investigator has drawn but not yet resolved.
 *
 * The engine focuses the card for exactly the length of the `when you draw`
 * window (`FocusCards` ... `whenDraw` ... `UnfocusCards` in Game/Runner.hs), so
 * whoever may cancel it -- Ward of Protection -- can see what was drawn. No
 * entity exists for it yet: it is in limbo until the revelation resolves. The
 * play area renders these beside the threat area, which is why the focused-card
 * modal leaves them out.
 *
 * The window payload is the only place the drawing investigator and the card
 * are named together, and it arrives in more than one shape -- on an
 * `AbilityLabel` choice's own `windows`, or buried in the window list a
 * play-from-hand message carries -- so this walks each choice for anything
 * shaped like a `When`-timed `DrawCard` window instead of reaching for a fixed
 * path. Only cards that are ALSO focused are returned, which is what pins the
 * result to the span the engine focused them for; a stale window mentioned by
 * some other message names a card that is no longer on the table.
 *
 * It can only see what the decoders kept. `Label` drops its messages, so the
 * Foresight prompt -- two plain labels, where the modal IS the question --
 * matches nothing and keeps its card, which is what should happen there.
 */
export function pendingDraws(game: Game, playerId: string): PendingDraw[] {
  let byPlayer = cache.get(game)
  if (!byPlayer) {
    byPlayer = new Map()
    cache.set(game, byPlayer)
  }
  const known = byPlayer.get(playerId)
  if (known) return known

  const value = build(game, playerId)
  byPlayer.set(playerId, value)
  return value
}

const cache = new WeakMap<Game, Map<string, PendingDraw[]>>()

function build(game: Game, playerId: string): PendingDraw[] {
  const focused = new Map(game.focusedCards.map((card) => [cardId(card), card]))
  if (focused.size === 0) return []

  const found = new Map<string, PendingDraw>()

  const walk = (value: unknown, depth: number) => {
    if (depth > 12 || value === null || typeof value !== 'object') return

    if (Array.isArray(value)) {
      for (const entry of value) walk(entry, depth + 1)
      return
    }

    const record = value as Record<string, unknown>
    const draw = drawWindow(record)
    if (draw) {
      const card = focused.get(draw.cardId)
      if (card && !found.has(draw.cardId)) {
        found.set(draw.cardId, { investigatorId: draw.investigatorId, card })
      }
    }

    for (const entry of Object.values(record)) walk(entry, depth + 1)
  }

  for (const choice of choices(game, playerId)) walk(choice, 0)

  return [...found.values()]
}

/* `{windowTiming: "When", windowType: {tag: "DrawCard", contents: [iid, card, deck]}}`,
 * read defensively: a choice's nested messages are not decoded, so every step of
 * the way down is unknown JSON. */
function drawWindow(record: Record<string, unknown>): { investigatorId: string; cardId: string } | null {
  if (record.windowTiming !== 'When') return null
  const windowType = record.windowType
  if (!windowType || typeof windowType !== 'object') return null
  const { tag, contents } = windowType as { tag?: unknown; contents?: unknown }
  if (tag !== 'DrawCard' || !Array.isArray(contents)) return null

  const investigatorId = contents[0]
  const card = contents[1]
  if (typeof investigatorId !== 'string' || !card || typeof card !== 'object') return null

  const cardContents = (card as { contents?: unknown }).contents
  if (!cardContents || typeof cardContents !== 'object') return null
  const id = (cardContents as { id?: unknown }).id
  if (typeof id !== 'string') return null

  return { investigatorId, cardId: id }
}

/* Game.vue's copy, for the board.
 *
 * While a revelation overlay holds the UI lock the question is blanked out of
 * the board's game (`applyGameUpdate`), so the board cannot derive a draw for
 * itself -- and that is exactly when it needs one, because the card flying out
 * of the overlay has to have somewhere laid out to land. Game.vue reads the
 * draws off the update before the blanking and holds them here.
 */
export const PENDING_DRAWS = Symbol('pendingDraws') as InjectionKey<Ref<PendingDraw[]>>

/* The draws to render: what the board's own game says, and only when that says
 * nothing -- the overlay has blanked the question -- what Game.vue held.
 *
 * That order matters because the board's game is also set from places no update
 * passes through (the first load, a refetch, an undo), where the held value is
 * whatever was last seen. Either way the result is vouched for by the game: a
 * draw survives only while its card is focused, which is the span the engine
 * focuses it for and so the span it is really mid-draw. */
export function usePendingDraws(game: MaybeRefOrGetter<Game>, playerId: MaybeRefOrGetter<string>) {
  const held = inject(PENDING_DRAWS, null)

  return computed(() => {
    const value = toValue(game)
    const own = pendingDraws(value, toValue(playerId))
    const draws = own.length > 0 ? own : (held?.value ?? [])
    const focused = new Set(value.focusedCards.map(cardId))
    return draws.filter((draw) => focused.has(cardId(draw.card)))
  })
}
