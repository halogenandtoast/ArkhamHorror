/* Debug-only drags that land on a card in play.
 *
 * Two kinds share this module because they share every drop target and the same
 * hint UI:
 *  - `seal`: a chaos token dragged out of the bag preview, sealed on the card
 *    (backend `DebugSealChaosToken`).
 *  - `tokens`: a generic token dragged out of the debug token panel, placed on the
 *    card (backend `PlaceTokens`, which every one of these targets already handles).
 *  - `remove`: a token already on a card, dragged off it. Dropped on the token
 *    panel's trash it is removed (`RemoveTokens`); dropped on another card it moves
 *    there (`MoveTokens`). Breaches are a location field rather than a token, so they
 *    get `RemoveBreaches`/`PlaceBreaches` and only move between locations.
 *
 * Only the chaos token's *id* crosses the wire: the frontend's `ChaosToken` decodes
 * a subset of the backend record, so echoing the object back would not parse, and
 * naming it by id also makes a stale drag (a token no longer in the bag) a no-op
 * rather than a fabricated seal.
 */
import { ref, shallowRef } from 'vue'
import { useDebug } from '@/arkham/debug'
import type { ChaosToken } from '@/arkham/types/ChaosToken'

/* The card kinds whose runners keep a sealed-token pool AND render one. */
export type SealTarget =
  | { tag: 'AssetTarget'; contents: string }
  | { tag: 'EnemyTarget'; contents: string }
  | { tag: 'LocationTarget'; contents: string }
  | { tag: 'InvestigatorTarget'; contents: string }

/* Takes placed tokens (Piper's skull counts resources on it) but has no sealed
 * pool, so it refuses a seal drag. */
export type TokensOnlyTarget = { tag: 'ScenarioTarget' }

export type CardDropTarget = SealTarget | TokensOnlyTarget

/* Where a remove drag came from. Wider than `CardDropTarget` because the drag starts
 * from a token that is already on the card, so anything rendering a pool qualifies --
 * treacheries, events, stories -- not just the kinds that accept a drop. */
export type RemoveTarget = { tag: string; contents?: string }

export const assetTarget = (id: string): SealTarget => ({ tag: 'AssetTarget', contents: id })
export const enemyTarget = (id: string): SealTarget => ({ tag: 'EnemyTarget', contents: id })
export const locationTarget = (id: string): SealTarget => ({ tag: 'LocationTarget', contents: id })
export const investigatorTarget = (id: string): SealTarget =>
  ({ tag: 'InvestigatorTarget', contents: id })
export const scenarioTarget: TokensOnlyTarget = { tag: 'ScenarioTarget' }

const sameTarget = (a: RemoveTarget, b: RemoveTarget) =>
  a.tag === b.tag && a.contents === b.contents

function accepts(drop: DebugCardDrop, target: CardDropTarget): boolean {
  if (drop.kind === 'remove') {
    // Dropping a token back where it came from would be a no-op that still costs a
    // round trip, and the scenario card has no breach pool to move one into.
    if (sameTarget(drop.target, target)) return false
    return drop.token !== 'Breach' || target.tag === 'LocationTarget'
  }
  return drop.kind !== 'seal' || target.tag !== 'ScenarioTarget'
}

/* Shift takes the whole stack rather than the 5 a place drag uses: a pool is usually
 * small, and "move all the breaches" is the thing worth one gesture. */
const removeAmountFor = (drop: DebugCardDrop, event: DragEvent) =>
  drop.kind === 'remove' && event.shiftKey ? Math.max(1, drop.count) : 1

const amountForDrop = (drop: DebugCardDrop, event: DragEvent) =>
  drop.kind === 'remove' ? removeAmountFor(drop, event) : amountFor(event)

/* The generic tokens the debug panel offers. Deliberately the small set that means
 * the same thing on every card, rather than all of `TOKENS`. */
export const PLACEABLE_TOKENS = ['Resource', 'Clue', 'Doom', 'Horror', 'Damage'] as const
export type PlaceableToken = (typeof PLACEABLE_TOKENS)[number]

/* `PoolItem`'s image keys for the tokens above. */
export const TOKEN_POOL_TYPE: Record<PlaceableToken, string> = {
  Resource: 'resource',
  Clue: 'clue',
  Doom: 'doom',
  Horror: 'horror',
  Damage: 'health',
}

export type DebugCardDrop =
  | { kind: 'seal'; chaosToken: ChaosToken }
  | { kind: 'tokens'; token: PlaceableToken }
  /* `token` is the backend `Token` tag as the card reports it, or the pseudo-tag
   * `Breach`, which is a location field rather than a token. `count` is how many were
   * on the card when the drag started, which is what shift takes. */
  | { kind: 'remove'; target: RemoveTarget; token: string; count: number }

/* A card with printed uses takes those instead of bare resources: a resource
 * dropped on a Flashlight is a supply, on a .45 Automatic an ammo. Read off the
 * card def, which is also what the hint reads, so what it promises and what the
 * drop sends cannot drift apart. Customizations that change a use type are not
 * followed -- the printed type is the debug-useful answer. */
export function resolveToken(drop: DebugCardDrop, useType: string | null): string | null {
  if (drop.kind !== 'tokens') return null
  return drop.token === 'Resource' && useType ? useType : drop.token
}

/* What is in flight, if anything.
 *
 * `dataTransfer.getData` returns '' during dragover in every browser -- the payload
 * is only readable on drop -- so a drop zone that wants to light up while the drag
 * is still in the air has to read it from here instead. Same reason
 * `debugCardMove` keeps `draggedCardId`.
 */
// Shallow: nothing mutates a drop, and a deep ref hands back a proxy, so identity
// checks against the object passed to `beginDrag` would never match.
const draggedDrop = shallowRef<DebugCardDrop | null>(null)

/* Viewport position of the hint saying what the drop will do, or null when no card
 * is under the cursor. The hint is `position: fixed`, so these are client
 * coordinates -- mirrors `Draw.vue`'s `deckDropPosition`. */
const dropPosition = ref<{ x: number; y: number } | null>(null)

/* How many tokens the drop would place, so the hint can say so before the drop. */
const dropAmount = ref(1)

/* Whether the pools may show the dragged token as already gone. Held off until after
 * `dragstart` returns: taking the last token off a card empties its pool item, and a
 * drag source that disappears in the same tick the drag starts makes the browser
 * abort the drag -- which is why a single token could not be picked up at all. */
const previewActive = ref(false)

/* The hovered card's own use type, when it has one, so the hint can name it. */
const dropUseType = ref<string | null>(null)

/* Shift places five at a time. Read per event rather than captured at dragstart, so
 * pressing or releasing shift mid-drag updates the hint. */
const amountFor = (event: DragEvent) => (event.shiftKey ? 5 : 1)

/* A remove drag carries its amount and its hint wherever the cursor is, not just over
 * a drop zone: `dragover` fires on whatever is under the cursor and bubbles to the
 * window, and the drag model re-fires it a few times a second even while the pointer
 * is still -- so pressing shift picks the whole stack up on the spot. */
function trackRemoveDrag(event: DragEvent) {
  const drop = draggedDrop.value
  if (drop?.kind !== 'remove') return
  dropAmount.value = removeAmountFor(drop, event)
  dropPosition.value = { x: event.clientX, y: event.clientY }
}

function beginDrag(drop: DebugCardDrop) {
  draggedDrop.value = drop
  window.addEventListener('dragend', endCardDrag, { once: true })
  window.addEventListener('drop', endCardDrag, { once: true })
  if (drop.kind === 'remove') {
    window.addEventListener('dragover', trackRemoveDrag)
    setTimeout(() => { if (draggedDrop.value === drop) previewActive.value = true }, 0)
  }
}

export const beginSealDrag = (chaosToken: ChaosToken) => beginDrag({ kind: 'seal', chaosToken })
export const beginTokenDrag = (token: PlaceableToken) => beginDrag({ kind: 'tokens', token })

/* Attrs for a token already on a card, spread with `v-bind`, so it can be dragged off
 * onto the trash. Callers gate this on `debug.active` -- undebugged play must not make
 * the pool draggable. */
export function removeDragAttrs(target: RemoveTarget, token: string, count: number) {
  return {
    draggable: true,
    onDragstart: (event: DragEvent) => {
      // Stop the card underneath from starting its own move drag instead.
      event.stopPropagation()
      if (event.dataTransfer) {
        event.dataTransfer.effectAllowed = 'move'
        /* The default drag image is a copy of the whole pool item, count badge and
         * all -- which reads as dragging the stack. The item's own <img> is the bare
         * token art and is already in the document, so hand that over instead. */
        const art = (event.currentTarget as HTMLElement | null)?.querySelector('img')
        if (art) event.dataTransfer.setDragImage(art, art.width / 2, art.height / 2)
      }
      beginDrag({ kind: 'remove', target, token, count })
    },
    onDragend: endCardDrag,
  }
}

/* How many of `token` are in the air off `target` right now -- 1 while it is being
 * dragged, 0 otherwise. Pools subtract it so the token visibly leaves the card while
 * the drag is in flight (and disappears when it was the last one). Purely visual; the
 * drop on the trash is what actually removes it. */
/* A dropped remove keeps its preview until the server's new state lands. Dropping the
 * hold at drop time puts the token back on the card for the length of the round trip
 * and then takes it away again, which reads as a glitch.
 *
 * The hold releases itself: it only applies while the pool still reports the count it
 * had at drop, so the moment the real update arrives it stops matching. The timer is
 * just garbage collection, so a send that never lands cannot hide a token for good. */
const pending = shallowRef<{ target: RemoveTarget; token: string; count: number; amount: number } | null>(null)
let pendingTimer: ReturnType<typeof setTimeout> | null = null

function holdPreview(drop: DebugCardDrop, amount: number) {
  if (drop.kind !== 'remove') return
  pending.value = { target: drop.target, token: drop.token, count: drop.count, amount }
  if (pendingTimer) clearTimeout(pendingTimer)
  pendingTimer = setTimeout(() => { pending.value = null }, 3000)
}

export function draggedOff(target: RemoveTarget | undefined, token: string, count: number): number {
  if (!target) return 0
  const held = pending.value
  if (held && held.token === token && held.count === count && sameTarget(held.target, target)) {
    return held.amount
  }
  const drop = draggedDrop.value
  if (!drop || drop.kind !== 'remove' || !previewActive.value) return 0
  if (!sameTarget(drop.target, target) || drop.token !== token) return 0
  return Math.min(dropAmount.value, drop.count)
}

/* The `Source` matching a target, for the `from` side of a move. Every one of these is
 * the same name with the suffix swapped. */
const sourceOf = (target: RemoveTarget) => ({
  tag: target.tag.replace(/Target$/, 'Source'),
  ...(target.contents === undefined ? {} : { contents: target.contents }),
})

export function endCardDrag() {
  window.removeEventListener('dragover', trackRemoveDrag)
  previewActive.value = false
  draggedDrop.value = null
  dropPosition.value = null
  dropAmount.value = 1
  dropUseType.value = null
}

/* True while either kind of drag is in flight.
 *
 * The debug card-move drop zones (hand, play area, decks, discards, cards-under)
 * call `preventDefault` on every dragover, so without asking this they advertise a
 * drop they then silently ignore -- a `+` cursor over a hand card that seals
 * nothing. They answer `dropEffect = 'none'` instead when this is true.
 */
export function cardDropInFlight(): boolean {
  return draggedDrop.value !== null
}

/* `target` is null only for a remove dropped on the trash, which has no destination. */
function send(
  gameId: string,
  drop: DebugCardDrop,
  target: CardDropTarget | null,
  amount: number,
  useType: string | null,
) {
  const debug = useDebug()
  if (drop.kind === 'remove') {
    // Breaches are a location field, not a token, so they have their own messages and
    // no MoveTokens equivalent. `Run` keeps the pair in one queue pass, which is what
    // makes the move a single step: undone message by message, one undo would leave the
    // breach removed from one location and never placed on the other.
    if (drop.token === 'Breach') {
      const remove = { tag: 'RemoveBreaches', contents: [drop.target, amount] }
      if (!target) return debug.send(gameId, remove)
      return debug.send(gameId, {
        tag: 'Run',
        contents: [remove, { tag: 'PlaceBreaches', contents: [target, amount] }],
      })
    }
    const contents = target
      ? [{ tag: 'GameSource' }, sourceOf(drop.target), target, drop.token, amount]
      : [{ tag: 'GameSource' }, drop.target, drop.token, amount]
    return debug.send(gameId, {
      tag: 'TokenMessage',
      contents: { tag: target ? 'MoveTokens_' : 'RemoveTokens_', contents },
    })
  }
  if (drop.kind === 'seal') {
    return debug.send(gameId, { tag: 'DebugSealChaosToken', contents: [drop.chaosToken.id, target] })
  }
  return debug.send(gameId, {
    tag: 'TokenMessage',
    contents: {
      tag: 'PlaceTokens_',
      contents: [{ tag: 'GameSource' }, target, resolveToken(drop, useType), amount],
    },
  })
}

/* The token half of a drop without the drag, so the hover keybindings can place the
 * same tokens from the keyboard. It goes through `send` rather than building its own
 * message, so the two paths cannot drift apart.
 *
 * `useType` defaults to null: a dragged resource becomes the card's printed use
 * (ammo, supplies) because the drop zone reads that off the hovered card's def, and
 * a keypress has no such lookup. A bare Resource is the honest answer here.
 */
export function placeTokensOn(
  gameId: string,
  target: CardDropTarget,
  token: PlaceableToken,
  amount: number,
  useType: string | null = null,
) {
  return send(gameId, { kind: 'tokens', token }, target, amount, useType)
}

/* Listeners for the trash can, spread with `v-bind`. Deliberately narrow: it takes
 * remove drags only, so a token dragged out of the panel and back into it is a no-op
 * rather than a place-then-remove. */
export function trashDropHandlers(gameId: string) {
  const over = (event: DragEvent) => {
    const drop = draggedDrop.value
    if (drop?.kind !== 'remove') return
    event.preventDefault()
    // Deliberately no stopPropagation: the window-level tracker is what reads shift and
    // moves the hint, and it only sees events that reach the window.
    if (event.dataTransfer) event.dataTransfer.dropEffect = 'move'
  }

  return {
    onDragenter: over,
    onDragover: over,
    onDrop: (event: DragEvent) => {
      const drop = draggedDrop.value
      if (drop?.kind !== 'remove') return
      event.preventDefault()
      event.stopPropagation()
      const amount = removeAmountFor(drop, event)
      endCardDrag()
      holdPreview(drop, amount)
      send(gameId, drop, null, amount, null)
    },
  }
}

/* Listeners for a card that accepts these drags, spread with `v-bind`.
 *
 * `dragover` must call `preventDefault` or the browser refuses the drop, and it only
 * does so while one of our drags is actually in flight -- otherwise a card would
 * swallow the card-moving drags that `debugCardMove` owns.
 */
export function cardDropHandlers(
  gameId: string,
  target: () => CardDropTarget,
  useType: () => string | null = () => null,
) {
  const over = (event: DragEvent) => {
    const drop = draggedDrop.value
    if (!drop) return
    if (!accepts(drop, target())) {
      if (event.dataTransfer) event.dataTransfer.dropEffect = 'none'
      if (drop.kind !== 'remove') {
        dropPosition.value = null
        dropAmount.value = 1
      }
      return
    }
    event.preventDefault()
    event.stopPropagation()
    /* Must agree with the drag's `effectAllowed` or the browser cancels the drop
     * without firing it: a remove drag moves, the other two copy. */
    if (event.dataTransfer) {
      event.dataTransfer.dropEffect = drop.kind === 'remove' ? 'move' : 'copy'
    }
    dropPosition.value = { x: event.clientX, y: event.clientY }
    dropAmount.value = amountForDrop(drop, event)
    dropUseType.value = useType()
  }

  return {
    onDragenter: over,
    onDragover: over,
    // Moving onto a child fires dragleave on the parent, so ignore the ones that
    // stay inside this card or the hint flickers as the cursor crosses the art.
    onDragleave: (event: DragEvent) => {
      const from = event.currentTarget
      const to = event.relatedTarget
      if (from instanceof Node && to instanceof Node && from.contains(to)) return
      if (draggedDrop.value?.kind === 'remove') return
      dropPosition.value = null
      dropAmount.value = 1
    },
    onDrop: (event: DragEvent) => {
      const drop = draggedDrop.value
      if (!drop || !accepts(drop, target())) return
      event.preventDefault()
      event.stopPropagation()
      const amount = amountForDrop(drop, event)
      const uses = useType()
      endCardDrag()
      holdPreview(drop, amount)
      send(gameId, drop, target(), amount, uses)
    },
  }
}

export { draggedDrop, dropPosition, dropAmount, dropUseType }
