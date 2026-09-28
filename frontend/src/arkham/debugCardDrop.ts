/* Debug-only drags that land on a card in play.
 *
 * Two kinds share this module because they share every drop target and the same
 * hint UI:
 *  - `seal`: a chaos token dragged out of the bag preview, sealed on the card
 *    (backend `DebugSealChaosToken`).
 *  - `tokens`: a generic token dragged out of the debug token panel, placed on the
 *    card (backend `PlaceTokens`, which every one of these targets already handles).
 *
 * Only the chaos token's *id* crosses the wire: the frontend's `ChaosToken` decodes
 * a subset of the backend record, so echoing the object back would not parse, and
 * naming it by id also makes a stale drag (a token no longer in the bag) a no-op
 * rather than a fabricated seal.
 */
import { ref } from 'vue'
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

export const assetTarget = (id: string): SealTarget => ({ tag: 'AssetTarget', contents: id })
export const enemyTarget = (id: string): SealTarget => ({ tag: 'EnemyTarget', contents: id })
export const locationTarget = (id: string): SealTarget => ({ tag: 'LocationTarget', contents: id })
export const investigatorTarget = (id: string): SealTarget =>
  ({ tag: 'InvestigatorTarget', contents: id })
export const scenarioTarget: TokensOnlyTarget = { tag: 'ScenarioTarget' }

function accepts(drop: DebugCardDrop, target: CardDropTarget): boolean {
  return drop.kind !== 'seal' || target.tag !== 'ScenarioTarget'
}

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
const draggedDrop = ref<DebugCardDrop | null>(null)

/* Viewport position of the hint saying what the drop will do, or null when no card
 * is under the cursor. The hint is `position: fixed`, so these are client
 * coordinates -- mirrors `Draw.vue`'s `deckDropPosition`. */
const dropPosition = ref<{ x: number; y: number } | null>(null)

/* How many tokens the drop would place, so the hint can say so before the drop. */
const dropAmount = ref(1)

/* The hovered card's own use type, when it has one, so the hint can name it. */
const dropUseType = ref<string | null>(null)

/* Shift places five at a time. Read per event rather than captured at dragstart, so
 * pressing or releasing shift mid-drag updates the hint. */
const amountFor = (event: DragEvent) => (event.shiftKey ? 5 : 1)

function beginDrag(drop: DebugCardDrop) {
  draggedDrop.value = drop
  window.addEventListener('dragend', endCardDrag, { once: true })
  window.addEventListener('drop', endCardDrag, { once: true })
}

export const beginSealDrag = (chaosToken: ChaosToken) => beginDrag({ kind: 'seal', chaosToken })
export const beginTokenDrag = (token: PlaceableToken) => beginDrag({ kind: 'tokens', token })

export function endCardDrag() {
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

function send(
  gameId: string,
  drop: DebugCardDrop,
  target: CardDropTarget,
  amount: number,
  useType: string | null,
) {
  const debug = useDebug()
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
      dropPosition.value = null
      return
    }
    event.preventDefault()
    event.stopPropagation()
    if (event.dataTransfer) event.dataTransfer.dropEffect = 'copy'
    dropPosition.value = { x: event.clientX, y: event.clientY }
    dropAmount.value = amountFor(event)
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
      dropPosition.value = null
    },
    onDrop: (event: DragEvent) => {
      const drop = draggedDrop.value
      if (!drop || !accepts(drop, target())) return
      event.preventDefault()
      event.stopPropagation()
      const amount = amountFor(event)
      const uses = useType()
      endCardDrag()
      send(gameId, drop, target(), amount, uses)
    },
  }
}

export { draggedDrop, dropPosition, dropAmount, dropUseType }
