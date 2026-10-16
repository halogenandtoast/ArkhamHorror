/* Debug-only drags of a card that is already in play, and the engine messages
 * behind them.
 *
 * Unlike `debugCardMove`, which moves a *card* between zones with
 * `DebugMoveCard`, these send the real gameplay message -- `ReturnToHand` or
 * `Discard` -- because what is being dragged is an entity, not a card sitting in
 * a pile. Leaving play is the whole point: the asset has to hand its sealed
 * tokens back, send what sits underneath it to the discard and raise its
 * leave-play windows. `DebugMoveCard` would obtain the card out from under a
 * live entity and leave the entity itself in play.
 */
import { shallowRef } from 'vue'

/* The in-play entities `ReturnToHand` has a handler for (Game/Runner.hs).
 *
 * `Discard` is narrower: only the asset runner answers it for one of these, so
 * the discard pile takes assets alone -- an event or skill dropped there would
 * fall through the runner's catch-all and silently do nothing. */
export type InPlayTarget =
  | { tag: 'AssetTarget'; contents: string }
  | { tag: 'EventTarget'; contents: string }
  | { tag: 'SkillTarget'; contents: string }

export const assetTarget = (id: string): InPlayTarget => ({ tag: 'AssetTarget', contents: id })
export const eventTarget = (id: string): InPlayTarget => ({ tag: 'EventTarget', contents: id })
export const skillTarget = (id: string): InPlayTarget => ({ tag: 'SkillTarget', contents: id })

export const canDiscard = (target: InPlayTarget) => target.tag === 'AssetTarget'

/* `iid` names whose hand the card lands in, not whose card it was: dropping
 * another investigator's asset on my hand hands it to me. */
export const returnToHandMessage = (investigatorId: string, target: InPlayTarget) => ({
  tag: 'ReturnToHand',
  contents: [investigatorId, target],
})

/* The discarding investigator is left null -- as the debug asset panel's own
 * Discard button does -- so the card goes to its owner's discard rather than to
 * whichever pile it was dropped on. */
export const discardMessage = (target: InPlayTarget) => ({
  tag: 'Discard',
  contents: [null, { tag: 'GameSource' }, target],
})

/* What is being dragged, if anything.
 *
 * `dataTransfer.getData` returns '' during dragover in every browser -- the
 * payload is only readable on the drop itself -- so a drop zone that wants to
 * light up while the card is still in the air has to read it from here instead.
 * Same reason `debugCardMove` keeps `draggedCardId`.
 *
 * Shallow: nothing mutates a target, and a deep ref would hand back a proxy. */
const draggedInPlay = shallowRef<InPlayTarget | null>(null)

export function beginInPlayDrag(target: InPlayTarget) {
  draggedInPlay.value = target
  window.addEventListener('dragend', endInPlayDrag, { once: true })
  window.addEventListener('drop', endInPlayDrag, { once: true })
}

export function endInPlayDrag() {
  draggedInPlay.value = null
}

/* What a drop should act on, if this drag is one of ours.
 *
 * Deliberately `draggedInPlay` rather than the `dataTransfer` payload: an
 * `AssetTarget` on the wire does not mean a debug drag. Player.vue's
 * permanent-tucking drags publish the same payload without coming through
 * `beginInPlayDrag`, because hiding an inert card is a display setting rather
 * than a debug feature -- reading the payload would turn a tuck that landed
 * slightly off the play area into a return to hand.
 *
 * Safe to read inside a drop handler: `beginInPlayDrag`'s own cleanup listens on
 * the window, so it only runs once the event has bubbled past the drop zone. */
export function droppedInPlayTarget(): InPlayTarget | null {
  return draggedInPlay.value
}

export { draggedInPlay }
