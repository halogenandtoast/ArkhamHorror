import { onScopeDispose, toValue, watch, type MaybeRefOrGetter } from 'vue'

/* Escape closes whatever was opened last.
 *
 * Debug windows stack: a card's window sits over the board, a second card's
 * window sits over the first, and a picker inside one sits over its window. A
 * handler per component with no ordering between them would close all of them
 * on one press, so they share a stack and only the top of it runs.
 *
 * `active` is for a panel that stays mounted while it is closed -- a dropdown
 * whose button is part of the card, a modal owned by a long-lived component.
 * Those would otherwise hold whatever position in the stack their component's
 * mount order gave them, so opening one moves it to the top, and a press skips
 * the ones that are showing nothing.
 *
 * Capture phase, so it beats the board-level shortcut handler Game.vue puts on
 * the document, and it runs wherever focus happens to be -- including inside a
 * window's own inputs, where Escape means "back out of this" and not "type".
 */

type Entry = {
  close: () => void
  active: MaybeRefOrGetter<boolean>
}

const entries: Entry[] = []

const onKeydown = (event: KeyboardEvent) => {
  if (event.key !== 'Escape') return
  for (let i = entries.length - 1; i >= 0; i--) {
    const entry = entries[i]
    if (!toValue(entry.active)) continue
    event.preventDefault()
    event.stopPropagation()
    entry.close()
    return
  }
}

const push = (entry: Entry) => {
  if (entries.length === 0) window.addEventListener('keydown', onKeydown, true)
  entries.push(entry)
}

const remove = (entry: Entry) => {
  const index = entries.lastIndexOf(entry)
  if (index !== -1) entries.splice(index, 1)
  if (entries.length === 0) window.removeEventListener('keydown', onKeydown, true)
}

export function useEscape(close: () => void, active: MaybeRefOrGetter<boolean> = true) {
  const entry: Entry = { close, active }
  push(entry)

  if (active !== true) {
    watch(
      () => toValue(active),
      (isActive) => {
        if (!isActive) return
        remove(entry)
        push(entry)
      },
    )
  }

  onScopeDispose(() => remove(entry))
}
