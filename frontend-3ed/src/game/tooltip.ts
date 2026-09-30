import { ref } from 'vue'

/* One tooltip for the whole table. Anything with data-tip raises it on hover;
data-tip-body carries the sentence under the name. It lives outside the panels
so a token in a drawer that clips its own overflow still gets one. The anchor is
what the tooltip is about; where it actually fits is worked out once it has been
measured. */
export const tip = ref<{ title: string; body: string; anchor: DOMRect } | null>(null)

export function showTip(el: HTMLElement) {
  const title = el.dataset.tip
  if (!title) return
  tip.value = { title, body: el.dataset.tipBody ?? '', anchor: el.getBoundingClientRect() }
}

export const hideTip = () => (tip.value = null)

/** Where the box goes: over the anchor when there is room, under it otherwise,
and always inside the window by at least `edge`. */
export function placeTip(anchor: DOMRect, box: { width: number; height: number }, edge = 8) {
  const gap = 8
  const below = anchor.top - gap - box.height < edge
  const wantedTop = below ? anchor.bottom + gap : anchor.top - gap - box.height
  const top = Math.max(edge, Math.min(wantedTop, window.innerHeight - edge - box.height))
  const wanted = anchor.left + anchor.width / 2 - box.width / 2
  const left = Math.max(edge, Math.min(wanted, window.innerWidth - edge - box.width))
  // the arrow follows the anchor even when the box has been pushed off its centre
  const arrow = Math.max(10, Math.min(anchor.left + anchor.width / 2 - left, box.width - 10))
  return { left, top, arrow, below }
}
