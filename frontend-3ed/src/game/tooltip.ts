import { ref } from 'vue'

/* One tooltip for the whole table. Anything with data-tip raises it on hover;
data-tip-body carries the sentence under the name. It lives outside the panels
so a token in a drawer that clips its own overflow still gets one. */
export const tip = ref<{ title: string; body: string; x: number; y: number; below: boolean } | null>(null)

export function showTip(el: HTMLElement) {
  const title = el.dataset.tip
  if (!title) return
  const r = el.getBoundingClientRect()
  // above the token by default; below it when there is no room up there
  const below = r.top < 120
  tip.value = {
    title,
    body: el.dataset.tipBody ?? '',
    x: r.left + r.width / 2,
    y: below ? r.bottom + 8 : r.top - 8,
    below,
  }
}

export const hideTip = () => (tip.value = null)
