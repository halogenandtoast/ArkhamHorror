import { transitionDone } from '@/game/overlays'
import { sleep } from '@/game/util'

const rect = (el: Element | null | undefined) => {
  const r = el?.getBoundingClientRect()
  return r && r.width ? r : null
}

export interface EventRects {
  deck: DOMRect | null
  shown: DOMRect | null
}
export function eventRects(): EventRects {
  return {
    deck: rect(document.querySelector('[data-deck="event"] .deck-stack > img')),
    shown: rect(document.querySelector('#activeCard img[data-event]')),
  }
}

export interface DrawRects {
  decks: Record<string, { rect: DOMRect; back: string | null }>
  shown: Set<string>
}
export function drawRects(): DrawRects {
  const decks: DrawRects['decks'] = {}
  const shown = new Set<string>()
  document.querySelectorAll<HTMLElement>('[data-deck]').forEach((el) => {
    const img = el.querySelector('.deck-stack > img')
    const r = rect(img ?? el.querySelector('.deck-stack'))
    if (r) decks[el.dataset.deck!] = { rect: r, back: img?.getAttribute('src') ?? null }
  })
  document.querySelectorAll<HTMLElement>('[data-card]').forEach((el) => shown.add(el.dataset.card!))
  return { decks, shown }
}

// flies a copy of a card from `from` onto `target`, turning it face up on the way when `back` is given
// `under` is what the target showed before this card: a pile keeps showing its old top
// card until the new one lands on it, instead of looking empty
export function flyCard(
  from: DOMRect,
  target: HTMLElement,
  face: string,
  back: string | null,
  delay = 0,
  under: string | null = null,
): Promise<void> {
  if (matchMedia('(prefers-reduced-motion: reduce)').matches) return Promise.resolve()
  const landed = target.getAttribute('src')
  if (under) target.setAttribute('src', under)
  else target.style.visibility = 'hidden'
  const fly = document.createElement('div')
  fly.className = 'fly-card'
  Object.assign(fly.style, {
    left: `${from.left}px`,
    top: `${from.top}px`,
    width: `${from.width}px`,
    height: `${from.height}px`,
  })
  const inner = document.createElement('div')
  inner.className = 'fly-inner'
  if (back) {
    const b = document.createElement('img')
    b.className = 'fly-back'
    b.src = back
    b.alt = ''
    inner.appendChild(b)
  }
  const f = document.createElement('img')
  f.className = 'fly-face'
  f.src = face
  f.alt = ''
  inner.appendChild(f)
  fly.appendChild(inner)
  const start = async () => {
    await transitionDone()
    if (delay) await sleep(delay)
    document.body.appendChild(fly)
    const to = target.getBoundingClientRect()
    const dx = to.left - from.left,
      dy = to.top - from.top,
      sx = to.width / from.width,
      sy = to.height / from.height
    const opts: KeyframeAnimationOptions = { duration: 750, easing: 'cubic-bezier(.4,.1,.2,1)', fill: 'forwards' }
    const move = fly.animate(
      [
        { transform: 'translate(0, 0) scale(1, 1)' },
        {
          transform: `translate(${dx / 2}px, ${dy / 2 - 40}px) scale(${((1 + sx) / 2) * 1.15}, ${((1 + sy) / 2) * 1.15})`,
          offset: 0.5,
        },
        { transform: `translate(${dx}px, ${dy}px) scale(${sx}, ${sy})` },
      ],
      opts,
    )
    if (back) inner.animate([{ transform: 'rotateY(180deg)' }, { transform: 'rotateY(0deg)' }], opts)
    await move.finished.catch(() => {})
    if (under && landed) target.setAttribute('src', landed)
    target.style.visibility = ''
    fly.remove()
  }
  return start()
}
