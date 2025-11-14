<script lang="ts" setup>
import {
  ComputedRef,
  reactive,
  ref,
  computed,
  watch,
  watchEffect,
  onMounted,
  onUnmounted,
} from 'vue'
import { imgsrc, isLocalized, toCamelCase } from '@/arkham/helpers'
import { BugAntIcon } from '@heroicons/vue/20/solid'
import KeyToken from '@/arkham/components/Key.vue'
import { type ArkhamKey, keyToId } from '@/arkham/types/Key'
import PoolItem from '@/arkham/components/PoolItem.vue'
import { useDbCardStore, ArkhamDBCard } from '@/stores/dbCards'

/* =============================================================================
 * Constants, basic helpers, and caches
 * ========================================================================== */

const CARD_RATIO = 0.705        // width / height (portrait)
const OVERLAY_W = 300           // base width (portrait) or height (sideways)
const TAROT_H = 500             // fixed tarot height
const VIEW_W = 1000             // stable viewBox width for percent → px mapping

// Cache natural aspect ratios (width / height) to infer sideways when needed
const imgARCache = reactive(new Map<string, number>())
const loadAR = (url: string) => {
  if (!url || imgARCache.has(url)) return
  const i = new Image()
  i.decoding = 'async'
  i.onload = () => imgARCache.set(url, i.naturalWidth / i.naturalHeight)
  i.src = url
}

// Small helpers
type Pct = { top: number; left: number }

/* =============================================================================
 * Stores & reactive top-level state
 * ========================================================================== */

const store = useDbCardStore()

const cardOverlay = ref<HTMLElement | null>(null)
const hoveredElement = ref<HTMLElement | null>(null)
const isMobile = ref(false)

onMounted(() => {
  const mq = window.matchMedia('(hover: none) and (pointer: coarse)')
  const update = () => (isMobile.value = mq.matches)
  update()
  mq.addEventListener?.('change', update)
  onUnmounted(() => mq.removeEventListener?.('change', update))
})

/* =============================================================================
 * Pointer/hover handling
 * ========================================================================== */

const CARD_SELECTOR = '.card,[data-image-id],[data-target],[data-image]'
let hoverTimer: number | null = null
let pressTimer: number | null = null
let canDisablePress = false

const clearTimer = (t: number | null) => (t !== null ? (clearTimeout(t), null) : null)

const targetFromEvent = (e: Event): HTMLElement | null => {
  const raw = e.target as HTMLElement | null
  return raw ? (raw.closest(CARD_SELECTOR) as HTMLElement | null) : null
}

const queueHover = (el: HTMLElement) => {
  hoverTimer = clearTimer(hoverTimer)
  const delay = el.dataset.delay ? parseInt(el.dataset.delay, 10) : 0
  hoverTimer = window.setTimeout(() => {
    hoveredElement.value = el
    canDisablePress = true
  }, delay)
}

const handlePointerMove = (e: PointerEvent) => {
  const el = targetFromEvent(e)
  hoverTimer = clearTimer(hoverTimer)
  if (!el || el.classList.contains('dragging') || el.classList.contains('no-overlay')) {
    if (!isMobile.value) hoveredElement.value = null
    return
  }
  queueHover(el)
}

const onPointerDown = (e: PointerEvent) => {
  if (e.pointerType === 'touch') {
    const el = targetFromEvent(e)
    if (!el) return
    pressTimer = clearTimer(pressTimer)
    pressTimer = window.setTimeout(() => queueHover(el), 200) // long press
  }
}

const onPointerMove = (e: PointerEvent) => {
  if (e.pointerType === 'touch') {
    if (hoveredElement.value?.classList.contains('card--locations')) {
      hoveredElement.value = null
    }
    pressTimer = clearTimer(pressTimer)
  } else {
    handlePointerMove(e)
  }
}

const onPointerUp = () => {
  if (canDisablePress) {
    canDisablePress = false
  } else {
    hoveredElement.value = null
    pressTimer = clearTimer(pressTimer)
  }
}

onMounted(() => {
  document.addEventListener('pointerdown', onPointerDown, { passive: true })
  document.addEventListener('pointermove', onPointerMove, { passive: true })
  document.addEventListener('pointerup', onPointerUp, { passive: true })
  // only block context menu inside the overlay, not globally
  cardOverlay.value?.addEventListener('contextmenu', (e) => {
    const t = e.target as HTMLElement
    if (t?.tagName.toLowerCase() !== 'input') e.preventDefault()
  })
})
onUnmounted(() => {
  document.removeEventListener('pointerdown', onPointerDown)
  document.removeEventListener('pointermove', onPointerMove)
  document.removeEventListener('pointerup', onPointerUp)
  hoverTimer = clearTimer(hoverTimer)
  pressTimer = clearTimer(pressTimer)
})

/* =============================================================================
 * Image lookup & orientation
 * ========================================================================== */

const getImage = (el: HTMLElement, depth = 0): string | null => {
  if (depth > 3) return null // avoid runaway recursion

  if (el.dataset.imageId) return imgsrc(`cards/${el.dataset.imageId}.avif`)

  if (el instanceof HTMLImageElement && el.classList.contains('card') && !el.closest('.revelation')) {
    return el.src || null
  }

  if (el instanceof HTMLDivElement && el.classList.contains('card')) {
    const bg = el.style.backgroundImage
    if (!bg || bg === 'none') return null
    return bg.slice(4, -1).replaceAll('"', '') // strip url("...")
  }

  if (el.dataset.target) {
    const target = document.querySelector<HTMLElement>(`[data-id="${el.dataset.target}"]`)
    return target ? getImage(target, depth + 1) : null
  }

  return el.dataset.image ?? null
}

const card = computed<string | null>(() => (hoveredElement.value ? getImage(hoveredElement.value) : null))

const upsideDown = computed<boolean>(() => hoveredElement.value?.classList.contains('Reversed') ?? false)
const reversed = computed<boolean>(() => hoveredElement.value?.classList.contains('reversed') ?? false)

const sideways = computed<boolean>(() => {
  const el = hoveredElement.value
  if (!el) return false

  // explicit dataset wins
  if (el.dataset.sideways === 'true') return true
  if (el.dataset.sideways === 'false') return false

  // class heuristics
  if (el.classList.contains('exhausted')) return false
  if (el.classList.contains('attached')) return false
  if (el.classList.contains('card--sideways') || el.classList.contains('sideways')) return true
  if (el.classList.contains('modifier')) return false
  if (el.tagName.toLowerCase() === 'span') return false

  // fall back to natural aspect for dataset image
  const url = el.dataset.image ?? (el.dataset.imageId ? imgsrc(`cards/${el.dataset.imageId}.avif`) : null)
  if (url) {
    const ar = imgARCache.get(url)
    if (ar != null) return ar > 1
  }

  // final fallback: element geometry
  return el.matches('.card, [data-image-id], [data-target]') && el.offsetWidth > el.offsetHeight
})

watch(card, (src) => { if (src) loadAR(src) })

/* =============================================================================
 * Overlay positioning
 * ========================================================================== */

const overlayPosition = ref<{ top: number; left: number }>({ top: 0, left: 0 })
let posRAF: number | null = null

const getPosition = (el: HTMLElement): { top: number; left: number } => {
  const rect = el.getBoundingClientRect()
  const width = sideways.value ? OVERLAY_W / CARD_RATIO : OVERLAY_W
  const height = sideways.value ? OVERLAY_W : Math.round(OVERLAY_W / CARD_RATIO)

  const top = rect.top + window.scrollY - 40
  const bottom = top + height
  const newTop = Math.max(0, bottom > window.innerHeight ? rect.bottom - height + window.scrollY - 40 : top)

  const gap = 2
  const hasCust = !!customizationsCard.value
  const totalWidth = hasCust ? (width * 2 + gap) : width

  const rightSide = rect.left + window.scrollX + rect.width + 10
  return (rightSide + totalWidth >= window.innerWidth)
    ? { top: newTop, left: rect.left - totalWidth - 10 }
    : { top: newTop, left: rightSide }
}

watch([hoveredElement, sideways], ([el]) => {
  if (!el) { overlayPosition.value = { top: 0, left: 0 }; return }
  if (posRAF !== null) cancelAnimationFrame(posRAF)
  posRAF = requestAnimationFrame(() => { overlayPosition.value = getPosition(el as HTMLElement) })
}, { flush: 'post' })

/* =============================================================================
 * SVG sizing & transforms
 * ========================================================================== */

const svgWidth = computed(() => (tarot.value ? Math.round(TAROT_H * CARD_RATIO)
  : (sideways.value ? Math.round(OVERLAY_W / CARD_RATIO) : OVERLAY_W)))
const svgHeight = computed(() => (tarot.value ? TAROT_H
  : (sideways.value ? OVERLAY_W : Math.round(OVERLAY_W / CARD_RATIO))))

const viewH = computed(() => Math.round(VIEW_W / CARD_RATIO))
const viewBox = computed(() => (sideways.value ? `0 0 ${viewH.value} ${VIEW_W}` : `0 0 ${VIEW_W} ${viewH.value}`))

const groupTransform = computed(() => {
  if (!reversed.value && !upsideDown.value) return ''
  const w = sideways.value ? viewH.value : VIEW_W
  const h = sideways.value ? VIEW_W : viewH.value
  return `rotate(180 ${w / 2} ${h / 2})`
})

// CSS-like % to viewBox coords
const xyFromPct = (p: Pct) => {
  const vbW = sideways.value ? viewH.value : VIEW_W
  const vbH = sideways.value ? VIEW_W : viewH.value
  return { x: (p.left / 100) * vbW, y: (p.top / 100) * vbH }
}

/* =============================================================================
 * Dataset API helpers
 * ========================================================================== */

const ds = <T extends string = string>(key: T) =>
  computed<string | null>(() => hoveredElement.value?.dataset?.[key as any] ?? null)

const dsMap = <X>(key: keyof DOMStringMap, map: (v: string) => X): ComputedRef<X | null> =>
  computed(() => {
    const v = hoveredElement.value?.dataset?.[key]
    return v !== undefined ? map(v) : null
  })

const jsonDs = <T>(key: string): T => {
  try { return JSON.parse(hoveredElement.value?.dataset?.[key] ?? '[]') as T }
  catch { return [] as unknown as T }
}

/* =============================================================================
 * Minor overlays & simple dataset fields
 * ========================================================================== */

const fight = ds('fight')
const health = ds('health')
const evade = ds('evade')
const victory = ds('victory')
const keywords = ds('keywords')
const playingCardOverlay = ds('pc')
const swarm = dsMap('swarm', v => v === 'true')

const depth = computed<number | null>(() => {
  const d = hoveredElement.value?.dataset?.depth
  return d ? parseInt(d, 10) : null
})

const crossedOff = computed<string[] | null>(() => {
  const arr = jsonDs<string[]>('crossedOff')
  return arr.length ? arr : null
})

type Checkmark = { left: number; top: number; }

const checkmarks = computed<Checkmark[] | null>(() => {
  const arr = jsonDs<Checkmark[]>('checkmarks')
  return arr.length ? arr : null
})

const spentKeys = computed<ArkhamKey[]>(() => jsonDs<ArkhamKey[]>('spentKeys'))
const overlay = ds('overlay')

const tarot = computed<boolean>(() => !!hoveredElement.value?.classList.contains('tarot-card'))

// numeric damage/horror parsed from data-* (so "0" doesn't render)
const damage = computed<number | null>(() => {
  const v = hoveredElement.value?.dataset?.damage
  const n = v == null ? NaN : Number(v)
  return Number.isFinite(n) ? n : null
})
const horror = computed<number | null>(() => {
  const v = hoveredElement.value?.dataset?.horror
  const n = v == null ? NaN : Number(v)
  return Number.isFinite(n) ? n : null
})

// where badges go (ported from your CSS %)
const damagePositions: Pct[] = [
  { top: 56.1, left: 37.1 },
  { top: 55.0, left: 30.5 },
  { top: 53.3, left: 24.5 },
]
const horrorPositions: Pct[] = [
  { top: 56.5, left: 62.9 },
  { top: 55.0, left: 69.5 },
  { top: 53.5, left: 75.7 },
]
// CSS had 22px on ~300px-tall image ⇒ ~7.33% height
const badgeSize = computed(() => {
  const vbH = sideways.value ? VIEW_W : viewH.value
  const h = 0.0533 * vbH
  return { w: h, h } // ~square
})

/* =============================================================================
 * Card code & customization image
 * ========================================================================== */

const allCustomizations = new Set([
  '09021', '09022', '09023', '09040', '09041', '09042', '09059', '09060',
  '09061', '09079', '09080', '09081', '09099', '09100', '09101', '09119'
])

const cardCode = computed<string | null>(() => {
  if (!card.value) return null
  const m = card.value.match(/cards\/(\d+)(_.*)?\.avif$/)
  return m ? m[1] : null
})

const mutated = computed<string>(() => {
  if (!card.value) return ''
  const m = card.value.match(/cards\/\d+(_Mutated\d+)\.avif$/)
  return m ? m[1] : ''
})

const customizationsCard = computed<string | null>(() => {
  if (!cardCode.value) return null
  if (!allCustomizations.has(cardCode.value)) return null
  return imgsrc(`customizations/${cardCode.value}${mutated.value}.jpg`)
})

/* =============================================================================
 * Customizations: parsing & geometry (ticks, labels, circles)
 * ========================================================================== */

type ChoiceTag = 'ChosenCard' | 'ChosenTrait' | 'ChosenSkill' | 'ChosenIndex'
type CustomizationEntry = [number, [number, Array<{ tag: ChoiceTag; contents: string }>]]

const customizations = computed<CustomizationEntry[] | null>(() => {
  const raw = hoveredElement.value?.dataset?.customizations
  if (!raw) return null
  try {
    const parsed = JSON.parse(raw) as CustomizationEntry[]
    return parsed?.length ? parsed : null
  } catch {
    return null
  }
})

const customizationTicks = computed<string[]>(() => {
  if (!cardCode.value || !customizations.value) return []
  const out: string[] = []
  for (const [first, [count]] of customizations.value) {
    for (let i = 1; i <= count; i++) out.push(`customization-${cardCode.value}-${first}-${i}`)
  }
  return out
})

const customizationLabels = computed<[string, string][]>(() => {
  if (!cardCode.value || !customizations.value) return []
  const out: [string, string][] = []
  for (const [first, [, arr]] of customizations.value) {
    arr.forEach((a, j) => {
      if (a.tag === 'ChosenCard' || a.tag === 'ChosenTrait') {
        out.push([`label-${cardCode.value}-${first}-${j}`, a.contents])
      }
    })
  }
  return out
})

const customizationSkills = computed<string[]>(() => {
  if (!cardCode.value || !customizations.value) return []
  const out: string[] = []
  for (const [, [, arr]] of customizations.value) {
    arr.forEach(a => { if (a.tag === 'ChosenSkill') out.push(`skill-${cardCode.value}-${a.contents}`) })
  }
  return out
})

/** -------------------- Tick tables ----------------- **/
type TickTable = Record<string, { top: Record<number, number>, left: Record<number, number> }>
const TICK_TABLE: TickTable = {
  // Hunter's Armor (09021)
  '09021': {
    top: { 0: 21.0, 1: 31.9, 2: 42.8, 3: 47.0, 4: 51.3, 5: 62.2, 6: 76.3 },
    left: { 1: 10.0, 2: 13.0, 3: 16.6 }
  },
  // Runic Axe (09022)
  '09022': {
    top: { 0: 20.5, 1: 27.2, 2: 36.8, 3: 49.1, 4: 58.6, 5: 71.2, 6: 77.8, 7: 84.3 },
    left: { 1: 10.0, 2: 13.0, 3: 16.1, 4: 19.1 }
  },
  // Custom Modifications (09023)
  '09023': {
    top: { 0: 21.0, 1: 35.3, 2: 42.8, 3: 53.6, 4: 64.4, 5: 75.2 },
    left: { 1: 10.0, 2: 13.3, 3: 16.8, 4: 20.5 }
  },
  // Alchemical Distillation (09040)
  '09040': {
    top: { 0: 21.0, 1: 28.6, 2: 36.2, 3: 47.0, 4: 54.7, 5: 62.1, 6: 76.2 },
    left: { 1: 10.0, 2: 13.3, 3: 16.8, 4: 20.5, 5: 23.8 }
  },
  // Empirical Hypothesis (09041)
  '09041': {
    top: { 0: 20.3, 1: 27.1, 2: 33.7, 3: 40.2, 4: 46.9, 5: 59.3, 6: 68.9, 7: 78.4 },
    left: { 1: 10.0, 2: 12.9, 3: 15.8, 4: 18.9 }
  },
  // The Raven Quill (09042)
  '09042': {
    top: { 1: 26.7, 2: 33.1, 3: 39.6, 4: 46.3, 5: 52.9, 6: 62.2, 7: 71.9 },
    left: { 1: 10.0, 2: 12.9, 3: 15.8, 4: 18.9 }
  },
  // Damning Testimony (09059)
  '09059': {
    top: { 0: 20.5, 1: 34.6, 2: 42.1, 3: 49.5, 4: 63.8, 5: 74.8 },
    left: { 1: 9.9, 2: 13.3, 3: 16.6, 4: 19.9 }
  },
  // Friends in Low Places (09060)
  '09060': {
    top: { 1: 26.2, 2: 35.9, 3: 48.4, 4: 57.9, 5: 67.3, 6: 74.0, 7: 80.5 },
    left: { 1: 9.9, 2: 12.7, 3: 15.6 }
  },
  // Honed Instinct (09061)
  '09061': {
    top: { 0: 20.9, 1: 27.5, 2: 34.2, 3: 40.5, 4: 47.3, 5: 54.0, 6: 60.5, 7: 70.1 },
    left: { 1: 9.8, 2: 12.7, 3: 15.6, 4: 18.7, 5: 22.0 }
  },
  // Living Ink (09079)
  '09079': {
    top: { 1: 27.4, 2: 38.4, 3: 52.6, 4: 63.5, 5: 67.6, 6: 71.8, 7: 82.7 },
    left: { 1: 9.8, 2: 13.2, 3: 16.6 }
  },
  // Summoned Servitor (09080)
  '09080': {
    top: { 0: 20.2, 1: 29.7, 2: 39.3, 3: 51.7, 4: 58.3, 5: 67.9, 6: 74.5, 7: 83.9 },
    left: { 1: 9.8, 2: 12.6, 3: 15.7, 4: 18.7, 5: 21.7 }
  },
  // Power Word (09081) — non-mutated
  '09081': {
    top: { 0: 20.4, 1: 30.1, 2: 39.5, 3: 49.0, 4: 58.6, 5: 65.3, 6: 74.8, 7: 81.4 },
    left: { 1: 9.8, 2: 12.6, 3: 15.6 }
  },
  // Pocket Multi Tool (09099)
  '09099': {
    top: { 0: 21.0, 1: 31.9, 2: 39.5, 3: 46.9, 4: 54.6, 5: 62.0, 6: 69.7 },
    left: { 1: 9.8, 2: 13.0, 3: 16.6, 4: 19.8 }
  },
  // Makeshift Trap (09100)
  '09100': {
    top: { 0: 21.1, 1: 28.7, 2: 39.5, 3: 46.9, 4: 57.8, 5: 68.8, 6: 79.7 },
    left: { 1: 9.8, 2: 13.2, 3: 16.6, 4: 20.1 }
  },
  // Grizzled (09101)
  '09101': {
    top: { 1: 27.3, 2: 35.5, 3: 43.5, 4: 61.6, 5: 76.5 },
    left: { 1: 9.8, 2: 13.4, 3: 16.7, 4: 20.4, 5: 23.8 }
  },
  // Hyperphysical Shotcaster (09119)
  '09119': {
    top: { 0: 20.9, 1: 30.3, 2: 42.5, 3: 57.5, 4: 69.8, 5: 82.0, 6: 88.3 },
    left: { 1: 9.8, 2: 12.6, 3: 15.6, 4: 18.7 }
  },
}
// Power Word (09081) — mutated tops override
const TICK_TABLE_MUT_09081_TOP: Record<number, number> = {
  0: 20.7, 1: 30.3, 2: 36.8, 3: 46.3, 4: 55.9, 5: 62.6, 6: 72.0, 7: 78.6,
}

type TickParsed = { code: string; first: number; idx: number }
const parseTickId = (id: string): TickParsed | null => {
  const m = id.match(/^customization-(\d+)-(\d+)-(\d+)$/)
  return m ? { code: m[1], first: Number(m[2]), idx: Number(m[3]) } : null
}

const parsedTicks = computed<TickParsed[]>(() =>
  (customizationTicks.value ?? []).map(parseTickId).filter((x): x is TickParsed => !!x)
)

const tickPct = (tp: TickParsed): { top?: number; left?: number } => {
  const base = TICK_TABLE[tp.code]
  if (!base) return {}
  const topMap = (tp.code === '09081' && mutated.value) ? TICK_TABLE_MUT_09081_TOP : base.top
  return { top: topMap[tp.first], left: base.left[tp.idx] }
}

// Size for the checkmark glyph (2.8% of card width)
const tickSize = computed(() => {
  const vbW = sideways.value ? viewH.value : VIEW_W
  return 0.028 * vbW
})

/** -------------------- Label auto-fit (uniform scale, no skew) --------------- **/
const BASE_LABEL_FONT = 16 // px inside viewBox units
type LabelFit = { scale: number; dx: number; dy: number }
const labelRefs = new Map<string, SVGTextElement>()
const labelFits = reactive(new Map<string, LabelFit>())
const labelDepsSig = new Map<string, string>()
let fitRAF: number | null = null

const queueFit = () => {
  if (fitRAF != null) return
  fitRAF = requestAnimationFrame(() => {
    fitRAF = null
    for (const [id, textEl] of labelRefs) {
      const parent = textEl.parentElement as SVGGElement | null
      if (!parent) continue
      const w = Number(parent.getAttribute('data-w') || 0)
      const h = Number(parent.getAttribute('data-h') || 0)
      if (!(w > 0 && h > 0)) continue
      const bbox = textEl.getBBox()
      if (!(bbox.width > 0 && bbox.height > 0)) continue
      const scale = Math.min(w / bbox.width, h / bbox.height) * 0.985
      const dx = (w - bbox.width * scale) / 2 - bbox.x * scale
      const dy = (h - bbox.height * scale) / 2 - bbox.y * scale
      labelFits.set(id, { scale, dx, dy })
    }
  })
}

const setLabelRef = (id: string, deps: () => string) => (el: SVGTextElement | null) => {
  if (!el) {
    labelRefs.delete(id)
    labelFits.delete(id)
    labelDepsSig.delete(id)
    return
  }
  labelRefs.set(id, el)
  const sig = deps()
  if (labelDepsSig.get(id) !== sig) {
    labelDepsSig.set(id, sig)
    queueFit()
  }
}

const labelTransform = (id: string, item: LabelRender) => {
  const fit = labelFits.get(id)
  if (!fit) return `translate(${item.x}, ${item.y})`
  return `translate(${item.x + fit.dx}, ${item.y + fit.dy}) scale(${fit.scale})`
}

/** -------------------- Label geometry for specific cards -------------------- **/
type LabelGeom = { top: number; left: number; width: number; height: number } // % units
const LABEL_TABLE: Record<string, Record<string, LabelGeom>> = {
  // Grizzled (09101)
  '09101': {
    '0-0': { top: 18.0, left: 35.2, width: 25.0, height: 5.8 },
    '0-1': { top: 18.0, left: 64.0, width: 25.0, height: 5.8 },
    '1-0': { top: 27.5, left: 8.0,  width: 25.0, height: 5.8 },
    '2-0': { top: 35.5, left: 8.0,  width: 25.0, height: 5.8 },
  },
  // Living Ink (09079) — labels unused; circles below.
  '09079': {},
  // Friends in Low Places (09060)
  '09060': {
    '0-0': { top: 18.0, left: 29.0, width: 40.0, height: 5.0 },
    '2-0': { top: 33.4, left: 66.0, width: 25.0, height: 5.0 },
  },
  // The Raven Quill (09042)
  '09042': {
    '0-0': { top: 18.0, left: 52.0, width: 40.0, height: 5.0 },
    '4-0': { top: 46.5, left: 18.0, width: 36.0, height: 5.0 },
    '4-1': { top: 46.5, left: 55.0, width: 35.0, height: 5.0 },
  },
}

type LabelRender = { x: number; y: number; w: number; h: number; text: string; code: string; key: string }
const parseLabelId = (id: string) => {
  const m = id.match(/^label-(\d+)-(\d+)-(\d+)$/)
  return m ? { code: m[1], key: `${m[2]}-${m[3]}` } : null
}

const rectFromPct = (r: LabelGeom) => {
  const vbW = sideways.value ? viewH.value : VIEW_W
  const vbH = sideways.value ? VIEW_W : viewH.value
  return { x: (r.left / 100) * vbW, y: (r.top / 100) * vbH, w: (r.width / 100) * vbW, h: (r.height / 100) * vbH }
}

const labelItems = computed<LabelRender[]>(() =>
  (customizationLabels.value ?? []).flatMap(([id, text]) => {
    const parsed = parseLabelId(id)
    if (!parsed) return []
    const table = LABEL_TABLE[parsed.code]
    if (!table) return []
    const geom = table[parsed.key]
    if (!geom) return []
    const px = rectFromPct(geom)
    return [{ x: px.x, y: px.y, w: px.w, h: px.h, text, code: parsed.code, key: parsed.key }]
  })
)

/** -------------------- Skills (09079 Living Ink) circles -------------------- **/
type SkillGeom = { top: number; left: number } // % units
const SKILL_TABLE_09079: Record<string, SkillGeom> = {
  'SkillWillpower': { top: 18.6, left: 42.5 },
  'SkillIntellect': { top: 18.6, left: 55.0 },
  'SkillCombat':    { top: 18.6, left: 68.4 },
  'SkillAgility':   { top: 18.6, left: 81.0 },
}

type SkillRender = { cx: number; cy: number; r: number; name: string }
const skillItems = computed<SkillRender[]>(() => {
  if (cardCode.value !== '09079') return []
  const vbW = sideways.value ? viewH.value : VIEW_W
  const vbH = sideways.value ? VIEW_W : viewH.value
  const sizePct = 7.0
  const r = 0.5 * (sizePct / 100) * vbW
  return (customizationSkills.value ?? []).flatMap(cls => {
    const m = cls.match(/^skill-(\d+)-(.+)$/)
    if (!m) return []
    const skillName = m[2]
    const pos = SKILL_TABLE_09079[skillName]
    if (!pos) return []
    const cx = (pos.left / 100) * vbW + r
    const cy = (pos.top  / 100) * vbH + r
    return [{ cx, cy, r, name: skillName }]
  })
})

/* =============================================================================
 * ArkhamDB fallback text (localization overlay)
 * ========================================================================== */

const dbCardName = ref<string>('')
const dbCardTraits = ref<string>('')
const dbCardText = ref<string>('')
const dbCardFlavor = ref<string>('')
const dbCardCustomizationText = ref<string>('')

const dbCardData = computed<boolean>(() =>
  !!(dbCardName.value || dbCardTraits.value || dbCardText.value || dbCardCustomizationText.value || dbCardFlavor.value)
)

const TOKEN_MAP: Record<string, string> = {
  '[action]': '<span class="action-icon"></span>',
  '[fast]': '<span class="fast-icon"></span>',
  '[free]': '<span class="free-icon"></span>',
  '[reaction]': '<span class="reaction-icon"></span>',
  '[willpower]': '<span class="willpower-icon"></span>',
  '[intellect]': '<span class="intellect-icon"></span>',
  '[combat]': '<span class="combat-icon"></span>',
  '[agility]': '<span class="agility-icon"></span>',
  '[wild]': '<span class="wild-icon"></span>',
  '[guardian]': '<span class="guardian-icon"></span>',
  '[seeker]': '<span class="seeker-icon"></span>',
  '[rogue]': '<span class="rogue-icon"></span>',
  '[mystic]': '<span class="mystic-icon"></span>',
  '[survivor]': '<span class="survivor-icon"></span>',
  '[elder_sign]': '<span class="elder-sign"></span>',
  '[auto_fail]': '<span class="auto-fail"></span>',
  '[skull]': '<span class="skull-icon"></span>',
  '[cultist]': '<span class="cultist-icon"></span>',
  '[tablet]': '<span class="tablet-icon"></span>',
  '[elder_thing]': '<span class="elder-thing-icon"></span>',
  '[bless]': '<span class="bless-icon"></span>',
  '[curse]': '<span class="curse-icon"></span>',
  '[frost]': '<span class="frost-icon"></span>',
  '[per_investigator]': '<span class="per-player"></span>',
  '[seal_a]': '<span class="seal-a-icon"></span>',
  '[seal_b]': '<span class="seal-b-icon"></span>',
  '[seal_c]': '<span class="seal-c-icon"></span>',
  '[seal_d]': '<span class="seal-d-icon"></span>',
  '[seal_e]': '<span class="seal-e-icon"></span>',
}

const escapeRegExp = (s: string) => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
const tokenRE = new RegExp(Object.keys(TOKEN_MAP).map(escapeRegExp).join('|'), 'g')
const replaceText = (text: string): string => !text ? '' :
  text
    .replaceAll('[[', '<span style="font-style: italic; font-weight: bold">')
    .replaceAll(']]', '</span>')
    .replaceAll('<i>', '<span style="font-style: italic;">')
    .replaceAll('</i>', '</span>')
    .replace(tokenRE, (m) => TOKEN_MAP[m] ?? m)

const getCardName = (dbCard: ArkhamDBCard, needBack: boolean): string | null => {
  if (!card.value || isLocalized(card.value)) return null
  if (dbCard.name === dbCard.real_name) return null
  let name = (needBack ? (dbCard.double_sided ? (dbCard.back_name || dbCard.name) : dbCard.back_name) : dbCard.name) || null
  if (!name) return null
  if (!needBack && dbCard.subname) name = `${name}: ${dbCard.subname}`
  if ((dbCard.xp || 0) > 0) name = `${name} (${dbCard.xp})`
  if (dbCard.is_unique) name = `*${name}`
  return name
}
const getCardTraits = (dbCard: ArkhamDBCard, needBack: boolean): string | null =>
  (!card.value || isLocalized(card.value)) ? null :
    (needBack ? (dbCard.double_sided ? (dbCard.back_traits || dbCard.traits) : dbCard.back_traits) : dbCard.traits) || null

const getCardText = (dbCard: ArkhamDBCard, needBack: boolean): string | null => {
  if (!card.value || isLocalized(card.value)) return null
  const t = needBack ? (dbCard.back_text || null) : (dbCard.text || null)
  return t ? replaceText(t) : null
}

const getCardFlavor = (dbCard: ArkhamDBCard, needBack: boolean): string | null => {
  if (!card.value || isLocalized(card.value)) return null
  const t = needBack ? (dbCard.back_flavor || null) : (dbCard.flavor || null)
  return t ? replaceText(t) : null
}

const getCardCustomizationText = (dbCard: ArkhamDBCard): string | null =>
  (!card.value || isLocalized(card.value)) ? null : replaceText(dbCard.customization_text || '')

watchEffect(() => {
  dbCardName.value = dbCardTraits.value = dbCardText.value = dbCardCustomizationText.value = dbCardFlavor.value = ''
  const src = card.value
  if (!src) return
  const m = src.match(/(\d+b?)(_.*)?\.avif$/)
  if (!m) return
  const code = m[1]
  const tabooSuffix = m[2]
  const language = localStorage.getItem('language') || 'en'
  if (imgsrc(`cards/${m[0]}`).includes(language)) return

  const dbCard = store.getDbCard(code)
  if (!dbCard) return
  const needBack = dbCard.code !== code

  const name = getCardName(dbCard, needBack)
  const traits = getCardTraits(dbCard, needBack)
  const text = getCardText(dbCard, needBack)
  const flavor = getCardFlavor(dbCard, needBack)
  const cust = getCardCustomizationText(dbCard)

  dbCardName.value = name ? `${tabooSuffix ? '[Taboo] ' : ''}${name}` : ''
  dbCardTraits.value = traits ?? ''
  dbCardText.value = text ?? ''
  dbCardFlavor.value = flavor ?? ''
  dbCardCustomizationText.value = cust ?? ''
})
</script>

<template>
  <div
    class="card-overlay"
    ref="cardOverlay"
    :style="{ top: overlayPosition.top + 'px', left: overlayPosition.left + 'px'}"
    :class="{ sideways, tarot, isMobile }"
  >
    <div class="card-image">
      <svg
        v-if="card"
        class="card-svg"
        :viewBox="viewBox"
        :width="svgWidth"
        :height="svgHeight"
        :style="`width:${svgWidth}px;height:${svgHeight}px`"
        preserveAspectRatio="xMidYMid meet"
      >
        <g :transform="groupTransform" :data-pc="playingCardOverlay">
          <image
            :href="card"
            x="0" y="0"
            :width="sideways ? viewH : VIEW_W"
            :height="sideways ? VIEW_W : viewH"
          />
          <image
            v-if="playingCardOverlay"
            :href="playingCardOverlay"
            class="playing-card-overlay"
            x="25" y="30"
            width="160"
          />
          <image
            v-if="overlay"
            :href="overlay"
            x="0" y="0"
            :width="sideways ? viewH : VIEW_W"
            :height="sideways ? VIEW_W : viewH"
          />

          <template v-if="damage">
            <template v-for="i in Math.min(damage, 3)" :key="'d'+i">
              <template v-if="damagePositions[i-1]">
                <image
                  :href="imgsrc('damage-overlay.png')"
                  :x="xyFromPct(damagePositions[i-1]).x - badgeSize.w/2"
                  :y="xyFromPct(damagePositions[i-1]).y - badgeSize.h/2"
                  :width="badgeSize.w"
                  :height="badgeSize.h"
                />
              </template>
            </template>
          </template>

          <template v-if="horror">
            <template v-for="i in Math.min(horror, 3)" :key="'h'+i">
              <template v-if="horrorPositions[i-1]">
                <image
                  :href="imgsrc('horror-overlay.png')"
                  :x="xyFromPct(horrorPositions[i-1]).x - badgeSize.w/2"
                  :y="xyFromPct(horrorPositions[i-1]).y - badgeSize.h/2"
                  :width="badgeSize.w"
                  :height="badgeSize.h"
                />
              </template>
            </template>
          </template>

          <template v-for="pos in checkmarks" :key="`checkmark-${pos.top}-${pos.left}`">
            <template v-if="pos.top !== undefined && pos.left !== undefined">
              <g
                :transform="(() => {
                  const vbW = sideways ? viewH : VIEW_W;
                  const vbH = sideways ? VIEW_W : viewH;
                  const x = (pos.left! / 100) * vbW;
                  const y = (pos.top!  / 100) * vbH;
                  const s = tickSize;
                  return `translate(${x - s/2}, ${y - s/2}) scale(${s/24})`;
                })()"
                fill="#690000"
                aria-label="tick"
              >
                <path d="M20.285 2l-11.285 11.567-5.286-5.011-3.714 3.716 9 8.728 15-15.285z"/>
              </g>
            </template>
          </template>
        </g>
      </svg>

      <svg
        v-if="customizationsCard"
        class="card-svg customizations-svg"
        :width="svgWidth"
        :height="svgHeight"
        :style="`width:${svgWidth}px;height:${svgHeight}px`"
        :viewBox="viewBox"
        preserveAspectRatio="xMidYMid meet"
      >
        <g :transform="groupTransform">
          <image
            :href="customizationsCard"
            x="0" y="0"
            :width="sideways ? viewH : VIEW_W"
            :height="sideways ? VIEW_W : viewH"
          />

          <template v-for="tp in parsedTicks" :key="`cust-${tp.code}-${tp.first}-${tp.idx}`">
            <template v-if="tickPct(tp).top !== undefined && tickPct(tp).left !== undefined">
              <g
                :transform="(() => {
                  const vbW = sideways ? viewH : VIEW_W;
                  const vbH = sideways ? VIEW_W : viewH;
                  const pos = tickPct(tp);
                  const x = (pos.left! / 100) * vbW;
                  const y = (pos.top!  / 100) * vbH;
                  const s = tickSize;
                  return `translate(${x - s/2}, ${y - s/2}) scale(${s/24})`;
                })()"
                fill="currentColor"
                aria-label="tick"
              >
                <path d="M20.285 2l-11.285 11.567-5.286-5.011-3.714 3.716 9 8.728 15-15.285z"/>
              </g>
            </template>
          </template>

          <template v-for="item in labelItems" :key="`lbl-${item.code}-${item.key}`">
            <g :data-w="item.w" :data-h="item.h" :transform="labelTransform(`lbl-${item.code}-${item.key}`, item)">
              <text
                :ref="setLabelRef(
                  `lbl-${item.code}-${item.key}`,
                  () => [item.text, item.w.toFixed(2), item.h.toFixed(2), sideways ? 'S' : 'P'].join('|')
                )"
                x="0" y="0"
                :font-size="BASE_LABEL_FONT"
                style="font-weight: 600;"
              >
                {{ item.text }}
              </text>
            </g>
          </template>

          <template v-for="s in skillItems" :key="`skill-09079-${s.name}`">
            <circle :cx="s.cx" :cy="s.cy" :r="s.r" fill="rgba(0,0,0,0.4)" stroke="#222" stroke-width="2" />
          </template>
        </g>
      </svg>

      <div v-for="entry in crossedOff" :key="entry" class="crossed-off" :class="{ [toCamelCase(entry)]: true }"></div>
    </div>

    <div class="card-data" v-if="dbCardData" :class="{ reversed, Reversed: upsideDown }">
      <p v-if="dbCardName" style="font-size: 1.0em;"><b>{{ dbCardName }}</b></p>
      <p v-if="dbCardTraits"><span style="font-style: italic;">{{ dbCardTraits }}</span></p>
      <p v-if="dbCardText"><br></p>
      <p v-if="dbCardText" v-html="dbCardText" style="font-size: 0.85em;"></p>
      <p v-if="dbCardFlavor"><br></p>
      <p v-if="dbCardFlavor" v-html="dbCardFlavor" style="font-size: 0.75em; font-style: italic;"></p>
    </div>

    <span class="swarm" v-if="swarm"><BugAntIcon aria-hidden="true" /></span>
    <span class="fight" v-if="fight">{{ fight }}</span>
    <span class="health" v-if="health">{{ health }}</span>
    <span class="evade" v-if="evade">{{ evade }}</span>
    <span class="victory" v-if="victory">Victory {{ victory }}.</span>
    <span class="keywords" v-if="keywords">{{ keywords }}.</span>
    <PoolItem class="depth" v-if="depth" type="resource" :amount="depth" />

    <div class="spent-keys" v-if="spentKeys.length > 0">
      <KeyToken v-for="k in spentKeys" :key="keyToId(k)" :keyToken="k" @choose="() => {}"/>
    </div>

    <div class="card-data" v-if="dbCardCustomizationText">
      <p v-if="dbCardName"><b>{{ dbCardName }}</b></p>
      <p v-if="dbCardCustomizationText" v-html="dbCardCustomizationText" style="font-size: 0.85em;"></p>
    </div>
  </div>
</template>

<style scoped>
.fight, .evade, .health, .swarm {
  font-family: "Teutonic";
  position: absolute;
  color: white;
  font-weight: bold;
  font-size: 1.3em;
  text-shadow:
    1px 1px 0 #000,
    -1px 1px 0 #000,
    -1px -1px 0 #000,
    1px -1px 0 #000;
}
.swarm {
  inset: 0;
  width: var(--card-width);
  height: auto;
}

.fight {
  top: 11%;
  left: 30%;
}

.health {
  font-size: 1.6em;
  top: 10%;
  transform: translate(-50%, 0);
  left:50%;
}

.evade {
  top: 11%;
  left: 68%;
}

.victory {
  position: absolute;
  color: rgba(0, 0, 0, 0.6);
  top: 47%;
  transform: translate(-50%, 0);
  left:50%;
  font-weight: 900;
  font-size: 0.8em;
}

.keywords {
  width: 80%;
  position: absolute;
  color: rgba(0, 0, 0, 0.6);
  top: 23.2%;
  left:13%;
  font-weight: bold;
  font-size: 0.8em;
}

.card-data {
  position: relative;
  width: 300px;
  min-height: inherit;
  overflow-y: visible;
  margin-left: 2px;
  padding: 5px;
  border-radius: 15px;
  font-family: Arial;
  white-space: pre-wrap;
  word-wrap: break-word;
  aspect-ratio: var(--card-aspect);
  -ms-overflow-style: none;
  scrollbar-width: none;
  scroll-behavior: smooth;
  background-color: rgba(185, 185, 185, 0.85);
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.75);
}
.card-data::-webkit-scrollbar {
  display: none;
}

.card-overlay {
  position: absolute;
  z-index: 1000;
  display: flex;
  width: max-content;
  height: auto;
  top: 0;
  left: 2px;
  pointer-events: none;
  animation: fadeIn 0.5s;
}
.card-overlay.sideways {
  /* on narrow portrait screens, allow horizontal scroll if both SVGs visible */
  @media (max-width: 800px) and (orientation: portrait){
    overflow: auto;
  }
}
.card-overlay.tarot {
  height: 500px !important;
  width: fit-content !important;
}

.card-svg {
  filter: drop-shadow(1px 1px 6px rgba(0, 0, 0, 0.75));
  border-radius: 15px;
  width: 300px;
  height: fit-content;
  aspect-ratio: var(--card-aspect);
  overflow: hidden;
}

.reversed, .Reversed { transform: rotateZ(180deg); }

.card-image { position: relative; }

@keyframes fadeIn { 0% { opacity: 0; } 100% { opacity: 1; } }

.crossed-off {
  position: absolute;
  margin-inline: auto;
  inset-inline: 0;
  border-top: 2px solid red;
  width: 33%;
}

.brianBurnham { top: 31.8%; }
.otheraGilman { top: 36.0%; }
.joyceLittle { top: 40.4%; }
.barnabasMarsh { top: 44.2%; }
.zadokAllen { top: 48.5%; }
.robertFriendly { top: 53%; }
.innsmouthJail { top: 65.4%; }
.shorewardSlums { top: 69.5%; }
.sawboneAlley { top: 73.9%; }
.theHouseOnWaterStreet { top: 78%; width: 50%; }
.esotericOrderOfDagon { top: 82.2%; width: 50%; }
.newChurchGreen { top: 86.7%; }

.spent-keys {
  position: absolute;
  bottom: 17%;
  inset-inline: 40px;
  margin-inline: auto;
  display: flex;
  gap: 2px;
}
.spent-keys :deep(img) {
  border-radius: 2px;
  width: 25px;
  height: 25px;
}

.depth {
  left: calc(50% - 20px);
  bottom: 16%;
  position: absolute;
  width: 40px;
}

.isMobile {
  inset: 0 !important;
  margin: auto;
  align-self: center;
  justify-content: center;
  width: fit-content;
}
</style>
