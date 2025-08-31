<script lang="ts" setup>
import { ref, computed, watch, watchEffect, onMounted, onUnmounted } from 'vue'
import { imgsrc, isLocalized, toCamelCase } from '@/arkham/helpers'
import { BugAntIcon } from '@heroicons/vue/20/solid'
import Key from '@/arkham/components/Key.vue'
import PoolItem from '@/arkham/components/PoolItem.vue'
import { useDbCardStore, ArkhamDBCard } from '@/stores/dbCards'

/* --------------------------------- stores --------------------------------- */

const store = useDbCardStore()

/* ------------------------------- DOM & state ------------------------------- */

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

/* -------------------------- event handling (pointer) ----------------------- */

const CARD_SELECTOR = '.card,[data-image-id],[data-target],[data-image]'
let hoverTimer: number | null = null
let pressTimer: number | null = null
let canDisablePress = false

const clearTimer = (t: number | null) => {
  if (t !== null) clearTimeout(t)
  return null
}

const targetFromEvent = (e: Event): HTMLElement | null => {
  const raw = e.target as HTMLElement | null
  if (!raw) return null
  const cardish = raw.closest(CARD_SELECTOR) as HTMLElement | null
  return cardish ?? null
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
  if (!el || el.classList.contains('dragging')) {
    if (!isMobile.value) hoveredElement.value = null
    return
  }
  queueHover(el)
}

const onPointerDown = (e: PointerEvent) => {
  // Long-press only for touch
  if (e.pointerType === 'touch') {
    const el = targetFromEvent(e)
    if (!el) return
    pressTimer = clearTimer(pressTimer)
    pressTimer = window.setTimeout(() => queueHover(el), 200)
  }
}

const onPointerMove = (e: PointerEvent) => {
  if (e.pointerType === 'touch') {
    // special filters: moving while showing location cards cancels
    if (hoveredElement.value?.classList.contains('card--locations')) {
      hoveredElement.value = null
    }
    pressTimer = clearTimer(pressTimer)
  } else {
    handlePointerMove(e)
  }
}

const onPointerUp = (_e: PointerEvent) => {
  // If press revealed, first click/tap just arms disable
  if (canDisablePress) {
    canDisablePress = false
  } else {
    // Otherwise, hide and cancel timers
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

/* --------------------------- image & positioning --------------------------- */

const getImage = (el: HTMLElement, depth = 0): string | null => {
  if (depth > 3) return null // avoid runaway recursion
  if (el.dataset.imageId) return imgsrc(`cards/${el.dataset.imageId}.avif`)

  if (el instanceof HTMLImageElement && el.classList.contains('card') && !el.closest('.revelation')) {
    return el.src || null
  }

  if (el instanceof HTMLDivElement && el.classList.contains('card')) {
    const bg = el.style.backgroundImage
    if (!bg || bg === 'none') return null
    // url("...") -> ...
    return bg.slice(4, -1).replaceAll('"', '')
  }

  if (el.dataset.target) {
    const target = document.querySelector<HTMLElement>(`[data-id="${el.dataset.target}"]`)
    return target ? getImage(target, depth + 1) : null
  }

  if (el.dataset.image) return el.dataset.image
  return null
}

const card = computed<string | null>(() => hoveredElement.value ? getImage(hoveredElement.value) : null)

const upsideDown = computed<boolean>(() => hoveredElement.value?.classList.contains('Reversed') ?? false)
const reversed = computed<boolean>(() => hoveredElement.value?.classList.contains('reversed') ?? false)

const sideways = computed<boolean>(() => {
  const el = hoveredElement.value
  if (!el) return false
  if (el.classList.contains('exhausted')) return false
  if (el.classList.contains('attached')) return false
  if (el.classList.contains('card--sideways') || el.classList.contains('sideways')) return true
  if (el.classList.contains('modifier')) return false
  if (el.tagName.toLowerCase() === 'span') return false
  // last resort: geometry (avoid frequent layout reads elsewhere)
  return el.offsetWidth > el.offsetHeight
})

const overlayPosition = ref<{ top: number; left: number }>({ top: 0, left: 0 })
let posRAF: number | null = null

const getPosition = (el: HTMLElement): { top: number; left: number } => {
  const rect = el.getBoundingClientRect()
  const overlayWidth = 300
  const ratio = 0.705
  const width = sideways.value ? overlayWidth / ratio : overlayWidth
  const height = sideways.value ? overlayWidth : width / ratio
  const top = rect.top + window.scrollY - 40
  const bottom = top + height
  const newTop = Math.max(0, bottom > window.innerHeight ? rect.bottom - height + window.scrollY - 40 : top)
  const left = rect.left + window.scrollX + rect.width + 10
  return (left + width >= window.innerWidth)
    ? { top: newTop, left: rect.left - overlayWidth - 10 }
    : { top: newTop, left }
}

watch([hoveredElement, sideways], ([el]) => {
  if (!el) { overlayPosition.value = { top: 0, left: 0 }; return }
  if (posRAF !== null) cancelAnimationFrame(posRAF)
  posRAF = requestAnimationFrame(() => { overlayPosition.value = getPosition(el as HTMLElement) })
}, { flush: 'post' })

/* ------------------------------- datasets API ------------------------------ */

const ds = <T extends string = string>(key: T) =>
  computed<string | null>(() => hoveredElement.value?.dataset?.[key as any] ?? null)

const jsonDs = <T>(key: string): T => {
  try { return JSON.parse(hoveredElement.value?.dataset?.[key] ?? '[]') as T }
  catch { return [] as unknown as T }
}

const fight = ds('fight')
const health = ds('health')
const evade = ds('evade')
const victory = ds('victory')
const keywords = ds('keywords')
const swarm = ds('swarm')

const depth = computed<number | null>(() => {
  const d = hoveredElement.value?.dataset?.depth
  return d ? parseInt(d, 10) : null
})

const crossedOff = computed<string[] | null>(() => {
  const arr = jsonDs<string[]>('crossedOff')
  return arr.length ? arr : null
})

const spentKeys = computed<string[]>(() => jsonDs<string[]>('spentKeys'))

const overlay = ds('overlay')

/* ---------------------------- card code & variants ------------------------- */

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

/* ------------------------------- customizations ---------------------------- */

type ChoiceTag = 'ChosenCard' | 'ChosenTrait' | 'ChosenSkill' | 'ChosenIndex'
type CustomizationEntry = [number, [number, Array<{ tag: ChoiceTag; contents: string }>]]

const customizations = computed<CustomizationEntry[] | null>(() => {
  const raw = hoveredElement.value?.dataset?.customizations
  if (!raw) return null
  try {
    const parsed = JSON.parse(raw) as CustomizationEntry[]
    return parsed?.length ? parsed : null
  } catch {
    console.log(raw)
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

const customizationIndexes = computed<string[]>(() => {
  if (!cardCode.value || !customizations.value) return []
  const out: string[] = []
  for (const [first, [, arr]] of customizations.value) {
    arr.forEach(a => { if (a.tag === 'ChosenIndex') out.push(`index-${cardCode.value}-${first}-${a.contents}`) })
  }
  return out
})

/* --------------------------- DB card localization -------------------------- */

const dbCardName = ref<string>('')
const dbCardTraits = ref<string>('')
const dbCardText = ref<string>('')
const dbCardFlavor = ref<string>('')
const dbCardCustomizationText = ref<string>('')

const dbCardData = computed<boolean>(() =>
  !!(dbCardName.value || dbCardTraits.value || dbCardText.value || dbCardCustomizationText.value || dbCardFlavor.value)
)

watchEffect(() => {
  dbCardName.value = dbCardTraits.value = dbCardText.value = dbCardCustomizationText.value = dbCardFlavor.value = ''

  const src = card.value
  if (!src) return
  const m = src.match(/(\d+b?)(_.*)?\.avif$/)
  if (!m) return
  const code = m[1]
  const tabooSuffix = m[2]
  const language = localStorage.getItem('language') || 'en'

  // If image already localized, skip DB text overlay
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

/* ----------------------------- text replacement ---------------------------- */

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

function escapeRegExp(string: string): string {
  return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'); 
}
const tokenRE = new RegExp(Object.keys(TOKEN_MAP).map(escapeRegExp).join('|'), 'g')

const replaceText = (text: string): string => {
  if (!text) return ''
  return text
    .replaceAll('[[', '<span style="font-style: italic; font-weight: bold">')
    .replaceAll(']]', '</span>')
    .replaceAll('<i>', '<span style="font-style: italic;">')
    .replaceAll('</i>', '</span>')
    .replace(tokenRE, (m) => TOKEN_MAP[m] ?? m)
}

/* ----------------------------- DB helpers (pure) --------------------------- */

const getCardName = (dbCard: ArkhamDBCard, needBack: boolean): string | null => {
  if (!card.value) return null
  if (isLocalized(card.value)) return null

  if (dbCard.name === dbCard.real_name) return null
  let name = (needBack ? (dbCard.double_sided ? (dbCard.back_name || dbCard.name) : dbCard.back_name) : dbCard.name) || null;
  if (!name) return null
  if (!needBack && dbCard.subname) name = `${name}: ${dbCard.subname}`
  if ((dbCard.xp || 0) > 0) name = `${name} (${dbCard.xp})`
  if ((dbCard.is_unique)) name = `*${name}`
  return name
}

const getCardTraits = (dbCard: ArkhamDBCard, needBack: boolean): string | null => {
  if (!card.value) return null
  if (isLocalized(card.value)) return null

  return (needBack ? (dbCard.double_sided ? (dbCard.back_traits || dbCard.traits) : dbCard.back_traits) : dbCard.traits) || null

}

const getCardText = (dbCard: ArkhamDBCard, needBack: boolean): string | null => {
  if (!card.value) return null
  if (isLocalized(card.value)) return null
  
  const t = needBack ? (dbCard.back_text || null) : (dbCard.text || null)
  return t ? replaceText(t) : null
}

const getCardFlavor = (dbCard: ArkhamDBCard, needBack: boolean): string | null => {
  if (!card.value) return null
  if (isLocalized(card.value)) return null

  const t = needBack ? (dbCard.back_flavor || null) : (dbCard.flavor || null)
  return t ? replaceText(t) : null
}


const getCardCustomizationText = (dbCard: ArkhamDBCard): string | null => {
  return replaceText(dbCard.customization_text || '')
}

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

</script>

<template>
  <div class="card-overlay" ref="cardOverlay" :style="{ top: overlayPosition.top + 'px', left: overlayPosition.left + 'px'}" :class="{ sideways, tarot, isMobile }">
    <div class="card-image">
      <img v-if="card" :src="card" :class="{ reversed, Reversed: upsideDown }" />
      <img
        v-if="overlay"
        class="card-overlay"
        :src="overlay"
      />
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
    <img class="damage damage-1" v-if="damage && damage >= 1" :src="imgsrc('damage-overlay.png')"/>
    <img class="damage damage-2" v-if="damage && damage >= 2" :src="imgsrc('damage-overlay.png')"/>
    <img class="damage damage-3" v-if="damage && damage >= 3" :src="imgsrc('damage-overlay.png')"/>
    <img class="horror horror-1" v-if="horror && horror >= 1" :src="imgsrc('horror-overlay.png')"/>
    <img class="horror horror-2" v-if="horror && horror >= 2" :src="imgsrc('horror-overlay.png')"/>
    <img class="horror horror-3" v-if="horror && horror >= 3" :src="imgsrc('horror-overlay.png')"/>
    <PoolItem class="depth" v-if="depth" type="resource" :amount="depth" />
    <div class="spent-keys" v-if="spentKeys.length > 0">
      <Key v-for="key in spentKeys" :key="key" :name="key" />
    </div>
    <div v-if="customizationsCard" class="customizations-wrapper" :class="{mutated}">
      <img :src="customizationsCard" />
      <div v-for="label in customizationLabels" :key="label[0]" :class="`label label-${cardCode} ${label[0]}`">
        <svg xmlns="http://www.w3.org/2000/svg" width="100" height="20" viewBox="0 0 100 20">
         <g>
          <path id="svg-text" d="M 0 10 H 100" fill="transparent" stroke="lightgray" />
          <text><textPath xlink:href="#svg-text" method="stretch" lengthAdjust="spacingAndGlyphs" textLength="100%">{{ label[1] }}</textpath></text>
         </g>
        </svg>
      </div>
      <div v-for="skill in customizationSkills" :key="skill" :class="`skill skill-${cardCode} ${skill}`">
      </div>
      <div v-for="index in customizationIndexes" :key="index" :class="`index index-${cardCode} ${index}`">
      </div>
      <div v-for="tick in customizationTicks" :key="tick" :class="`tick tick-${cardCode} ${tick}`">
        <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24"><path d="M20.285 2l-11.285 11.567-5.286-5.011-3.714 3.716 9 8.728 15-15.285z"/></svg>
      </div>
    </div>
    <div class="card-data" v-if="dbCardCustomizationText">
      <p v-if="dbCardName"><b>{{ dbCardName }}</b></p>
      <p v-if="dbCardCustomizationText" v-html="dbCardCustomizationText" style="font-size: 0.85em;"></p>
    </div>
  </div>
</template>

<style scoped lang="scss">
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
  -ms-overflow-style: none; /* IE and Edge */
  scrollbar-width: none; /* Firefox */
  scroll-behavior: smooth;
  background-color: rgba(185, 185, 185, 0.85);
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.75);
}

.card-data::-webkit-scrollbar {
  display: none; /* Chrome, Safari, Opera*/
}

.card-overlay {
  position: absolute;
  z-index: 1000;
  display: flex;
  img {
    box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.75);
    border-radius: 15px;
    width: 300px;
    height: fit-content;
    aspect-ratio: var(--card-aspect);
  }
  img.damage {
    width: auto;
    height: 22px;
    position: absolute;
    top: 53.5%;
    left: 34.5%;
    &.damage-2 {
      top: 52%;
      left: 27.5%;
    }

    &.damage-3 {
      top: 50.5%;
      left: 20.5%;
    }


  }
  img.horror {
    width: auto;
    height: 22px;
    position: absolute;
    top: 53.5%;
    left: 57.5%;
    &.horror-2 {
      top: 52%;
      left: 64.5%;
    }

    &.horror-3 {
      top: 50.5%;
      left: 71.5%;
    }

  }
  &.sideways {
    height: 300px !important;
    //width: fit-content !important;
    //aspect-ratio: var(--card-sideways-aspect);
    width: auto;
    @media (max-width: 800px) and (orientation: portrait){
      overflow: auto;
    }
    img {
      aspect-ratio: var(--card-sideways-aspect);
      border-radius: 15px;
      height: 300px;
      width: auto;
      max-width: unset;
    }
  }
  &.tarot {
    height: 500px !important;
    width: fit-content !important;
    img {
      aspect-ratio: var(--card-tarot-aspect);
      border-radius: 15px;
      height: 500px;
      width: fit-content;
    }
  }
}

.reversed {
  transform: rotateZ(180deg);
}

.customizations-wrapper {
  position: relative;
  width: fit-content;
  height: fit-content;
  margin-left: 2px;
}

.label {
  line-height: 1;
  position: absolute;
  width: 100%;
  height: 1em;
}

.tick {
  line-height: 0;
  position: absolute;
  width: 2.8%;
  height: 2.8%;

  svg {
    width: 100%;
    height: 100%;
  }
}

.skill {
  line-height: 0;
  position: absolute;
  width: 7%;
  aspect-ratio: 1/1;
  border-radius: 50%;
  border: 1px solid #222;
  background-color: rgba(0,0,0,0.4);
}

.index {
  line-height: 0;
  position: absolute;
  width: 7%;
  aspect-ratio: 1/1;
  border-radius: 50%;
  border: 1px solid #222;
  background-color: rgba(0,0,0,0.4);
}

// Hunter's Armor
.tick-09021 {
  --top-0: 19.1%;
  --top-1: 30.0%;
  --top-2: 40.9%;
  --top-3: 45.1%;
  --top-4: 49.4%;
  --top-5: 60.3%;
  --top-6: 74.4%;
  --left-1: 8.6%;
  --left-2: 11.6%;
  --left-3: 15.2%;
}

.customization-09021-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09021-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09021-1-2 {
  top: var(--top-1);
  left: var(--left-2);
}
.customization-09021-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09021-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09021-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09021-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09021-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09021-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09021-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09021-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09021-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09021-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09021-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09021-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}

// Runic Axe
.tick-09022 {
  --top-0: 18.6%;
  --top-1: 25.3%;
  --top-2: 34.9%;
  --top-3: 47.2%;
  --top-4: 56.7%;
  --top-5: 69.3%;
  --top-6: 75.9%;
  --top-7: 82.4%;
  --left-1: 8.6%;
  --left-2: 11.6%;
  --left-3: 14.7%;
  --left-4: 17.7%;
}

.customization-09022-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09022-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09022-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09022-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09022-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09022-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09022-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09022-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09022-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09022-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09022-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09022-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09022-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09022-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09022-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}
.customization-09022-7-4 {
  top: var(--top-7);
  left: var(--left-4);
}

// Custom Modifications
.tick-09023 {
  --top-0: 19.1%;
  --top-1: 33.4%;
  --top-2: 40.9%;
  --top-3: 51.7%;
  --top-4: 62.5%;
  --top-5: 73.3%;
  --left-1: 8.6%;
  --left-2: 11.9%;
  --left-3: 15.4%;
  --left-4: 19.1%;
}

.customization-09023-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09023-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09023-1-2 {
  top: var(--top-1);
  left: var(--left-2);
}
.customization-09023-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09023-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09023-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09023-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09023-3-3 {
  top: var(--top-3);
  left: var(--left-3);
}
.customization-09023-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09023-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09023-4-3 {
  top: var(--top-4);
  left: var(--left-3);
}
.customization-09023-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09023-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09023-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09023-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}

// Alchemical Distillation
.tick-09040 {
  --top-0: 19.1%;
  --top-1: 26.7%;
  --top-2: 34.3%;
  --top-3: 45.1%;
  --top-4: 52.8%;
  --top-5: 60.2%;
  --top-6: 74.3%;
  --left-1: 8.6%;
  --left-2: 11.9%;
  --left-3: 15.4%;
  --left-4: 19.1%;
  --left-5: 22.4%;
}

.customization-09040-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09040-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09040-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09040-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09040-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09040-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09040-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09040-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09040-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09040-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}
.customization-09040-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09040-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09040-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09040-6-4 {
  top: var(--top-6);
  left: var(--left-4);
}
.customization-09040-6-5 {
  top: var(--top-6);
  left: var(--left-5);
}

// Empirical Hypothesis
.tick-09041 {
  --top-0: 18.4%;
  --top-1: 25.2%;
  --top-2: 31.8%;
  --top-3: 38.3%;
  --top-4: 45.0%;
  --top-5: 57.4%;
  --top-6: 67.0%;
  --top-7: 76.5%;
  --left-1: 8.6%;
  --left-2: 11.5%;
  --left-3: 14.4%;
  --left-4: 17.5%;
}

.customization-09041-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09041-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09041-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09041-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09041-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09041-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09041-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09041-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09041-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09041-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}
.customization-09041-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09041-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09041-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09041-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09041-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09041-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}
.customization-09041-7-4 {
  top: var(--top-7);
  left: var(--left-4);
}

// The Raven Quill
.tick-09042 {
  --top-1: 24.8%;
  --top-2: 31.2%;
  --top-3: 37.7%;
  --top-4: 44.4%;
  --top-5: 51.0%;
  --top-6: 60.3%;
  --top-7: 70.0%;
  --left-1: 8.6%;
  --left-2: 11.5%;
  --left-3: 14.4%;
  --left-4: 17.5%;
}

.label-09042-0-0 {
  top: 18%;
  left: 52%;
  width: 40%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}

.label-09042-4-0 {
  top: 46.5%;
  left: 18%;
  width: 36%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}

.label-09042-4-1 {
  top: 46.5%;
  left: 55%;
  width: 35%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}

.customization-09042-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09042-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09042-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09042-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09042-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09042-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09042-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09042-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09042-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09042-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09042-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09042-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09042-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09042-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}
.customization-09042-7-4 {
  top: var(--top-7);
  left: var(--left-4);
}

// Damning Testimony
.tick-09059 {
  --top-0: 18.6%;
  --top-1: 32.7%;
  --top-2: 40.2%;
  --top-3: 47.6%;
  --top-4: 61.9%;
  --top-5: 72.9%;
  --left-1: 8.5%;
  --left-2: 11.9%;
  --left-3: 15.2%;
  --left-4: 18.5%;
}

.customization-09059-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09059-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09059-1-2 {
  top: var(--top-1);
  left: var(--left-2);
}
.customization-09059-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09059-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09059-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09059-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09059-3-3 {
  top: var(--top-3);
  left: var(--left-3);
}
.customization-09059-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09059-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09059-4-3 {
  top: var(--top-4);
  left: var(--left-3);
}
.customization-09059-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09059-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09059-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09059-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}

// Friends in Low Places
.tick-09060 {
  --top-1: 24.3%;
  --top-2: 34.0%;
  --top-3: 46.5%;
  --top-4: 56.0%;
  --top-5: 65.4%;
  --top-6: 72.1%;
  --top-7: 78.6%;
  --left-1: 8.5%;
  --left-2: 11.3%;
  --left-3: 14.2%;
}

.label-09060-0-0 {
  top: 18%;
  left: 29%;
  width: 40%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}

.label-09060-2-0 {
  top: 33.4%;
  left: 66%;
  width: 25%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}

.customization-09060-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09060-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09060-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09060-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09060-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09060-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09060-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09060-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09060-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09060-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09060-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09060-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09060-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09060-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09060-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}

// Honed Instinct
.tick-09061 {
  --top-0: 19.0%;
  --top-1: 25.6%;
  --top-2: 32.3%;
  --top-3: 38.6%;
  --top-4: 45.4%;
  --top-5: 52.1%;
  --top-6: 58.6%;
  --top-7: 68.2%;
  --left-1: 8.4%;
  --left-2: 11.3%;
  --left-3: 14.2%;
  --left-4: 17.3%;
  --left-5: 20.6%;
}

.customization-09061-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09061-0-2 {
  top: var(--top-0);
  left: var(--left-2);
}
.customization-09061-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09061-1-2 {
  top: var(--top-1);
  left: var(--left-2);
}
.customization-09061-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09061-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09061-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09061-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09061-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09061-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09061-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09061-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09061-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09061-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}
.customization-09061-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09061-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09061-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09061-6-4 {
  top: var(--top-6);
  left: var(--left-4);
}
.customization-09061-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09061-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09061-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}
.customization-09061-7-4 {
  top: var(--top-7);
  left: var(--left-4);
}
.customization-09061-7-5 {
  top: var(--top-7);
  left: var(--left-5);
}

// Living Ink
.tick-09079 {
  --top-1: 25.5%;
  --top-2: 36.5%;
  --top-3: 50.7%;
  --top-4: 61.6%;
  --top-5: 65.7%;
  --top-6: 69.9%;
  --top-7: 80.8%;
  --left-1: 8.4%;
  --left-2: 11.8%;
  --left-3: 15.2%;
}

.skill-09079-SkillWillpower {
  top: 18.6%;
  left: 42.5%;
}

.skill-09079-SkillIntellect {
  top: 18.6%;
  left: 55%;
}

.skill-09079-SkillCombat {
  top: 18.6%;
  left: 68.4%;
}

.skill-09079-SkillAgility {
  top: 18.6%;
  left: 81%;
}

.customization-09079-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09079-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09079-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09079-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09079-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09079-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09079-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09079-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09079-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09079-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09079-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09079-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09079-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09079-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09079-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}

// Summoned Servitor
.tick-09080 {
  --top-0: 18.3%;
  --top-1: 27.8%;
  --top-2: 37.4%;
  --top-3: 49.8%;
  --top-4: 56.4%;
  --top-5: 66.0%;
  --top-6: 72.6%;
  --top-7: 82.0%;
  --left-1: 8.4%;
  --left-2: 11.2%;
  --left-3: 14.3%;
  --left-4: 17.3%;
  --left-5: 20.3%;
}

.index-09080-5-0 {
  top: 68.4%;
  height: 4%;
  border-radius: 40%;
  left: 36.5%;
  width: 11.5%;
}

.index-09080-5-1 {
  top: 68.4%;
  height: 4%;
  border-radius: 40%;
  left: 49.5%;
  width: 14%;
}

.customization-09080-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09080-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09080-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09080-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09080-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09080-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09080-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09080-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09080-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09080-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09080-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09080-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09080-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}
.customization-09080-7-4 {
  top: var(--top-7);
  left: var(--left-4);
}
.customization-09080-7-5 {
  top: var(--top-7);
  left: var(--left-5);
}

// Power Word
.tick-09081 {
  --top-0: 18.5%;
  --top-1: 28.2%;
  --top-2: 37.6%;
  --top-3: 47.1%;
  --top-4: 56.7%;
  --top-5: 63.4%;
  --top-6: 72.9%;
  --top-7: 79.5%;
  --left-1: 8.4%;
  --left-2: 11.2%;
  --left-3: 14.2%;
}

// Power Word (Mutated)
.mutated .tick-09081 {
  --top-0: 18.8%;
  --top-1: 28.4%;
  --top-2: 34.9%;
  --top-3: 44.4%;
  --top-4: 54.0%;
  --top-5: 60.7%;
  --top-6: 70.1%;
  --top-7: 76.7%;
  --left-1: 8.4%;
  --left-2: 11.2%;
  --left-3: 14.2%;
}

.customization-09081-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09081-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09081-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09081-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09081-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09081-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09081-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09081-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09081-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09081-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09081-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09081-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09081-7-1 {
  top: var(--top-7);
  left: var(--left-1);
}
.customization-09081-7-2 {
  top: var(--top-7);
  left: var(--left-2);
}
.customization-09081-7-3 {
  top: var(--top-7);
  left: var(--left-3);
}

// Pocket Multi Tool
.tick-09099 {
  --top-0: 19.1%;
  --top-1: 30.0%;
  --top-2: 37.6%;
  --top-3: 45.0%;
  --top-4: 52.7%;
  --top-5: 60.1%;
  --top-6: 67.8%;
  --left-1: 8.4%;
  --left-2: 11.6%;
  --left-3: 15.2%;
  --left-4: 18.4%;
}

.customization-09099-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09099-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09099-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09099-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09099-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09099-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09099-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09099-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09099-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09099-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09099-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09099-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09099-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09099-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09099-6-4 {
  top: var(--top-6);
  left: var(--left-4);
}

// Makeshift Trap
.tick-09100 {
  --top-0: 19.2%;
  --top-1: 26.8%;
  --top-2: 37.6%;
  --top-3: 45.0%;
  --top-4: 55.9%;
  --top-5: 66.9%;
  --top-6: 77.8%;
  --left-1: 8.4%;
  --left-2: 11.8%;
  --left-3: 15.2%;
  --left-4: 18.7%;
}

.customization-09100-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09100-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09100-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09100-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09100-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09100-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09100-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09100-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09100-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09100-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09100-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09100-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09100-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09100-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09100-6-4 {
  top: var(--top-6);
  left: var(--left-4);
}

// Grizzled
.tick-09101 {
  --top-1: 25.4%;
  --top-2: 33.6%;
  --top-3: 41.6%;
  --top-4: 59.7%;
  --top-5: 74.6%;
  --left-1: 8.4%;
  --left-2: 12.0%;
  --left-3: 15.3%;
  --left-4: 19.0%;
  --left-5: 22.4%;
}

.label-09101-0-0 {
  top: 18%;
  left: 35.2%;
  width: 25%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}
.label-09101-0-1 {
  top: 18%;
  left: 64%;
  width: 25%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}
.label-09101-1-0 {
  top: 27.5%;
  left: 8%;
  width: 25%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}
.label-09101-2-0 {
  top: 35.5%;
  left: 8%;
  width: 25%;
  height: 5%;
  svg {
    width: 100%;
    height: 100%;
    path {
      d: path("M 0 14 H 100");
    }
  }
}

.customization-09101-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09101-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09101-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09101-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09101-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09101-3-3 {
  top: var(--top-3);
  left: var(--left-3);
}
.customization-09101-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09101-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09101-4-3 {
  top: var(--top-4);
  left: var(--left-3);
}
.customization-09101-4-4 {
  top: var(--top-4);
  left: var(--left-4);
}
.customization-09101-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09101-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09101-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09101-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}
.customization-09101-5-5 {
  top: var(--top-5);
  left: var(--left-5);
}

// Hyperphysical Shotcaster
.tick-09119 {
  --top-0: 19.0%;
  --top-1: 28.4%;
  --top-2: 40.6%;
  --top-3: 55.6%;
  --top-4: 67.9%;
  --top-5: 80.1%;
  --top-6: 86.4%;
  --left-1: 8.4%;
  --left-2: 11.2%;
  --left-3: 14.2%;
  --left-4: 17.3%;
}

.customization-09119-0-1 {
  top: var(--top-0);
  left: var(--left-1);
}
.customization-09119-0-2 {
  top: var(--top-0);
  left: var(--left-2);
}
.customization-09119-1-1 {
  top: var(--top-1);
  left: var(--left-1);
}
.customization-09119-1-2 {
  top: var(--top-1);
  left: var(--left-2);
}
.customization-09119-2-1 {
  top: var(--top-2);
  left: var(--left-1);
}
.customization-09119-2-2 {
  top: var(--top-2);
  left: var(--left-2);
}
.customization-09119-3-1 {
  top: var(--top-3);
  left: var(--left-1);
}
.customization-09119-3-2 {
  top: var(--top-3);
  left: var(--left-2);
}
.customization-09119-4-1 {
  top: var(--top-4);
  left: var(--left-1);
}
.customization-09119-4-2 {
  top: var(--top-4);
  left: var(--left-2);
}
.customization-09119-5-1 {
  top: var(--top-5);
  left: var(--left-1);
}
.customization-09119-5-2 {
  top: var(--top-5);
  left: var(--left-2);
}
.customization-09119-5-3 {
  top: var(--top-5);
  left: var(--left-3);
}
.customization-09119-5-4 {
  top: var(--top-5);
  left: var(--left-4);
}
.customization-09119-6-1 {
  top: var(--top-6);
  left: var(--left-1);
}
.customization-09119-6-2 {
  top: var(--top-6);
  left: var(--left-2);
}
.customization-09119-6-3 {
  top: var(--top-6);
  left: var(--left-3);
}
.customization-09119-6-4 {
  top: var(--top-6);
  left: var(--left-4);
}

.Reversed {
  transform: rotateZ(180deg);
}

.card-image {
  position: relative;
}

@keyframes fadeIn {
  0% {
    opacity: 0;
  }
  100% {
    opacity: 1;
  }
}

.card-overlay {
  width: max-content;
  height: auto;
  position: absolute;
  top: 0;
  left: 2px;
  pointer-events: none;
  animation: fadeIn 0.5s;
  @media (max-width: 800px) and (orientation: portrait) {
    position: auto;
  }
}

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

  &:deep(img) {
    border-radius: 2px;
    width: 25px;
    height: 25px;
  }
}

.isMobile {
  inset: 0 !important;
  margin: auto;
  align-self: center;
  justify-content: center;
  width: fit-content;
}

.depth {
  left: calc(50% - 20px);
  bottom: 16%;
  position: absolute;
  width: 40px;
}

</style>
