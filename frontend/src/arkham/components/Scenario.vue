<script lang="ts" setup>
import UpgradeDeck from '@/arkham/components/UpgradeDeck.vue';
import { EyeIcon, QuestionMarkCircleIcon, ViewColumnsIcon, ArchiveBoxXMarkIcon, ArrowPathIcon, LockClosedIcon, LockOpenIcon, ArrowUturnLeftIcon } from '@heroicons/vue/20/solid'
import {
  watchEffect,
  watch,
  onMounted,
  onUpdated,
  onBeforeUnmount,
  computed,
  nextTick,
  ref,
  ComputedRef,
  reactive,
  provide
} from 'vue';
import { type Game } from '@/arkham/types/Game';
import { type Scenario } from '@/arkham/types/Scenario';
import { type Enemy } from '@/arkham/types/Enemy';
import { type ConcealedCard } from '@/arkham/types/ConcealedCard';
import ConcealedCardView from '@/arkham/components/ConcealedCard.vue';
import { type Position } from '@/arkham/types/Placement';
import { type Card, cardId, cardImage } from '@/arkham/types/Card';
import { TarotCard, tarotCardImage } from '@/arkham/types/TarotCard';
import { TokenType } from '@/arkham/types/Token';
import { ModifierType, Hollow } from '@/arkham/types/Modifier';
import { Source } from '@/arkham/types/Source';
import { type Target } from '@/arkham/types/Target';
import { Message, AbilityMessage, AbilityLabel } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import { waitForImagesToLoad, imgsrc, groupBy } from '@/arkham/helpers';
import { gameLocalStorageKey, getGameLocalStorageItem, setGameLocalStorageItem } from '@/arkham/localStorage';
import { cardImage as cardCodeImage } from '@/arkham/cardImages';
import { fullName } from '@/arkham/types/Name';
import { useMenu } from '@/composable/menu';
import { useSettings } from '@/stores/settings';
import { keyToId } from '@/arkham/types/Key'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Act from '@/arkham/components/Act.vue';
import CardView from '@/arkham/components/Card.vue';
import Draggable from '@/components/Draggable.vue';
import ChaosBag from '@/arkham/components/ChaosBag.vue';
import Agenda from '@/arkham/components/Agenda.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import EnemyView from '@/arkham/components/Enemy.vue';
import CardRow from '@/arkham/components/CardRow.vue';
import KeyToken from '@/arkham/components/Key.vue';
import PlayerTabs from '@/arkham/components/PlayerTabs.vue';
import Connections from '@/arkham/components/Connections.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import EncounterDeck from '@/arkham/components/EncounterDeck.vue';
import VictoryDisplay from '@/arkham/components/VictoryDisplay.vue';
import SkillTest from '@/arkham/components/SkillTest.vue';
import ScenarioDeck from '@/arkham/components/ScenarioDeck.vue';
import ScenarioDebug from '@/arkham/components/ScenarioDebug.vue';
import CardsUnderIndicator from '@/arkham/components/CardsUnderIndicator.vue';
import Story from '@/arkham/components/Story.vue';
import Asset from '@/arkham/components/Asset.vue';
import Location from '@/arkham/components/Location.vue';
import TreacheryView from '@/arkham/components/Treachery.vue';
import { useGameChoices } from '@/arkham/composables/useGameChoices';
import { setLocationOffset, resetLocationOffsets } from '@/arkham/api';
import { useDebug, scenarioHasDebugOptions } from '@/arkham/debug'
import { storeToRefs } from 'pinia';
import { useI18n } from 'vue-i18n';
import { IsMobile } from '@/arkham/isMobile';
const { t } = useI18n();

// types
interface RefWrapper<T> {
  ref: ComputedRef<T>
}

const tarotCardBackground = `url(${imgsrc('background.jpg')})`

// Setup
export interface Props {
  game: Game
  scenario: Scenario
  playerId: string
  realityAcidLightDevoured?: boolean
  realityAcidLightActive?: boolean
}
const props = defineProps<Props>()
const emit = defineEmits(['choose', 'toggleRealityAcidLight'])
const debug = useDebug()
const { addEntry, removeEntry } = useMenu()

const upgradeDeck = computed(() => Object.values(props.game.question).some((q) => q.tag === 'ChooseUpgradeDeck'))

// emit helpers
const choose = async (idx: number) => emit('choose', idx)

//Refs
const settingsStore = useSettings()
const { splitView } = storeToRefs(settingsStore)
const { toggleSplitView, setGameId } = settingsStore
const needsInit = ref(true)
const showChaosBag = ref(false)
const showOutOfPlay = ref(false)
const forcedShowOutOfPlay = ref(false)
const forcedShowDiscard = ref(false)
const encounterDiscardPopoverShown = ref(false)
const spectralDiscardPopoverShown = ref(false)
const hollowedPopoverShown = ref(false)
const showScenarioDebugOptions = ref(false)
const realityAcidLightAnchor = ref<HTMLElement | null>(null)
const realityAcidLightRect = reactive({ left: 0, top: 0, width: 0, height: 0 })
const locationMap = ref<Element | null>(null)
const scrollerRef = ref<HTMLElement | null>(null)
const viewingDiscard = ref(false)
const revealingCards = ref(false)
const cardRowTitle = ref("")
// Atlach Nacha specific refs
const previousRotation = ref(0)
const legsSet = ref(["legs1", "legs2", "legs3", "legs4"])

let legObserver: MutationObserver | null = null
let cosmicEmissaryObserver: MutationObserver | null = null
let cosmicEmissaryResizeObserver: ResizeObserver | null = null
let cosmicEmissaryCompactRequest: number | null = null
let cosmicEmissaryCompactForce = false

function updateRealityAcidLightRect() {
  const rect = (realityAcidLightAnchor.value ?? document.querySelector<HTMLElement>('.reality-acid-light-switch-anchor'))?.getBoundingClientRect()
  realityAcidLightRect.left = rect?.left ?? 0
  realityAcidLightRect.top = rect?.top ?? 0
  realityAcidLightRect.width = rect?.width ?? 0
  realityAcidLightRect.height = rect?.height ?? 0
}

function readStyleMapCache(key: string): Record<string, Record<string, string>> {
  try {
    const cached = JSON.parse(sessionStorage.getItem(key) ?? '{}') as Record<string, Record<string, string>>
    for (const style of Object.values(cached)) {
      const match = style.transform?.match(/^translate\(([^,]+),\s*([^\)]+)\)$/)
      if (match) {
        style.translate = `${match[1]} ${match[2]}`
        delete style.transform
      }
    }
    return cached
  } catch {
    return {}
  }
}

function writeStyleMapCache(key: string, value: Record<string, Record<string, string>>) {
  try {
    sessionStorage.setItem(key, JSON.stringify(value))
  } catch {
    // Ignore storage failures; the in-memory ref still prevents normal update hops.
  }
}

const cosmicEmissaryEnemyStylesCacheKey = gameLocalStorageKey(props.game.id, 'cosmicEmissaryEnemyStyles')
const cosmicEmissaryLocationCellStylesCacheKey = gameLocalStorageKey(props.game.id, 'cosmicEmissaryLocationCellStyles')
const cosmicEmissaryAnimationSettingKey = gameLocalStorageKey(props.game.id, 'enableCosmicEmissaryAnimation')
const cachedCosmicEmissaryEnemyStyles = readStyleMapCache(cosmicEmissaryEnemyStylesCacheKey)
const cachedCosmicEmissaryLocationCellStyles = readStyleMapCache(cosmicEmissaryLocationCellStylesCacheKey)
const cosmicEmissaryEnemyStyles = ref<Record<string, Record<string, string>>>(cachedCosmicEmissaryEnemyStyles)
const cosmicEmissaryLocationCellStyles = ref<Record<string, Record<string, string>>>(cachedCosmicEmissaryLocationCellStyles)
const cosmicEmissaryFormationHasMeasured = ref(Object.keys(cachedCosmicEmissaryEnemyStyles).length > 0)
const enableCosmicEmissaryAnimation = ref(
  getGameLocalStorageItem(props.game.id, 'enableCosmicEmissaryAnimation') === null
    ? getGameLocalStorageItem(props.game.id, 'disableCosmicEmissaryAnimation') !== 'true'
    : getGameLocalStorageItem(props.game.id, 'enableCosmicEmissaryAnimation') !== 'false'
)
const locationsZoom = ref(parseFloat(getGameLocalStorageItem(props.game.id, 'locationsZoom') ?? '1'))
const doubleZoomActive = ref(false)
const doubleZoomPrevValue = ref(1)
const doubleZoomPrevScroll = { left: 0, top: 0 }
const DOUBLE_ZOOM_LEVEL = 3
watch(locationsZoom, async (value) => {
  setGameLocalStorageItem(props.game.id, 'locationsZoom', String(value))
  await updateScrollMargins()
})

function zoomStep(value: number): number {
  const center = 1.5  // peak step around the middle of the normal range
  const sigma = 1.0  // controls how quickly the step tapers off
  const max = 0.15
  const min = 0.01
  return Math.max(min, max * Math.exp(-Math.pow(value - center, 2) / (2 * sigma * sigma)))
}

function increaseZoom() {
  locationsZoom.value = parseFloat((locationsZoom.value + zoomStep(locationsZoom.value)).toFixed(3))
}

function decreaseZoom() {
  locationsZoom.value = parseFloat(Math.max(0.01, locationsZoom.value - zoomStep(locationsZoom.value)).toFixed(3))
}

const locationsUnlocked = ref(false)
const draggingLocationId = ref<string | null>(null)
// Optimistic offsets after a drop, kept until the server echoes them back.
// Stored in canonical (rotationSteps=0) coordinates, same as the backend.
const pendingOffsets = ref<Record<string, { x: number, y: number }>>({})

// Plain (non-reactive) drag state. Mutated on every pointermove without
// triggering Vue re-renders; the live drag visual is applied via direct DOM.
type DragInternal = {
  locationId: string
  element: HTMLElement
  pointerId: number
  startX: number
  startY: number
  // Canonical base offset (matches what's stored on the backend).
  baseCanonicalX: number
  baseCanonicalY: number
  // Pre-drag screen-space offset = displayOffset(base).
  baseScreenX: number
  baseScreenY: number
  // Updated canonical offset after current drag delta (used by Vue fallback
  // path if a mid-drag render happens).
  canonicalFinalX: number
  canonicalFinalY: number
  // Capture rotation at start so mid-drag rotation changes don't desync.
  rotationAtStart: number
  moved: boolean
}
let dragInternal: DragInternal | null = null
const DRAG_THRESHOLD_PX = 3

// Measured location-cell dimensions (in unscaled CSS px). Updated after layout
// changes so the rotation math can compensate for non-square cells: the grid
// reshuffles via grid-template-areas (cells don't visually rotate), so a
// "1 cell right" displacement before rotation isn't the same pixel distance as
// "1 cell down" after rotation when cells aren't square.
const cellDimensions = ref<{ w: number, h: number }>({ w: 1, h: 1 })

function updateCellDimensions() {
  nextTick(() => {
    const cell = document.querySelector('.location-cell') as HTMLElement | null
    if (!cell) return
    const rect = cell.getBoundingClientRect()
    const zoom = locationsZoom.value || 1
    if (rect.width > 0 && rect.height > 0) {
      cellDimensions.value = { w: rect.width / zoom, h: rect.height / zoom }
    }
  })
}

// Screen-space 90° CW rotation in pixels, with aspect-ratio correction so
// "N cells worth of horizontal pixels" maps to "N cells worth of vertical
// pixels" after rotation (and vice versa). For square cells this reduces to
// the plain (-y, x) matrix; for rectangular cells it preserves the visual
// relationship between cells across rotations. 180° always yields (-x, -y)
// regardless of aspect ratio, which is why 180° looked right with the naive
// matrix while 90°/270° looked off.
function rotateOffset(off: { x: number, y: number }, steps: number): { x: number, y: number } {
  const k = ((steps % 4) + 4) % 4
  const { w, h } = cellDimensions.value
  const ratio = w > 0 && h > 0 ? w / h : 1
  let { x, y } = off
  for (let i = 0; i < k; i++) {
    const nx = -y * ratio
    const ny = x / ratio
    x = nx; y = ny
  }
  return { x, y }
}

const locationOffsets = computed<Record<string, { x: number, y: number }>>(() => {
  // Iterate props.game.locations directly so this computed doesn't depend on
  // the `locations` ref (which is declared later in this setup script and
  // would otherwise TDZ when the watch below registers its source eagerly).
  const offsets: Record<string, { x: number, y: number }> = {}
  for (const loc of Object.values(props.game.locations)) {
    for (const m of loc.modifiers ?? []) {
      if (m.type.tag !== 'UIModifier') continue
      const c = m.type.contents as any
      if (c && typeof c === 'object' && c.tag === 'Positioned') {
        offsets[loc.id] = { x: c.x, y: c.y }
      }
    }
  }
  return offsets
})

const hasAnyOffset = computed(() =>
  Object.keys(locationOffsets.value).length > 0
    || Object.keys(pendingOffsets.value).length > 0
)

// Padding to extend the scroll area so dragged locations near the edges
// aren't clipped. Transforms don't expand the parent's layout box, so we
// measure each moved cell's actual displaced position against the grid's
// content bounds and only add padding for real overflow — a location moved
// "into" the grid (e.g. a bottom-row cell nudged up) shouldn't grow padding.
const layoutPadding = ref({ left: 0, right: 0, top: 0, bottom: 0 })

async function updateLayoutPadding() {
  await nextTick()
  const grid = (locationMap.value as any)?.$el ?? locationMap.value as HTMLElement | null
  if (!grid) return

  const current = layoutPadding.value
  const allOffsets: Record<string, { x: number, y: number }> = {
    ...locationOffsets.value,
    ...pendingOffsets.value,
  }

  if (Object.keys(allOffsets).length === 0) {
    if (current.left || current.right || current.top || current.bottom) {
      layoutPadding.value = { left: 0, right: 0, top: 0, bottom: 0 }
    }
    return
  }

  const contentWidth = grid.clientWidth - current.left - current.right
  const contentHeight = grid.clientHeight - current.top - current.bottom

  let left = 0, right = 0, top = 0, bottom = 0
  const cells = grid.querySelectorAll('.location-cell[data-location-id]') as NodeListOf<HTMLElement>
  for (const cell of cells) {
    const id = cell.dataset.locationId
    if (!id) continue
    const offset = allOffsets[id]
    if (!offset) continue

    const rot = rotateOffset(offset, rotationSteps.value)
    // offsetLeft/offsetTop are relative to the offset parent's border box and
    // ignore CSS transforms, so they give us the cell's natural grid position
    // *including* the grid's current padding — subtract it to get content-area
    // coords, which are stable across padding updates.
    const cellLeft = cell.offsetLeft - current.left + rot.x
    const cellTop = cell.offsetTop - current.top + rot.y
    const cellRight = cellLeft + cell.offsetWidth
    const cellBottom = cellTop + cell.offsetHeight

    if (cellLeft < 0) left = Math.max(left, -cellLeft)
    if (cellTop < 0) top = Math.max(top, -cellTop)
    if (cellRight > contentWidth) right = Math.max(right, cellRight - contentWidth)
    if (cellBottom > contentHeight) bottom = Math.max(bottom, cellBottom - contentHeight)
  }

  if (left !== current.left || right !== current.right || top !== current.top || bottom !== current.bottom) {
    layoutPadding.value = { left, right, top, bottom }
  }
}

// Returns the CANONICAL offset for a location (server-side coordinate frame).
function effectiveOffset(locationId: string): { x: number, y: number } {
  if (dragInternal && dragInternal.locationId === locationId && dragInternal.moved) {
    return { x: dragInternal.canonicalFinalX, y: dragInternal.canonicalFinalY }
  }
  return pendingOffsets.value[locationId] ?? locationOffsets.value[locationId] ?? { x: 0, y: 0 }
}

// Returns only the user-offset transform. Grid placement lives on the wrapper
// `.location-cell` so Vue's TransitionGroup FLIP can animate the wrapper
// without clobbering this transform during rotation reshuffles.
function locationOffsetStyle(location: { id: string }) {
  const canonical = effectiveOffset(location.id)
  // Apply the user's current rotation so the offset moves with the rotated
  // layout instead of staying in absolute screen space.
  const off = rotateOffset(canonical, rotationSteps.value)
  const style: Record<string, string> = {}
  if (off.x !== 0 || off.y !== 0) {
    style.transform = `translate(${off.x}px, ${off.y}px)`
  }
  return style
}

function onLocationPointerDown(event: PointerEvent, location: { id: string }) {
  if (!locationsUnlocked.value) return
  event.preventDefault()
  event.stopPropagation()
  const element = event.currentTarget as HTMLElement | null
  if (!element) return
  const baseCanonical = effectiveOffset(location.id)
  const baseScreen = rotateOffset(baseCanonical, rotationSteps.value)
  dragInternal = {
    locationId: location.id,
    element,
    pointerId: event.pointerId,
    startX: event.clientX,
    startY: event.clientY,
    baseCanonicalX: baseCanonical.x,
    baseCanonicalY: baseCanonical.y,
    baseScreenX: baseScreen.x,
    baseScreenY: baseScreen.y,
    canonicalFinalX: baseCanonical.x,
    canonicalFinalY: baseCanonical.y,
    rotationAtStart: rotationSteps.value,
    moved: false,
  }
  window.addEventListener('pointermove', onWindowPointerMove, { passive: true })
  window.addEventListener('pointerup', onWindowPointerUp, { passive: true })
  window.addEventListener('pointercancel', onWindowPointerUp, { passive: true })
}

function onWindowPointerMove(event: PointerEvent) {
  if (!dragInternal || dragInternal.pointerId !== event.pointerId) return
  const zoom = locationsZoom.value || 1
  const screenDx = (event.clientX - dragInternal.startX) / zoom
  const screenDy = (event.clientY - dragInternal.startY) / zoom
  if (!dragInternal.moved
    && Math.hypot(event.clientX - dragInternal.startX, event.clientY - dragInternal.startY) > DRAG_THRESHOLD_PX) {
    dragInternal.moved = true
    draggingLocationId.value = dragInternal.locationId
  }
  if (dragInternal.moved) {
    // Live visual: screen-space delta on top of pre-drag screen position.
    const screenX = dragInternal.baseScreenX + screenDx
    const screenY = dragInternal.baseScreenY + screenDy
    dragInternal.element.style.transform = `translate(${screenX}px, ${screenY}px)`
    // Mirror it in canonical coords (inverse rotation) for commit + fallback.
    const canonicalDelta = rotateOffset({ x: screenDx, y: screenDy }, -dragInternal.rotationAtStart)
    dragInternal.canonicalFinalX = dragInternal.baseCanonicalX + canonicalDelta.x
    dragInternal.canonicalFinalY = dragInternal.baseCanonicalY + canonicalDelta.y
  }
}

function onWindowPointerUp(event: PointerEvent) {
  if (!dragInternal || dragInternal.pointerId !== event.pointerId) return
  const drag = dragInternal
  dragInternal = null
  window.removeEventListener('pointermove', onWindowPointerMove)
  window.removeEventListener('pointerup', onWindowPointerUp)
  window.removeEventListener('pointercancel', onWindowPointerUp)
  draggingLocationId.value = null
  if (!drag.moved) return
  // Stash the drop position before the next render so the inline style stays
  // at the drop point until the server echoes the new modifier back.
  pendingOffsets.value = {
    ...pendingOffsets.value,
    [drag.locationId]: { x: drag.canonicalFinalX, y: drag.canonicalFinalY },
  }
  nextTick(() => window.dispatchEvent(new Event('arkham-location-layout-change')))
  void setLocationOffset(props.game.id, drag.locationId, drag.canonicalFinalX, drag.canonicalFinalY)
    .finally(() => nextTick(() => window.dispatchEvent(new Event('arkham-location-layout-change'))))
}

// Drop a pending entry once the server's modifier confirms it.
watch(locationOffsets, (newServer) => {
  if (Object.keys(pendingOffsets.value).length === 0) return
  const next = { ...pendingOffsets.value }
  let changed = false
  for (const id of Object.keys(next)) {
    const server = newServer[id]
    if (server && Math.abs(server.x - next[id].x) < 0.5 && Math.abs(server.y - next[id].y) < 0.5) {
      delete next[id]
      changed = true
    }
  }
  if (changed) {
    pendingOffsets.value = next
    nextTick(() => window.dispatchEvent(new Event('arkham-location-layout-change')))
  }
}, { deep: true })

function cancelActiveDrag() {
  if (!dragInternal) return
  window.removeEventListener('pointermove', onWindowPointerMove)
  window.removeEventListener('pointerup', onWindowPointerUp)
  window.removeEventListener('pointercancel', onWindowPointerUp)
  dragInternal.element.style.transform = ''
  dragInternal = null
  draggingLocationId.value = null
}

function toggleLocationsUnlocked() {
  locationsUnlocked.value = !locationsUnlocked.value
  if (!locationsUnlocked.value) cancelActiveDrag()
  nextTick(() => window.dispatchEvent(new Event('arkham-location-layout-change')))
}

function suppressLocationInteractionWhenUnlocked(event: MouseEvent) {
  if (!locationsUnlocked.value) return
  event.preventDefault()
  event.stopPropagation()
  event.stopImmediatePropagation()
}

function clearCosmicEmissaryCompactStyles() {
  cosmicEmissaryEnemyStyles.value = {}
  cosmicEmissaryLocationCellStyles.value = {}
  sessionStorage.removeItem(cosmicEmissaryEnemyStylesCacheKey)
  sessionStorage.removeItem(cosmicEmissaryLocationCellStylesCacheKey)
  cosmicEmissaryFormationHasMeasured.value = false
}

function resetLocationsLayout() {
  if (!hasAnyOffset.value) return
  pendingOffsets.value = {}
  if (props.scenario.id === 'c10651') clearCosmicEmissaryCompactStyles()
  void resetLocationOffsets(props.game.id).finally(() => {
    if (props.scenario.id === 'c10651') {
      requestCosmicEmissaryCompact(true)
      setTimeout(() => requestCosmicEmissaryCompact(true), 100)
      setTimeout(() => requestCosmicEmissaryCompact(true), 500)
    }
  })
}

let holdTimer: ReturnType<typeof setTimeout> | null = null
let holdInterval: ReturnType<typeof setInterval> | null = null

function startHold(action: () => void) {
  action()
  holdTimer = setTimeout(() => {
    holdInterval = setInterval(action, 80)
  }, 400)
}

function stopHold() {
  if (holdTimer !== null) { clearTimeout(holdTimer); holdTimer = null }
  if (holdInterval !== null) { clearInterval(holdInterval); holdInterval = null }
}

function requestCosmicEmissaryCompact(force = false) {
  cosmicEmissaryCompactForce = cosmicEmissaryCompactForce || force
  if (cosmicEmissaryCompactRequest !== null) return
  cosmicEmissaryCompactRequest = requestAnimationFrame(() => {
    const shouldForce = cosmicEmissaryCompactForce
    cosmicEmissaryCompactForce = false
    cosmicEmissaryCompactRequest = null
    compactCosmicEmissaryFormation(shouldForce)
  })
}

const { isMobile } = IsMobile();

function updateCosmicEmissaryAnimationSetting(value: string | null) {
  enableCosmicEmissaryAnimation.value = value !== 'false'
  nextTick(() => compactCosmicEmissaryFormation())
}

const onCosmicEmissaryStorage = (event: StorageEvent) => {
  if (event.key === cosmicEmissaryAnimationSettingKey) updateCosmicEmissaryAnimationSetting(event.newValue)
}

const onCosmicEmissarySettingChange = (event: Event) => {
  const detail = (event as CustomEvent<{ key?: string, value?: string }>).detail
  if (detail?.key === cosmicEmissaryAnimationSettingKey) updateCosmicEmissaryAnimationSetting(detail.value ?? null)
}

function proxyClippedLocationClick(event: MouseEvent) {
  if (event.defaultPrevented || event.button !== 0) return

  const target = event.target as HTMLElement | null
  if (!target?.closest('.location-cards-container')) return
  if (target.closest('.location-cell, .draggable, button, a, input, select, textarea, [role="button"]')) return

  const cell = [...document.querySelectorAll<HTMLElement>('.location-cell--can-interact')]
    .find((el) => {
      const rects = [el, ...el.querySelectorAll<HTMLElement>('.location-wrapper, .location, .card-frame')]
        .map((node) => node.getBoundingClientRect())
        .filter((rect) => rect.width > 0 && rect.height > 0)

      return rects.some((rect) => event.clientX >= rect.left
        && event.clientX <= rect.right
        && event.clientY >= rect.top
        && event.clientY <= rect.bottom)
    })

  if (!cell) return

  const clickTarget = cell.querySelector<HTMLElement>('.card-frame') ?? cell.querySelector<HTMLElement>('.location') ?? cell
  event.preventDefault()
  event.stopPropagation()
  clickTarget.dispatchEvent(new MouseEvent('click', {
    bubbles: true,
    cancelable: true,
    clientX: event.clientX,
    clientY: event.clientY,
    ctrlKey: event.ctrlKey,
    shiftKey: event.shiftKey,
    altKey: event.altKey,
    metaKey: event.metaKey,
  }))
}

// callbacks
onMounted(() => {
  setGameId(props.game.id)
  window.addEventListener('storage', onCosmicEmissaryStorage)
  window.addEventListener('arkham-setting-change', onCosmicEmissarySettingChange)
  window.addEventListener('resize', updateRealityAcidLightRect)
  window.addEventListener('scroll', updateRealityAcidLightRect, true)
  document.addEventListener('click', proxyClippedLocationClick, true)
  nextTick(updateRealityAcidLightRect)
  updateScrollMargins()
  updateCellDimensions()
  updateLayoutPadding()
  if(props.scenario.id === "c10651") {
    nextTick(requestCosmicEmissaryCompact)
    setTimeout(requestCosmicEmissaryCompact, 100)
    setTimeout(requestCosmicEmissaryCompact, 500)
    setTimeout(requestCosmicEmissaryCompact, 1500)

    const setupCosmicEmissaryObservers = () => {
      const locationCards = document.querySelector('.location-cards') as HTMLElement | null
      if (!locationCards || cosmicEmissaryObserver) return

      cosmicEmissaryObserver = new MutationObserver(() => nextTick(requestCosmicEmissaryCompact))
      cosmicEmissaryObserver.observe(locationCards, { childList: true, subtree: true })

      cosmicEmissaryResizeObserver = new ResizeObserver(() => requestCosmicEmissaryCompact())
      cosmicEmissaryResizeObserver.observe(locationCards)
      locationCards.querySelectorAll<HTMLElement>('[data-label]').forEach((el) => cosmicEmissaryResizeObserver?.observe(el))
    }

    nextTick(setupCosmicEmissaryObservers)
    waitForImagesToLoad(() => {
      setupCosmicEmissaryObservers()
      requestCosmicEmissaryCompact()
    })
  }

  if(props.scenario.id === "c06333") {
    waitForImagesToLoad(() => {
      nextTick(() => rotateImages(true));

      // NEW: observe for legs added back in (undo etc.) and immediately apply rotation
      const locationCards = document.querySelector('.location-cards') as HTMLElement | null
      const atlachNacha = document.querySelector('[data-label=atlachNacha]') as HTMLElement | null
      if (locationCards && atlachNacha) {
        const middleCardImg = atlachNacha.querySelector('img') as HTMLImageElement | null
        const computeOrigin = () => {
          if (!middleCardImg) return null
          const middleCardRect = atlachNacha.getBoundingClientRect()
          const middleCardImgRect = middleCardImg.getBoundingClientRect()
          const originX = middleCardImgRect.left + middleCardImgRect.width / 2 - middleCardRect.left
          const originY = middleCardImgRect.top + middleCardImgRect.height / 2 - middleCardRect.top
          const oX = middleCardImgRect.left + middleCardImgRect.width / 2
          const oY = middleCardImgRect.top + middleCardImgRect.height / 2
          return { originX, originY, oX, oY }
        }

        const applyLegTransform = (el: HTMLElement) => {
          const o = computeOrigin()
          if (!o) return
          // set container origin if needed (harmless if repeated)
          atlachNacha.style.transformOrigin = `${o.originX}px ${o.originY}px`
          const r = el.getBoundingClientRect()
          el.style.transformOrigin = `${o.oX - r.left}px ${o.oY - r.top}px`
          el.style.transition = 'transform 0.5s'
          el.style.transform = `rotate(${previousRotation.value}deg)`
        }

        // run once for any current legs missing transform (e.g., first mount)
        locationCards
          .querySelectorAll<HTMLElement>('[data-label=legs1],[data-label=legs2],[data-label=legs3],[data-label=legs4]')
          .forEach(applyLegTransform)

        legObserver = new MutationObserver(muts => {
          for (const m of muts) {
            if (m.type === 'childList' && (m.addedNodes?.length ?? 0) > 0) {
              m.addedNodes.forEach(n => {
                if (!(n instanceof HTMLElement)) return
                const maybeApply = (el: HTMLElement) => {
                  const label = el.dataset?.label
                  if (label && (label === 'legs1' || label === 'legs2' || label === 'legs3' || label === 'legs4')) {
                    applyLegTransform(el)
                  }
                }
                // node itself
                maybeApply(n)
                // or any legs inside subtree
                n.querySelectorAll?.('[data-label=legs1],[data-label=legs2],[data-label=legs3],[data-label=legs4]')
                  ?.forEach(el => maybeApply(el as HTMLElement))
              })
            }
          }
        })
        legObserver.observe(locationCards, { childList: true, subtree: true })
      }
    })
  }
});

onBeforeUnmount(() => {
  window.removeEventListener('storage', onCosmicEmissaryStorage)
  window.removeEventListener('arkham-setting-change', onCosmicEmissarySettingChange)
  window.removeEventListener('resize', updateRealityAcidLightRect)
  window.removeEventListener('scroll', updateRealityAcidLightRect, true)
  document.removeEventListener('click', proxyClippedLocationClick, true)
  legObserver?.disconnect()
  legObserver = null
  cosmicEmissaryObserver?.disconnect()
  cosmicEmissaryObserver = null
  cosmicEmissaryResizeObserver?.disconnect()
  cosmicEmissaryResizeObserver = null
  if (cosmicEmissaryCompactRequest !== null) cancelAnimationFrame(cosmicEmissaryCompactRequest)
  cosmicEmissaryCompactRequest = null
  cancelActiveDrag()
})

onUpdated(() => {
  updateRealityAcidLightRect()
  if(props.scenario.id === "c06333") {
    nextTick(() => rotateImages(needsInit.value))
  }
});

// Menu
addEntry({
  id: "viewChaosBag",
  icon: QuestionMarkCircleIcon,
  content: t('gameBar.viewChaosBag'),
  shortcut: "c",
  nested: 'view',
  action: () => showChaosBag.value = !showChaosBag.value
})

addEntry({
  id: "splitView",
  icon: ViewColumnsIcon,
  content: t('gameBar.splitView'),
  nested: 'view',
  action: toggleSplitView
})


addEntry({
  id: "viewRemovedFromPlay",
  icon: ArchiveBoxXMarkIcon,
  content: "View removed from Play",
  nested: 'view',
  action: () => showRemovedFromPlay()
})

addEntry({
  id: "rotateLayout",
  icon: ArrowPathIcon,
  content: t('gameBar.rotateLayout'),
  shortcut: ">",
  nested: 'view',
  action: () => {
    rotationSteps.value = (rotationSteps.value + 1) % 4
  }
})

addEntry({
  id: "rotateLayoutCounterClockwise",
  icon: ArrowPathIcon,
  content: t('gameBar.rotateLayout'),
  shortcut: "<",
  nested: 'hidden',
  action: () => {
    rotationSteps.value = (rotationSteps.value - 1) % 4
  }
})

// Computed
const pendingScenarioDifficulty = ref<string | null>(null)
const displayedScenarioDifficulty = computed(() => pendingScenarioDifficulty.value ?? props.scenario.difficulty)
watch(() => props.scenario.difficulty, (difficulty) => {
  if (pendingScenarioDifficulty.value === difficulty) pendingScenarioDifficulty.value = null
})

const scenarioGuide = computed(() => {
  const { reference } = props.scenario
  const difficulty = displayedScenarioDifficulty.value
  const referenceCode = reference.replace(/^c/, '')
  const referenceBase = referenceCode.replace(/b$/, '')

  if (props.scenario.id === 'c10501' || referenceBase === '10501' || referenceBase === '10502') {
    const referenceSide = referenceCode.endsWith('b') ? 'b' : ''
    const writtenInRockReference = difficulty === 'Hard' || difficulty === 'Expert' ? '10502' : '10501'
    return cardCodeImage(`${writtenInRockReference}${referenceSide}`)
  }

  const difficultySuffix = difficulty === 'Hard' || difficulty === 'Expert' ? 'b' : ''
  return cardCodeImage(reference, difficultySuffix)
})

const changeScenarioDifficulty = (event: Event) => {
  const difficulty = (event.target as HTMLSelectElement).value
  pendingScenarioDifficulty.value = difficulty
  debug.send(props.game.id, { tag: 'SetScenarioDifficulty', contents: difficulty })
}

const additionalReferences = computed(() => {
  return props.scenario.additionalReferences.map((s) => cardCodeImage(s))
})
const abyssIsLocation = computed(() =>
  props.scenario.id === 'c10651' && props.scenario.meta?.abyssIsLocation === true
)

const abyssDeckCount = computed(() =>
  props.scenario.decks?.find(([key]) => key === 'AbyssDeck')?.[1]?.length ?? 0
)

const scenarioDecks = computed(() => {
  if (!props.scenario.decks) return null
  return Object.entries(props.scenario.decks).filter(([, scenarioDeck]) =>
    !(abyssIsLocation.value && scenarioDeck[0] === 'AbyssDeck')
  )
})

const hideEncounterDeck = computed(() => props.scenario.id === 'c10651')

const scenarioDeckDiscard = (key: string) => {
  const discards = props.scenario.deckDiscards
  if (!discards) return undefined
  const entry = discards.find(([k]) => k === key)
  return entry ? entry[1] : undefined
}

const isVertical = function(area: string) {
  const [start, end] = area.split('--')
  const startLocation = locations.value.find((l) => l.id === start);
  const endLocation = locations.value.find((l) => l.id === end);

  if (!startLocation || !endLocation) return false

  return startLocation.label[startLocation.label.length - 1] !== endLocation.label[endLocation.label.length - 1]
}

const barriers = computed(() => props.scenario.meta?.barriers)

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;
  return source.sourceTag === 'OtherSource' && source.tag === 'ScenarioSource' 
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i }];
      }

      return acc;
    }, []);
})

interface ScenarioBadge {
  key: string
  icon: string
  label: string
  detail?: string
}

const scenarioBadges = computed<ScenarioBadge[]>(() => {
  if (props.scenario.id !== 'c85001') return []

  const badges: ScenarioBadge[] = []
  if (props.scenario.meta?.foodAndDrinksActive === true) {
    const damage = typeof props.scenario.meta.foodAndDrinksDamageDealt === 'number' ? props.scenario.meta.foodAndDrinksDamageDealt : 0
    badges.push({
      key: 'foodAndDrinks',
      icon: '🚫🍔',
      label: 'No food or drinks',
      detail: `${Math.min(damage, 3)}/3 damage dealt to Subject 8L-08`,
    })
  }

  if (props.scenario.meta?.languageActive === true) {
    badges.push({
      key: 'language',
      icon: '🗣️?',
      label: 'Gibberish only',
      detail: 'The concept of language is devoured until the end of the investigation phase.',
    })
  }

  if (props.scenario.meta?.friendshipsActive === true) {
    badges.push({
      key: 'friendships',
      icon: '🤝⃠',
      label: 'No cross-commits',
      detail: 'Friendships are devoured until the end of the round.',
    })
  }

  if (props.scenario.meta?.senseOfTimeActive === true) {
    badges.push({
      key: 'senseOfTime',
      icon: '🚫⏱️',
      label: 'No timekeeping',
      detail: 'Until the agenda advances, investigators cannot use time-keeping devices, ask about the time, or trigger abilities on cards with “time,” “watch,” or “chrono” in their title.',
    })
  }

  if (props.scenario.meta?.discardPileActive === true) {
    badges.push({
      key: 'discardPile',
      icon: '🗑️🫥',
      label: 'No discard piles',
      detail: 'Until the end of the next mythos phase, cards that would be placed in an investigator discard pile are devoured instead.',
    })
  }

  const voiceActive = props.scenario.meta?.voiceActive
  if (Array.isArray(voiceActive) && voiceActive.length > 0) {
    const names = voiceActive.map((iid) => props.game.investigators[iid]?.name?.title).filter(Boolean)
    badges.push({
      key: 'voice',
      icon: '🤐',
      label: names.length === 1 ? `${names[0]} cannot speak` : 'No speaking/noise',
      detail: names.length > 0 ? names.join(', ') : 'One or more investigators cannot speak or make noise until the end of the round.',
    })
  }

  return badges
})

const showScenarioNotifierBar = computed(() => scenarioBadges.value.length > 0 || props.realityAcidLightDevoured === true)

watch(
  () => [props.realityAcidLightDevoured, props.realityAcidLightActive, scenarioBadges.value.length],
  () => {
    nextTick(updateRealityAcidLightRect)
    setTimeout(updateRealityAcidLightRect, 50)
  },
  { immediate: true, flush: 'post' },
)

const rotationSteps = ref(0)
const transpose = <T>(grid: T[][]): T[][] =>
  (grid[0] ?? []).map((_col, i) => grid.map(row => row[i]))

const rotateClockwise = <T>(grid: T[][]): T[][] =>
  transpose([...grid].reverse())

const rotateCounterClockwise = <T>(grid: T[][]): T[][] =>
  [...transpose(grid)].reverse()

const rotateNTimes = <T>(grid: T[][], steps: number): T[][] => {
  const k = ((steps % 4) + 4) % 4
  switch (k) {
    case 0: return grid
    case 1: return rotateClockwise(grid)
    case 2: return rotateClockwise(rotateClockwise(grid))
    case 3: return rotateCounterClockwise(grid)
    default: return grid
  }
}
const gridAreas = computed(()=>{
  const { locationLayout } = props.scenario
  if (!locationLayout) return null

  // fast path when no barriers meta
  if (!barriers.value) {
    const normalizeRow = (row: string): string[] => row.trim().split(/\s+/)
    const baseRows: string[][] = locationLayout.map(normalizeRow)
    const rotatedRows = rotateNTimes(baseRows, rotationSteps.value)
    return rotatedRows.map(r => `"${r.join(' ')}"`).join(' ')
  }

  const grid: any = {}
  for (const l of locations.value) grid[l.label] = l.id

  const cleanedRows = locationLayout.map(r => r.split(' '))
  const withHoriz: string[][] = []
  for (const row of cleanedRows) {
    const newRow: string[] = []
    for (let c = 0; c < row.length; c++){
      const a = row[c]
      newRow.push(a)
      if (c < row.length - 1){
        const b = row[c + 1]
        const idA = grid[a], idB = grid[b]
        newRow.push(idA && idB ? `barrier-${[idA,idB].sort().join('--')}` : '.')
      }
    }
    withHoriz.push(newRow)
  }
  const finalRows: string[][] = []
  for (let r = 0; r < withHoriz.length; r++){
    const row = withHoriz[r]
    finalRows.push(row)
    if (r < withHoriz.length - 1){
      const next = withHoriz[r + 1]
      let need = false
      const barrierRow = row.map((_cell, idx) => {
        const a = row[idx], b = next[idx]
        const idA = grid[a], idB = grid[b]
        const v = idA && idB ? `barrier-${[idA,idB].sort().join('--')}` : '.'
        if (v !== '.') need = true
        return v
      })
      if (need) finalRows.push(barrierRow)
    }
  }
  const rotatedRows = rotateNTimes(finalRows, rotationSteps.value)
  return rotatedRows.map(r => `"${r.join(' ')}"`).join(' ')
})

// transform: scale() is used rather than CSS zoom because it is handled consistently
// by getBoundingClientRect() in all browsers. The scroll area doesn't follow transform
// automatically, so we set margins after each render to compensate.
const locationStyles = computed(() => {
  const pad = layoutPadding.value
  const mobileEdgePadding = isMobile.value ? 140 : 0
  return {
    display: 'grid',
    gap: '20px',
    'grid-template-areas': gridAreas.value ?? '',
    gridAutoColumns: 'max-content',
    gridAutoRows: 'max-content',
    transform: `scale(${locationsZoom.value})`,
    transformOrigin: locationsZoom.value >= 1 ? '0 0' : 'center center',
    paddingLeft: `${pad.left + mobileEdgePadding}px`,
    paddingRight: `${pad.right + mobileEdgePadding}px`,
    paddingTop: `${pad.top}px`,
    paddingBottom: `${pad.bottom}px`,
  }
})

async function updateScrollMargins() {
  await nextTick()
  const grid = (locationMap.value as any)?.$el ?? locationMap.value as HTMLElement | null
  if (!grid) return
  const z = locationsZoom.value
  // offsetWidth/Height exclude margins, so we always read the natural grid size directly.
  if (z >= 1) {
    grid.style.marginRight  = `${grid.offsetWidth  * (z - 1)}px`
    grid.style.marginBottom = `${grid.offsetHeight * (z - 1)}px`
  } else {
    grid.style.marginRight  = ''
    grid.style.marginBottom = ''
  }
}

const scenarioDeckStyles = computed(() => {
  const { decksLayout } = props.scenario
  return {
    display: 'grid',
    'grid-template-areas': decksLayout.map((row) => `"${row}"`).join(' '),
    'grid-row-gap': '10px',
  }
})
const players = computed(() => props.game.investigators)
const playerOrder = computed(() => props.game.playerOrder)
const discards = computed<Card[]>(() => props.scenario.discard.map(c => ({ tag: 'EncounterCard', contents: c })))
const playerLocationZones = computed(() => props.game.playerOrder.flatMap((investigatorId) => {
  const investigator = props.game.investigators[investigatorId]
  if (!investigator) return []

  const playerLocations = Object.values(props.game.locations).filter((location) =>
    location.placement?.tag === 'InPlayArea' && location.placement.contents === investigator.id
  )

  if (playerLocations.length === 0) return []
  return [{
    investigatorId,
    name: fullName(investigator.name),
    locations: playerLocations,
  }]
}))

const enemyGroups = computed(()=>{
  const all = Object.values(props.game.enemies)
  const outOfPlay: Enemy[] = []
  const pursuit: Enemy[] = []
  const global: Enemy[] = []
  const asLoc: Enemy[] = []
  let firstVoid: Enemy|undefined

  for (const e of all) {
    const p = e.placement
    if (p.tag === 'OutOfPlay') {
      outOfPlay.push(e)
      if (!firstVoid && (p.contents === 'VoidZone' || p.contents === 'TheDepths')) firstVoid = e
      if (p.contents === 'PursuitZone') pursuit.push(e)
    }
    if (p.tag === 'OtherPlacement' && p.contents === 'Global' && e.asSelfLocation === null) global.push(e)
    if (e.asSelfLocation !== null) asLoc.push(e)
  }
  return { outOfPlay, pursuit, global, asLoc, firstVoid }
})

const outOfPlayEnemies = computed(() => enemyGroups.value.outOfPlay)
const pursuit = computed(() => enemyGroups.value.pursuit)
const globalEnemies = computed(() => enemyGroups.value.global)
const inTheShadows = computed(() => Object.values(props.game.enemies).filter((e) => e.placement.tag === "InTheShadows"))
type InTheShadowLocations = { left?: string; middle?: string; right?: string }
const inTheShadowLocations = computed<InTheShadowLocations>(() => {
  const locations = props.scenario.meta?.locationsInShadows
  if (!locations || typeof locations !== 'object') return {}
  return locations as InTheShadowLocations
})

const anyInTheShadowLocations = computed(() => {
  const locations = inTheShadowLocations.value
  return locations.left || locations.right || locations.middle
})
const inTheShadowsInvestigators = computed(() => Object.values(props.game.investigators).filter((e) => e.placement.tag === "InTheShadows"))
const enemiesAsLocations = computed(() => enemyGroups.value.asLoc)
const topEnemyInVoid = computed(() => enemyGroups.value.firstVoid)

type ConcealedGroup = {
  position: Position;
  known: ConcealedCard[];
  unknown: ConcealedCard[];
};

const positionToGridArea = function(pos: Position): string {
  // convert to posxxyy, negative number gets prefixed with an n so pos (-1, 0) is posn0100, (0, -1) is pos00n01, and (-1, -1) would be posn01n01
  const fmt = (n: number): string => {
    const sign = n < 0 ? 'n' : '';
    const abs = Math.abs(n);
    const padded = abs.toString().padStart(2, '0'); // 0 -> "00", 5 -> "05", 12 -> "12"
    return sign + padded;
  };

  return `pos${fmt(pos.x)}${fmt(pos.y)}`;  
}


const gridConcealed = computed<ConcealedGroup[]>(() => {
  const concealedCards = props.scenario.meta?.concealedCards as [[number, number], string[]][] | undefined
  if (!concealedCards) return []

  const available = Object.values(props.game.concealed)
  const availableIds = new Set(available.map(c => c.id))

  const cardPosition: Record<string, Position> = {}
  for (const [[x, y], cards] of concealedCards) {
    const position = { x, y }
    for (const cardId of cards) {
      if (availableIds.has(cardId)) cardPosition[cardId] = position
    }
  }

  const groups = new Map<string, ConcealedGroup>()
  for (const card of available) {
    const position = cardPosition[card.id]
    if (!position) continue
    const key = `${position.x}:${position.y}`
    const group = groups.get(key) ?? groups.set(key, { position, known: [], unknown: [] }).get(key)!
    ;(card.known ? group.known : group.unknown).push(card)
  }

  return [...groups.values()]
})

function isHollow(m: ModifierType): m is Hollow {
  return m.tag === "Hollow"
}
const hollowed = computed(() => [...new Set(Object.values(props.game.investigators).flatMap(i => (i.modifiers || []).map((m) => m.type).filter(isHollow).map(m => props.game.cards[m.contents])))])

const outOfPlay = computed(() => props.scenario?.setAsideCards || [])
const removedFromPlay = computed(() => props.game.removedFromPlay)
const noCards = computed<Card[]>(() => [])
const topOfEncounterDiscard = computed(() => {
  if (!props.scenario.discard[0]) return null
  return cardCodeImage(props.scenario.discard[0].cardCode)
})
const spectralEncounterDeck = computed(() => props.scenario.encounterDecks['SpectralEncounterDeck']?.[0])
const spectralDiscard = computed(() => props.scenario.encounterDecks['SpectralEncounterDeck']?.[1])
const spectralDiscards = computed<Card[]>(() => (spectralDiscard.value ?? []).map(c => ({ tag: 'EncounterCard', contents: c })))
const topOfSpectralDiscard = computed(() => {
  if (!spectralDiscard.value || !spectralDiscard.value[0]) return null
  return cardCodeImage(spectralDiscard.value[0].cardCode)
})
const activePlayerId = computed(() => props.game.activeInvestigatorId)
const globalStories = computed(() => Object.values(props.game.stories).filter((story) =>
  story.placement.tag === "OtherPlacement" && story.placement.contents === "Global"
))
const globalAssets = computed(() => Object.values(props.game.assets).filter((asset) => 
  asset.placement.tag === "OtherPlacement" && asset.placement.contents === "Global"
))
const cardsUnderScenarioReference = computed(() => props.scenario.cardsUnderScenarioReference)
const cardsUnderAgenda = computed(() => props.scenario.cardsUnderAgendaDeck)
const cardsUnderAct = computed(() => props.scenario.cardsUnderActDeck)
const cardsNextToAct = computed(() => props.scenario.cardsNextToActDeck)
const cardsNextToAgenda = computed(() => props.scenario.cardsNextToAgendaDeck)
const nextToTreacheries = computed<string[]>(() => Object.values(props.game.treacheries).
  filter((t) => t.placement.tag === "NextToAgenda").
  map((t) => t.id))
const agendaGroupedTreacheries = computed(() => Object.entries(groupBy(nextToTreacheries.value, (t) => props.game.treacheries[t].cardCode)))

const keys = computed(() => props.scenario.setAsideKeys)
const spentKeys = computed(() => props.scenario.keys)
// TODO: not showing cosmos should be more specific, as there could be a cosmos location in the future?
const locations = computed(() => Object.values(props.game.locations).
  filter((a) => a.placement === null && a.label !== "cosmos"))
watch(locations, updateScrollMargins, { flush: 'post' })
watch(layoutPadding, updateScrollMargins, { flush: 'post' })
watch([locations, rotationSteps, locationsZoom], updateCellDimensions, { flush: 'post' })
const cosmicEmissaryLayoutSignature = computed(() => {
  if (props.scenario.id !== 'c10651') return ''
  return [
    locations.value.map((l) => `${l.id}:${l.label}`).join('|'),
    enemiesAsLocations.value.map((e) => `${e.id}:${e.asSelfLocation}`).join('|'),
  ].join('::')
})
watch([cosmicEmissaryLayoutSignature, rotationSteps, locationsZoom], () => nextTick(requestCosmicEmissaryCompact), { flush: 'post' })
watch(
  [locationOffsets, pendingOffsets, rotationSteps, locationsZoom, locations, cellDimensions],
  updateLayoutPadding,
  { flush: 'post', deep: true },
)
const usedLabels = computed(() => locations.value.map((l) => l.label))
const unusedLabels = computed(() => {
  const { locationLayout, usesGrid } = props.scenario;
  if (!locationLayout || !usesGrid) return []
  return locationLayout.flatMap((row) => row.split(' ')).filter((x) => !usedLabels.value.includes(x) && x !== '.')
})
const choices = useGameChoices(() => props.game, () => props.playerId)

type LocationLike = { id: string, label: string }

const isLocationChoice = (c: Message, location: LocationLike): boolean => {
  if (c.tag === "TargetLabel") return c.target.contents === location.id
  if (c.tag === "GridLabel") return c.gridLabel === location.label

  if (c.tag !== "AbilityLabel") return false
  const { source } = c.ability

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) return source.source.contents === location.id
  } else if (source.tag === 'LocationSource') {
    return source.contents === location.id
  }

  return false
}

const locationCanInteract = (location: LocationLike): boolean =>
  !locationsUnlocked.value && choices.value.some((choice) => isLocationChoice(choice, location))

const isEncounterDiscardChoice = (c: Message) => {
  if (c.tag !== "TargetLabel") return false
  if (c.target.tag === "EnemyTarget") {
    const enemy = props.game.enemies[c.target.contents as string]
    if (!enemy) return false
    return discards.value.some(card => cardId(card) === enemy.cardId)
  }
  if (c.target.tag !== "CardIdTarget") return false
  return discards.value.some(card => cardId(card) === c.target.contents)
}
const encounterDiscardCardsAction = computed(() => choices.value.some(isEncounterDiscardChoice))

const resources = computed(() => props.scenario.tokens[TokenType.Resource])
const damage = computed(() => props.scenario.tokens[TokenType.Damage])
const targets = computed(() => props.scenario.tokens[TokenType.Target])
const hasPool = computed(() => resources.value && resources.value > 0 || damage.value && damage.value > 0)
const tarotCards = computed(() => props.scenario.tarotCards.filter((c) => c.scope.tag === 'GlobalTarot'))
const phase = computed(() => props.game.phase)
const phaseStep = computed(() => props.game.phaseStep)
const currentDepth = computed(() => props.scenario.counts["CurrentDepth"])
const civiliansSlain = computed(() => props.scenario.counts["CiviliansSlain"])
const scraps = computed(() => props.scenario.tokens[TokenType.Scrap])
const switches = computed(() => props.scenario.tokens[TokenType.Switch])
const darknessLevel = computed(() => props.scenario.tokens[TokenType.DarknessLevel])
const signOfTheGods = computed(() => props.scenario.counts["SignOfTheGods"])
const strengthOfTheAbyss = computed(() => props.scenario.counts["StrengthOfTheAbyss"])
const distortion = computed(() => props.scenario.counts["Distortion"])
// Laid to Rest: horror placed on the scenario reference card represents
// Spiritual Disturbance (defeats everyone at 4). Render it on the scenario card.
const spiritualDisturbance = computed(() =>
  props.scenario.id === 'c90054' ? props.scenario.tokens[TokenType.Horror] : undefined)
const gameOver = computed(() => props.game.gameState.tag === "IsOver")

// Reactive
const showCards = reactive<RefWrapper<any>>({ ref: noCards })
const doShowCards = (cards: ComputedRef<Card[]>, title: string, isDiscards: boolean, revealed = false) => {
  cardRowTitle.value = title
  showCards.ref = cards
  viewingDiscard.value = isDiscards
  revealingCards.value = revealed
}
const showRemovedFromPlay = () => doShowCards(removedFromPlay, t('scenario.removedFromPlay'), true)
const showDiscards = () => doShowCards(discards, t('scenario.discards'), true)
const hideCards = () => {
  showCards.ref = noCards
  revealingCards.value = false
}

// Watchers
watchEffect(() => {
  const oop = outOfPlay.value.length + outOfPlayEnemies.value.length

  if (oop == 0) {
    removeEntry("showOutOfPlay")
    showOutOfPlay.value = false
  } else {
    addEntry({
      id: "showOutOfPlay",
      icon: EyeIcon,
      content: t('gameBar.showOutOfPlay'),
      nested: 'view',
      shortcut: 'o',
      action: () => showOutOfPlay.value = !showOutOfPlay.value
    })
  }

  const isOutOfPlaySource = (source: Source) => {
    switch (source.tag) {
      case "EnemySource": {
       return outOfPlayEnemies.value.some((e) => e.id == source.contents)
      }
      case "TreacherySource": {
       return outOfPlayEnemies.value.some((e) => {
          if (source.contents) return e.treacheries.includes(source.contents)
          return false
       })
      }
      case "EventSource": {
       return outOfPlayEnemies.value.some((e) => {
          if (source.contents) return e.events.includes(source.contents)
          return false
       })
      }
      default: return false
    }
  }
  const isOutOfPlayCardId = (id: string) => outOfPlay.value.some(card => cardId(card) === id)

  const isOutOfPlayTarget = (target: Target) => {
    const contents = target.contents
    if (typeof contents !== 'string') return false

    switch (target.tag) {
      case "CardIdTarget": return isOutOfPlayCardId(contents)
      case "EnemyTarget": return outOfPlayEnemies.value.some((e) => e.id === contents)
      case "TreacheryTarget": {
        return outOfPlayEnemies.value.some((e) => e.treacheries.includes(contents))
      }
      case "EventTarget": {
        return outOfPlayEnemies.value.some((e) => e.events.includes(contents))
      }
      default: return false
    }
  }

  const isOutOfPlayChoice = (c: Message) => {
    if (c.tag === "AbilityLabel") return isOutOfPlaySource(c.ability.source)
    if (c.tag === "TargetLabel") return isOutOfPlayTarget(c.target)
    return false
  }

  const isActionableChoice = (c: Message) => !["Info", "InvalidLabel", "TooltipLabel"].includes(c.tag)
  const actionableChoices = choices.value.filter(isActionableChoice)
  forcedShowOutOfPlay.value = actionableChoices.length > 0 && actionableChoices.every(isOutOfPlayChoice)

  const isHollowedChoice = (c: Message) => {
    if (c.tag !== "TargetLabel") return false
    if (c.target.tag !== "CardIdTarget") return false
    return hollowed.value.some(card => cardId(card) === c.target.contents)
  }
  const showDiscard = choices.value.some(isEncounterDiscardChoice)
  const showHollowedCards = choices.value.some(isHollowedChoice)
  if (showDiscard) {
    encounterDiscardPopoverShown.value = true
    hideCards()
    forcedShowDiscard.value = true
    hollowedPopoverShown.value = false
  } else if (showHollowedCards) {
    hollowedPopoverShown.value = true
    forcedShowDiscard.value = false
  } else {
    hideCards()
    forcedShowDiscard.value = false
    hollowedPopoverShown.value = false
  }
})


// Helpers
const cosmicEmissaryLabels = [
  'cosmicEmissaryPhantasm',
  'cosmicEmissaryAbyss',
  'cosmicEmissaryBrilliance',
  'cosmicEmissaryMiasma',
] as const

type CosmicEmissaryLabel = typeof cosmicEmissaryLabels[number]

const cosmicEmissaryLocationLabels: Record<CosmicEmissaryLabel, string> = {
  cosmicEmissaryPhantasm: 'mirrorNestLeft',
  cosmicEmissaryAbyss: 'mirrorNestTop',
  cosmicEmissaryBrilliance: 'mirrorNestBottom',
  cosmicEmissaryMiasma: 'mirrorNestRight',
}

function locationHasManualOffset(el: HTMLElement): boolean {
  const locationId = el.dataset.locationId
  if (!locationId) return false
  return locationId in locationOffsets.value
    || locationId in pendingOffsets.value
    || dragInternal?.locationId === locationId
}

function transformTranslate(el: HTMLElement): { x: number, y: number } {
  const style = getComputedStyle(el)
  const translate = style.translate
  if (translate && translate !== 'none') {
    const [x = '0px', y = '0px'] = translate.split(/\s+/)
    return { x: parseFloat(x) || 0, y: parseFloat(y) || 0 }
  }

  const transform = style.transform
  if (!transform || transform === 'none') return { x: 0, y: 0 }
  const matrix = new DOMMatrixReadOnly(transform)
  return { x: matrix.e, y: matrix.f }
}

function styleMapsEqual(a: Record<string, Record<string, string>>, b: Record<string, Record<string, string>>): boolean {
  const aKeys = Object.keys(a)
  const bKeys = Object.keys(b)
  if (aKeys.length !== bKeys.length) return false
  return aKeys.every((key) => {
    const aStyle = a[key]
    const bStyle = b[key]
    if (!bStyle) return false
    const aStyleKeys = Object.keys(aStyle)
    const bStyleKeys = Object.keys(bStyle)
    return aStyleKeys.length === bStyleKeys.length
      && aStyleKeys.every((styleKey) => aStyle[styleKey] === bStyle[styleKey])
  })
}

function compactCosmicEmissaryFormation(force = false) {
  if (props.scenario.id !== 'c10651' || (locationsUnlocked.value && !force)) return

  const entries = cosmicEmissaryLabels.map((label) => {
    const el = document.querySelector(`[data-label=${label}]`) as HTMLElement | null
    return el ? [label, el] as const : null
  })

  if (entries.some((entry) => entry === null)) {
    if (Object.keys(cosmicEmissaryEnemyStyles.value).length > 0 || Object.keys(cosmicEmissaryLocationCellStyles.value).length > 0) {
      clearCosmicEmissaryCompactStyles()
    }
    return
  }

  const elements = Object.fromEntries(entries as [CosmicEmissaryLabel, HTMLElement][]) as Record<CosmicEmissaryLabel, HTMLElement>

  const locationElements = Object.fromEntries(
    cosmicEmissaryLabels.map((label) => [
      label,
      document.querySelector(`.location-cell[data-label=${cosmicEmissaryLocationLabels[label]}]`) as HTMLElement | null,
    ])
  ) as Record<CosmicEmissaryLabel, HTMLElement | null>

  requestAnimationFrame(() => {
    const locationCards = document.querySelector('.location-cards') as HTMLElement | null
    const visualScale = locationCards
      ? (new DOMMatrixReadOnly(getComputedStyle(locationCards).transform).a || 1)
      : 1
    const rectFor = (el: HTMLElement) => {
      const rectEl = (el.querySelector('img.card') ?? el) as HTMLElement
      const rect = rectEl.getBoundingClientRect()
      const existing = transformTranslate(el)
      // Measure the element's natural grid position without removing its current
      // transform. This keeps the compacted positions in the VDOM between game
      // updates, so cards don't snap outward before this rAF runs again.
      return {
        left: rect.left - existing.x * visualScale,
        top: rect.top - existing.y * visualScale,
        width: rect.width,
        height: rect.height,
      }
    }
    const rects = Object.fromEntries(
      Object.entries(elements).map(([label, el]) => [label, rectFor(el as HTMLElement)])
    ) as Record<CosmicEmissaryLabel, { left: number, top: number, width: number, height: number }>

    const centerX = cosmicEmissaryLabels.reduce((acc, label) => acc + rects[label].left + rects[label].width / 2, 0) / cosmicEmissaryLabels.length
    const centerY = cosmicEmissaryLabels.reduce((acc, label) => acc + rects[label].top + rects[label].height / 2, 0) / cosmicEmissaryLabels.length

    const targets: Record<CosmicEmissaryLabel, { left: number; top: number }> = {
      cosmicEmissaryPhantasm: {
        left: centerX - rects.cosmicEmissaryPhantasm.width,
        top: centerY - rects.cosmicEmissaryPhantasm.height,
      },
      cosmicEmissaryAbyss: {
        left: centerX,
        top: centerY - rects.cosmicEmissaryAbyss.height,
      },
      cosmicEmissaryBrilliance: {
        left: centerX - rects.cosmicEmissaryBrilliance.width,
        top: centerY,
      },
      cosmicEmissaryMiasma: {
        left: centerX,
        top: centerY,
      },
    }

    const nextEnemyStyles: Record<string, Record<string, string>> = {}
    const nextLocationCellStyles: Record<string, Record<string, string>> = {}
    const transition = (!enableCosmicEmissaryAnimation.value || cosmicEmissaryFormationHasMeasured.value) ? 'none' : 'transform 0.2s ease'

    for (const label of cosmicEmissaryLabels) {
      const rect = rects[label]
      const target = targets[label]
      const dx = Math.round(((target.left - rect.left) / visualScale) * 10) / 10
      const dy = Math.round(((target.top - rect.top) / visualScale) * 10) / 10
      nextEnemyStyles[label] = {
        translate: `${dx}px ${dy}px`,
        transition,
        zIndex: 'var(--z-index-20)',
      }

      const locationEl = locationElements[label]
      const locationLabel = cosmicEmissaryLocationLabels[label]
      if (locationEl && locationHasManualOffset(locationEl)) {
        const existing = cosmicEmissaryLocationCellStyles.value[locationLabel]
        if (existing) nextLocationCellStyles[locationLabel] = existing
      } else if (locationEl) {
        const shouldAlignVerticalMidpoint = label === 'cosmicEmissaryPhantasm' || label === 'cosmicEmissaryMiasma'
        const locationDy = shouldAlignVerticalMidpoint
          ? (() => {
              const locationRect = rectFor(locationEl)
              const enemyCenterY = rect.top + rect.height / 2
              const locationCenterY = locationRect.top + locationRect.height / 2
              return Math.round((dy + (enemyCenterY - locationCenterY) / visualScale) * 10) / 10
            })()
          : dy

        nextLocationCellStyles[locationLabel] = {
          translate: `${dx}px ${locationDy}px`,
          transition,
          zIndex: 'var(--z-index-10)',
        }
      }
    }

    const enemyStylesChanged = !styleMapsEqual(cosmicEmissaryEnemyStyles.value, nextEnemyStyles)
    const locationCellStylesChanged = !styleMapsEqual(cosmicEmissaryLocationCellStyles.value, nextLocationCellStyles)
    if (enemyStylesChanged) {
      cosmicEmissaryEnemyStyles.value = nextEnemyStyles
      writeStyleMapCache(cosmicEmissaryEnemyStylesCacheKey, nextEnemyStyles)
    }
    if (locationCellStylesChanged) {
      cosmicEmissaryLocationCellStyles.value = nextLocationCellStyles
      writeStyleMapCache(cosmicEmissaryLocationCellStylesCacheKey, nextLocationCellStyles)
    }
    if (enemyStylesChanged || locationCellStylesChanged) {
      nextTick(() => window.dispatchEvent(new Event('arkham-location-layout-change')))
    }
    cosmicEmissaryFormationHasMeasured.value = true
  })
}

function rotateImages(init: boolean) {
  const atlachNacha = document.querySelector('[data-label=atlachNacha]') as HTMLElement
  const locationCards = document.querySelector('.location-cards')
  if (atlachNacha && locationCards) {
    needsInit.value = false
    const inLocation = locationCards.querySelector('[data-label=atlachNacha]')

    if (inLocation) {
      ["legs1", "legs2", "legs3", "legs4"].forEach((legs) =>  {
        const legsDiv = locationCards.querySelector(`[data-label=${legs}]`)
        if (!legsDiv) {
          legsSet.value = legsSet.value.filter(item => item !== legs);

          const newDiv = document.createElement('div');
          newDiv.setAttribute('data-label', legs); // Setting the data-label attribute
          newDiv.style.width = '60px'; // Setting the width of the div
          newDiv.style.height = '84px'; // Setting the width of the div
          newDiv.style.gridArea = legs; // Assuming 'legs1' is a valid grid-area name

          locationCards.appendChild(newDiv); // Append the new div to the parent container
        }
      })
    }

    // const degrees = parseFloat(atlachNacha.dataset?.rotation || "0") || 0
    // Guard: don't clobber with transient 0/NaN
    const raw = atlachNacha.dataset?.rotation
    const parsed = Number(raw)
    const hasValid = raw != null && !Number.isNaN(parsed)
    const degrees = hasValid ? parsed : previousRotation.value
    const middleCardImg = atlachNacha.querySelector('img')
    if (!middleCardImg) return
    const middleCardRect = atlachNacha.getBoundingClientRect()
    const middleCardImgRect = middleCardImg.getBoundingClientRect()
    const originX = middleCardImgRect.left + middleCardImgRect.width / 2 - middleCardRect.left
    const originY = middleCardImgRect.top + middleCardImgRect.height / 2 - middleCardRect.top

    if (init) atlachNacha.style.transformOrigin = `${originX}px ${originY}px`
    atlachNacha.style.transition = 'none'
    atlachNacha.style.transform = `rotate(${previousRotation.value}deg)`
    const oX = middleCardImgRect.left + middleCardImgRect.width / 2
    const oY = middleCardImgRect.top + middleCardImgRect.height / 2

    document.querySelectorAll('[data-label=legs1],[data-label=legs2],[data-label=legs3],[data-label=legs4]').forEach((el) => {
      const img = el as HTMLElement
      const label = img.dataset.label
      if (!label) return

      if (init || !legsSet.value.includes(label)) {
        if(!legsSet.value.includes(label)) {
          legsSet.value = [...legsSet.value, label]
        }
        const thisRect = img.getBoundingClientRect()
        const thisX = thisRect.left
        const thisY = thisRect.top
        img.style.transformOrigin = `${oX - thisX}px ${oY - thisY}px`
      }
      img.style.transition = 'none'
      img.style.transform = `rotate(${previousRotation.value}deg)`
    });
    if (hasValid && degrees !== previousRotation.value) {
      previousRotation.value = degrees
      requestAnimationFrame(() => {
        requestAnimationFrame(() => {
          atlachNacha.style.transform = `rotate(${previousRotation.value}deg)`
          atlachNacha.style.transition = 'transform 0.5s'
          document.querySelectorAll('[data-label=legs1],[data-label=legs2],[data-label=legs3],[data-label=legs4]').forEach((el) => {
            const img = el as HTMLElement
            img.style.transition = 'transform 0.5s'
            img.style.transform = `rotate(${degrees}deg)`
          })
        })
      })
    }
  }
}

function beforeLeave(e: Element) {
  const el = e as HTMLElement
  const {marginLeft, marginTop, width, height} = window.getComputedStyle(el)

  el.style.left = `${el.offsetLeft - parseFloat(marginLeft)}px`
  el.style.top = `${el.offsetTop - parseFloat(marginTop)}px`
  el.style.width = width
  el.style.height = height
}

async function toggleZoom(e: MouseEvent) {
  const scroller = scrollerRef.value
  const gridEl = (locationMap.value as any)?.$el ?? locationMap.value as HTMLElement | null
  if (!scroller || !gridEl) return

  if (doubleZoomActive.value) {
    doubleZoomActive.value = false
    locationsZoom.value = doubleZoomPrevValue.value
    await updateScrollMargins()
    scroller.scrollLeft = doubleZoomPrevScroll.left
    scroller.scrollTop = doubleZoomPrevScroll.top
    return
  }

  // Find what to focus on: the clicked location, or the investigator's location
  const target = e.target as HTMLElement
  let focusEl: HTMLElement | null = target.closest('[data-id]')
  if (!focusEl) {
    const investigator = Object.values(props.game.investigators).find(i => i.playerId === props.playerId)
    if (investigator?.location) {
      focusEl = document.querySelector<HTMLElement>(`[data-id="${investigator.location}"]`)
    }
  }
  if (!focusEl) return

  const currentZ = locationsZoom.value
  const scrollerRect = scroller.getBoundingClientRect()
  const gridRect = gridEl.getBoundingClientRect()
  const focusRect = focusEl.getBoundingClientRect()

  // Compute the grid's layout position in scroller content space. This is invariant across zoom
  // levels since flex sizes items by their natural dimensions. We must account for transform-origin:
  //   z >= 1  → origin 0 0: visual top-left === layout top-left, so read directly.
  //   z <  1  → origin center: visual top-left is shifted inward; subtract the shift to get layout.
  const gridW = gridEl.offsetWidth
  const gridH = gridEl.offsetHeight
  const gridLayoutLeft = currentZ >= 1
    ? gridRect.left - scrollerRect.left + scroller.scrollLeft
    : (gridRect.left - scrollerRect.left + scroller.scrollLeft) - gridW * (1 - currentZ) / 2
  const gridLayoutTop = currentZ >= 1
    ? gridRect.top - scrollerRect.top + scroller.scrollTop
    : (gridRect.top - scrollerRect.top + scroller.scrollTop) - gridH * (1 - currentZ) / 2

  // Natural (unscaled) center of focus within the grid.
  // Visual delta from the grid's visual top-left = natural offset × scale, for any transform-origin.
  const natX = (focusRect.left + focusRect.width / 2 - gridRect.left) / currentZ
  const natY = (focusRect.top + focusRect.height / 2 - gridRect.top) / currentZ

  // Save current state
  doubleZoomPrevValue.value = currentZ
  doubleZoomPrevScroll.left = scroller.scrollLeft
  doubleZoomPrevScroll.top = scroller.scrollTop

  // Apply new zoom and wait for margins to update
  doubleZoomActive.value = true
  locationsZoom.value = DOUBLE_ZOOM_LEVEL
  await updateScrollMargins()

  // At new zoom (origin 0 0), focus sits at gridLayoutLeft + natX * newZ in content space.
  // gridLayoutLeft is invariant (flex uses natural dimensions), so it's the same before and after.
  // Use clientWidth/Height (viewport area, excludes scrollbars) for accurate centering.
  scroller.scrollLeft = gridLayoutLeft + natX * DOUBLE_ZOOM_LEVEL - scroller.clientWidth / 2
  scroller.scrollTop = gridLayoutTop + natY * DOUBLE_ZOOM_LEVEL - scroller.clientHeight / 2
}

const unusedCanInteract = (u: string) => choices.value.findIndex((c) =>
  c.tag === "GridLabel" && c.gridLabel === u
)
const tarotCardAbility = (card: TarotCard) => {
  return choices.value.findIndex((c) => {
    if (c.tag === "AbilityLabel") {
      return c.ability.source.sourceTag === "TarotSource" && c.ability.source.contents.arcana === card.arcana
    }

    return false
  })
}

const victoryDisplay = computed(() => props.scenario.victoryDisplay)

const isMinimized_SkillTest = ref(false)
provide('isMinimized_SkillTest', isMinimized_SkillTest)
function minimize_SkillTest(isMinimized:boolean){
  if (isMobile) {
    isMinimized_SkillTest.value = isMinimized
  }
}

const blessTokens = computed(() => props.scenario.chaosBag.chaosTokens.filter((t) => t.face === 'BlessToken'
).length)
const curseTokens = computed(() => props.scenario.chaosBag.chaosTokens.filter((t) => t.face === 'CurseToken').length)
const frostTokens = computed(() => props.scenario.chaosBag.chaosTokens.filter((t) => t.face === 'FrostToken').length)

async function removeChaosToken(face: any){
  debug.send(props.game.id, {tag: 'ChaosBagMessage', contents: {tag: 'RemoveChaosToken_', contents: face}})
}

async function addChaosToken(face: any){
  debug.send(props.game.id, {tag: 'AddChaosToken', contents: face})
}
</script>

<template>
  <div v-if="upgradeDeck" id="game" class="game">
    <UpgradeDeck :game="game" :key="playerId" :playerId="playerId" @choose="choose"/>
  </div>
  <div v-else-if="!gameOver" id="scenario" class="scenario" :data-scenario="scenario.id">
    <div class="scenario-body" :class="{'split-view': splitView, 'scenario-body--notifier-overlays': showScenarioNotifierBar }">
      <Draggable v-if="showOutOfPlay || forcedShowOutOfPlay">
        <template #handle><header><h2>{{ $t('gameBar.outOfPlay') }}</h2></header></template>
        <div class="card-row-cards">
          <div v-for="card in outOfPlay" :key="cardId(card)" class="card-row-card">
            <CardView :game="game" :card="card" :playerId="playerId" @choose="$emit('choose', $event)" />
          </div>
          <EnemyView
            v-for="enemy in outOfPlayEnemies"
            :key="enemy.id"
            :enemy="enemy"
            :game="game"
            :playerId="playerId"
            @choose="choose"
          />
        </div>
        <button v-if="!forcedShowOutOfPlay" class="close button" @click="showOutOfPlay = false">{{$t('close')}}</button>
      </Draggable>
      <Draggable v-if="showChaosBag">
        <template #handle><header><h2>{{$t('gameBar.chaosBag')}}</h2></header></template>
        <ChaosBag :game="game" :skillTest="null" :chaosBag="scenario.chaosBag" :playerId="playerId" @choose="choose" />
        <div v-if="debug.active" class="buttons buttons-row">
          <div class="tri-button blessed">
            <button class="button blessed" @click="removeChaosToken('BlessToken')">-</button>
            <span class="bless-icon"></span>
            <button class="button blessed" @click="addChaosToken('BlessToken')">+</button>
          </div>
          <div class="tri-button cursed">
            <button class="button cursed" @click="removeChaosToken('CurseToken')">-</button>
            <span class="curse-icon"></span>
            <button class="button cursed" @click="addChaosToken('CurseToken')">+</button>
          </div>
          <div class="tri-button frost">
            <button class="button frost" @click="removeChaosToken('FrostToken')">-</button>
            <span class="frost-icon"></span>
            <button class="button frost" @click="addChaosToken('FrostToken')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('PlusOne')">-</button>
            <span>+1</span>
            <button class="button" @click="addChaosToken('PlusOne')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('Zero')">-</button>
            <span>0</span>
            <button class="button" @click="addChaosToken('Zero')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('MinusOne')">-</button>
            <span>-1</span>
            <button class="button" @click="addChaosToken('MinusOne')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('MinusTwo')">-</button>
            <span>-2</span>
            <button class="button" @click="addChaosToken('MinusTwo')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('MinusThree')">-</button>
            <span>-3</span>
            <button class="button" @click="addChaosToken('MinusThree')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('MinusFour')">-</button>
            <span>-4</span>
            <button class="button" @click="addChaosToken('MinusFour')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('MinusFive')">-</button>
            <span>-5</span>
            <button class="button" @click="addChaosToken('MinusFive')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('MinusSix')">-</button>
            <span>-6</span>
            <button class="button" @click="addChaosToken('MinusSix')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('MinusSeven')">-</button>
            <span>-7</span>
            <button class="button" @click="addChaosToken('MinusSeven')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('MinusEight')">-</button>
            <span>-8</span>
            <button class="button" @click="addChaosToken('MinusEight')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('Skull')">-</button>
            <span class="skull-icon"></span>
            <button class="button" @click="addChaosToken('Skull')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('Cultist')">-</button>
            <span class="cultist-icon"></span>
            <button class="button" @click="addChaosToken('Cultist')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('Tablet')">-</button>
            <span class="tablet-icon"></span>
            <button class="button" @click="addChaosToken('Tablet')">+</button>
          </div>
          <div class="tri-button">
            <button class="button" @click="removeChaosToken('ElderThing')">-</button>
            <span class="elder-thing-icon"></span>
            <button class="button" @click="addChaosToken('ElderThing')">+</button>
          </div>
          <div class="tri-button elder-sign-button">
            <button class="button elder-sign-button" @click="removeChaosToken('ElderSign')">-</button>
            <span class="elder-sign"></span>
            <button class="button elder-sign-button" @click="addChaosToken('ElderSign')">+</button>
          </div>
          <div class="tri-button auto-fail-button">
            <button class="button auto-fail-button" @click="removeChaosToken('AutoFail')">-</button>
            <span class="auto-fail"></span>
            <button class="button auto-fail-button" @click="addChaosToken('AutoFail')">+</button>
          </div>
        </div>
        <button class="button close-button" @click="showChaosBag = false">{{$t('close')}}</button>
      </Draggable>
      <CardRow
        v-if="showCards.ref.length > 0"
        :game="game"
        :cards="showCards.ref"
        :isDiscards="viewingDiscard"
        :title="cardRowTitle"
        :playerId="playerId"
        :revealed="revealingCards"
        @choose="choose"
        @close="hideCards"
      />
      <div class="scenario-cards" :class="{ 'scenario-cards--has-badges': showScenarioNotifierBar }">
        <div v-if="anyInTheShadowLocations || inTheShadows.length > 0 || inTheShadowsInvestigators.length > 0" class="in-the-shadows">
          <template v-if="anyInTheShadowLocations">
            <Location
              v-if="inTheShadowLocations.left && game.locations[inTheShadowLocations.left]"
              class="location"
              :game="game"
              :playerId="playerId"
              :location="game.locations[inTheShadowLocations.left]"
              @choose="choose"
              @show="doShowCards"
            />
            <Location
              v-if="inTheShadowLocations.middle && game.locations[inTheShadowLocations.middle]"
              class="location"
              :game="game"
              :playerId="playerId"
              :location="game.locations[inTheShadowLocations.middle]"
              @choose="choose"
              @show="doShowCards"
            />
            <Location
              v-if="inTheShadowLocations.right && game.locations[inTheShadowLocations.right]"
              class="location"
              :game="game"
              :playerId="playerId"
              :location="game.locations[inTheShadowLocations.right]"
              @choose="choose"
              @show="doShowCards"
            />
          </template>
          <EnemyView
            v-for="enemy in inTheShadows"
            :key="enemy.id"
            :enemy="enemy"
            :game="game"
            :playerId="playerId"
            @choose="choose"
          />
          <Investigator
            v-for="investigator in inTheShadowsInvestigators"
            :key="investigator.id"
            :choices="[]"
            :investigator="investigator"
            :playerId="playerId"
            :game="game"
            :portrait="true"
          />
        </div>
        <div v-if="tarotCards.length > 0" class="tarot-cards">
          <div
            v-for="tarotCard in tarotCards"
            :key="tarotCard.arcana"
            class="tarot-card-container"
            :class="{ [tarotCard.facing]: true, 'can-interact': tarotCardAbility(tarotCard) !== -1 }"
            @click="choose(tarotCardAbility(tarotCard))"
          >
            <img :src="imgsrc(`tarot/${tarotCardImage(tarotCard)}`)" :class="tarotCard.facing" class="card tarot-card" />
          </div>
        </div>
        <div v-if="topEnemyInVoid">
          <EnemyView
            :enemy="topEnemyInVoid"
            :game="game"
            :playerId="playerId"
            @choose="choose"
          />
        </div>
        <ScenarioDeck
          v-for="[,scenarioDeck] in scenarioDecks"
          :key="scenarioDeck[0]"
          :deck="scenarioDeck"
          :discardPile="scenarioDeckDiscard(scenarioDeck[0])"
          :game="game"
          :playerId="playerId"
          @choose="choose"
          @show="doShowCards"
        />
        <VictoryDisplay :game="game" :victoryDisplay="victoryDisplay" @choose="choose" :playerId="playerId" />
        <div class="scenario-encounter-decks">
          <div v-if="topOfEncounterDiscard" class="discard" style="grid-area: encounterDiscard">
            <div class="discard-card">
              <img
                :src="topOfEncounterDiscard"
                class="card"
              />
              <span class="deck-size">{{discards.length}}</span>
            </div>


            <div v-if="discards.length > 0" class="buttons">
              <CardsUnderIndicator
                v-if="discards.length > 0"
                v-model:shown="encounterDiscardPopoverShown"
                class="view-discard-button"
                :cards="discards"
                :game="game"
                :playerId="playerId"
                :label="t('scenario.discards')"
                :isDiscards="true"
                :highlighted="encounterDiscardCardsAction"
                :fullWidth="true"
                @choose="choose"
              />
              <template v-if="debug.active">
                <button @click="debug.send(game.id, {tag: 'ShuffleEncounterDiscardBackIn'})">{{ $t('scenarioComponent.shuffleBackIn') }}</button>
              </template>
            </div>
          </div>

          <EncounterDeck
            :game="game"
            :playerId="playerId"
            @choose="choose"
            style="grid-area: encounterDeck"
            v-if="props.scenario.hasEncounterDeck && !hideEncounterDeck"
          />

          <div v-if="topOfSpectralDiscard" class="discard" style="grid-area: spectralDiscard">
            <div class="discard-card">
              <img
                :src="topOfSpectralDiscard"
                class="card"
              />
              <span class="deck-size">{{ spectralDiscards.length }}</span>
            </div>

            <div v-if="spectralDiscards.length > 0" class="buttons">
              <CardsUnderIndicator
                v-model:shown="spectralDiscardPopoverShown"
                class="view-discard-button"
                :cards="spectralDiscards"
                :game="game"
                :playerId="playerId"
                :label="t('scenario.discards')"
                :isDiscards="true"
                :fullWidth="true"
                @choose="choose"
              />
              <template v-if="debug.active">
                <button @click="debug.send(game.id, {tag: 'ShuffleEncounterDiscardBackInByKey', contents: 'SpectralEncounterDeck'})">{{ $t('scenarioComponent.shuffleBackIn') }}</button>
              </template>
            </div>
          </div>

          <EncounterDeck
            v-if="spectralEncounterDeck"
            :spectral="spectralEncounterDeck.length"
            :game="game"
            :playerId="playerId"
            @choose="choose"
            style="grid-area: spectralDeck"
          />
        </div>

        <div class="scenario-decks" :style="scenarioDeckStyles">
          <template v-if="Object.values(game.agendas).length > 0">
            <Agenda
              v-for="(agenda, key) in game.agendas"
              :key="key"
              :agenda="agenda"
              :cardsUnder="cardsUnderAgenda"
              :cardsNextTo="cardsNextToAgenda"
              :remainingStack="scenario.agendaStack[agenda.deckId] || []"
              :completedStack="scenario.completedAgendaStack[agenda.deckId] || []"
              :game="game"
              :playerId="playerId"
              :style="{ 'grid-area': `agenda${agenda.deckId}`, 'justify-self': 'center' }"
              @choose="choose"
              @show="doShowCards"
            />
          </template>
          <div v-else-if="agendaGroupedTreacheries.length > 0" class="treacheries">
            <div v-for="([cCode, treacheries], idx) in agendaGroupedTreacheries" :key="cCode" class="treachery-group" :style="{ zIndex: `calc(var(--z-index-10) * ${agendaGroupedTreacheries.length - idx})` }">
              <div v-for="treacheryId in treacheries" class="treachery-card" :key="treacheryId" >
                <TreacheryView
                  :treachery="game.treacheries[treacheryId]"
                  :game="game"
                  :playerId="playerId"
                  @choose="$emit('choose', $event)"
                  :overlay-delay="310"
                />
              </div>
            </div>
          </div>

          <Act
            v-for="(act, key) in game.acts"
            :key="key"
            :act="act"
            :cardsUnder="cardsUnderAct"
            :cardsNextTo="cardsNextToAct"
            :remainingStack="scenario.actStack[act.deckId] || []"
            :completedStack="scenario.completedActStack[act.deckId] || []"
            :game="game"
            :playerId="playerId"
            :style="{ 'grid-area': `act${act.deckId}`, 'justify-self': 'center' }"
            @choose="choose"
            @show="doShowCards"
          />
        </div>

        <EnemyView
          v-for="enemy in pursuit"
          :key="enemy.id"
          :enemy="enemy"
          :game="game"
          :playerId="playerId"
          @choose="choose"
        />

        <EnemyView
          v-for="enemy in globalEnemies"
          :key="enemy.id"
          :enemy="enemy"
          :game="game"
          :playerId="playerId"
          @choose="choose"
        />

        <Story
          v-for="story in globalStories"
          :key="story.id"
          :story="story"
          :game="game"
          :playerId="playerId"
          @choose="choose"
        />

        <Asset
          v-for="asset in globalAssets"
          :key="asset.id"
          :asset="asset"
          :game="game"
          :playerId="playerId"
          @choose="choose"
        />

        <div class="scenario-guide">
          <div class="scenario-guide-main">
            <div class="scenario-guide-card-wrapper">
              <div class="scenario-guide-card">
                <img
                  class="card"
                  :src="scenarioGuide"
                  :data-spent-keys="JSON.stringify(spentKeys)"
                  :data-depth="currentDepth"
                />
                <img
                  v-for="reference in additionalReferences"
                  class="card"
                  :src="reference"
                />
                <AbilityButton
                  v-for="ability in abilities"
                  :key="ability.index"
                  :ability="ability.contents"
                  :game="game"
                  @click="choose(ability.index)"
                />
              </div>
              <PoolItem class="depth" v-if="currentDepth" type="resource" :amount="currentDepth" />
              <PoolItem class="civilians-slain" v-if="civiliansSlain" type="resource" :amount="civiliansSlain" />
              <PoolItem class="strength-of-the-abyss" v-if="strengthOfTheAbyss !== undefined" type="resource" :amount="strengthOfTheAbyss" />
              <PoolItem class="targets" v-if="targets" type="resource" :amount="targets" />
              <PoolItem class="scraps" v-if="scraps" type="resource" :amount="scraps" />
              <PoolItem class="switches" v-if="switches" type="resource" :amount="switches" />
              <PoolItem class="darkness-level" v-if="darknessLevel" type="resource" :amount="darknessLevel" />
              <div class="spent-keys" v-if="spentKeys.length > 0">
                <KeyToken v-for="k in spentKeys" :key="keyToId(k)" :keyToken="k" :game="game" :playerId="playerId" @choose="choose" />
              </div>
              <PoolItem
                v-if="signOfTheGods"
                class="signOfTheGods"
                type="resource"
                tooltip="Sign of the Gods"
                :amount="signOfTheGods"
              />
              <PoolItem
                v-if="distortion"
                class="distortion"
                type="damage"
                tooltip="Distortion"
                :amount="distortion"
              />
              <PoolItem
                v-if="spiritualDisturbance"
                class="spiritualDisturbance"
                type="horror"
                tooltip="Spiritual Disturbance"
                :amount="spiritualDisturbance"
              />
              <div class="pool" v-if="hasPool">
                <PoolItem v-if="resources && resources > 0" type="resource" :amount="resources" />
                <PoolItem v-if="damage && damage > 0" type="damage" :amount="damage" />
              </div>
            </div>
          </div>
          <div class="keys" v-if="keys.length > 0">
            <KeyToken v-for="k in keys" :key="keyToId(k)" :keyToken="k" :game="game" :playerId="playerId" @choose="choose" />
          </div>
          <label v-if="debug.active" class="debug-difficulty">
            <span>Difficulty</span>
            <select :value="displayedScenarioDifficulty" @change="changeScenarioDifficulty">
              <option value="Easy">Easy</option>
              <option value="Standard">Standard</option>
              <option value="Hard">Hard</option>
              <option value="Expert">Expert</option>
            </select>
          </label>
          <button
            v-if="debug.active && scenarioHasDebugOptions(scenario)"
            type="button"
            class="scenario-debug-toggle"
            @click="showScenarioDebugOptions = true"
          >
            Debug
          </button>
          <CardsUnderIndicator
            v-if="cardsUnderScenarioReference.length > 0"
            class="scenario-cards-under"
            :cards="cardsUnderScenarioReference"
            :game="game"
            :playerId="playerId"
            :label="$t('scenario.cardsUnderScenarioReference')"
            full-width
            @choose="choose"
          />
        </div>

        <div v-if="hollowed.length > 0" class="discard">
          <div class="discard-card">
            <CardView
              :game="game"
              :card="hollowed[0]"
              :playerId="playerId"
              class="card"
            />
          </div>
          <div class="buttons">
            <CardsUnderIndicator
              v-model:shown="hollowedPopoverShown"
              class="view-discard-button"
              :cards="hollowed"
              :game="game"
              :playerId="playerId"
              :label="t('scenario.hollowed')"
              :fullWidth="true"
              @choose="choose"
            />
          </div>
        </div>
        <SkillTest
            v-if="game.skillTest"
            :game="game"
            :chaosBag="scenario.chaosBag"
            :skillTest="game.skillTest"
            :playerId="playerId"
            @choose="choose"
        >
        </SkillTest>

        <div v-if="showScenarioNotifierBar" class="scenario-badges" aria-label="Scenario reminders">
          <div
            v-for="badge in scenarioBadges"
            :key="badge.key"
            class="scenario-badge"
            v-tooltip="badge.detail"
            :aria-label="badge.detail ? `${badge.label}: ${badge.detail}` : badge.label"
          >
            <span class="scenario-badge-icon" aria-hidden="true">{{ badge.icon }}</span>
            <span class="scenario-badge-text">
              <strong>{{ badge.label }}</strong>
              <small v-if="badge.detail">{{ badge.detail }}</small>
            </span>
          </div>
          <span
            v-if="realityAcidLightDevoured"
            ref="realityAcidLightAnchor"
            class="scenario-badge reality-acid-light-switch-anchor"
            aria-hidden="true"
          >
            <span class="reality-acid-light-switch-track">
              <span class="reality-acid-light-switch-knob"></span>
            </span>
            <span class="scenario-badge-text reality-acid-light-switch-label">
              <strong>{{ realityAcidLightActive ? 'Lights off' : 'Lights on' }}</strong>
            </span>
          </span>
          <Teleport to="body">
            <button
              v-if="realityAcidLightDevoured"
              type="button"
              class="scenario-badge reality-acid-light-switch reality-acid-light-switch--floating"
              :class="{ 'reality-acid-light-switch--on': realityAcidLightActive }"
              :style="{
                left: `${realityAcidLightRect.left}px`,
                top: `${realityAcidLightRect.top}px`,
                width: `${realityAcidLightRect.width}px`,
                height: `${realityAcidLightRect.height}px`,
              }"
              :title="realityAcidLightActive ? 'Turn the lights back on' : 'Turn the lights off'"
              @click="$emit('toggleRealityAcidLight')"
            >
              <span class="reality-acid-light-switch-track" aria-hidden="true">
                <span class="reality-acid-light-switch-knob"></span>
              </span>
              <span class="scenario-badge-text reality-acid-light-switch-label">
                <strong>{{ realityAcidLightActive ? 'Lights off' : 'Lights on' }}</strong>
              </span>
            </button>
          </Teleport>
        </div>

      </div>


      <div class="location-cards-container" @dblclick.passive="toggleZoom">
        <div class="location-cards-scroller" ref="scrollerRef">
        <div class="location-cards-stage">
        <Connections :game="game" :playerId="playerId" :enableCosmicEmissaryAnimation="enableCosmicEmissaryAnimation" />
        <transition-group name="map" tag="div" ref="locationMap" class="location-cards" :css="props.scenario.id !== 'c10651'" :style="locationStyles" @before-leave="beforeLeave">
          <div
            v-for="location in locations"
            :key="location.label"
            class="location-cell"
            :class="{ 'location-cell--can-interact': locationCanInteract(location) }"
            :data-location-id="location.id"
            :data-label="location.label"
            :style="[
              { 'grid-area': location.label, 'justify-self': 'center' },
              cosmicEmissaryLocationCellStyles[location.label] ?? {},
            ]"
          >
            <div
              class="location-wrapper"
              :style="locationOffsetStyle(location)"
              @pointerdown.capture="onLocationPointerDown($event, location)"
              @click.capture="suppressLocationInteractionWhenUnlocked"
            >
              <div
                v-if="abyssIsLocation && location.label === 'theAbyss'"
                class="abyss-location-count"
                v-tooltip="`${abyssDeckCount} cards in The Abyss`"
              >
                {{ abyssDeckCount }}
              </div>
              <Location
                class="location"
                :class="{ 'location--unlocked': locationsUnlocked, 'location--dragging': draggingLocationId === location.id }"
                :game="game"
                :playerId="playerId"
                :location="location"
                @choose="choose"
                @show="doShowCards"
              />
            </div>
          </div>
          <EnemyView
            v-for="enemy in enemiesAsLocations"
            :key="enemy.id"
            :enemy="enemy"
            :game="game"
            :playerId="playerId"
            :data-label="enemy.asSelfLocation"
            :data-rotation="enemy.meta?.rotation ?? null"
            :style="[
              { 'grid-area': enemy.asSelfLocation, 'justify-self': 'center', 'align-items': 'center' },
              enemy.asSelfLocation ? (cosmicEmissaryEnemyStyles[enemy.asSelfLocation] ?? {}) : {},
            ]"
            @choose="choose"
          />
          <div
            v-for="group in gridConcealed"
            :key="`${group.position.x}-${group.position.y}`"
            class='concealed-card-group'
            :style="{'grid-area': positionToGridArea(group.position)}"
          >
            <div v-if="group.unknown.length > 0" class='concealed-card-stack'>
              <ConcealedCardView :card="group.unknown[0]" :game="game" :playerId="playerId" @choose="choose" />
              <span class='count'>{{group.unknown.length}}</span>
            </div>
            <ConcealedCardView v-for="card in group.known" :key="card.id" :card="card" :game="game" :playerId="playerId" @choose="choose" />
          </div>

          <template v-if="barriers">
            <div v-for="[area, amount] in Object.entries(barriers)" :key="area" class="barrier" :class="{ vertical: isVertical(area) }" :style="{ 'grid-area': `barrier-${area}` }">
              <img v-for="n in amount" :key="n" :src="imgsrc('resource.png')" />
              <button v-if="debug.active && (amount as number > 0)" @click="debug.send(game.id, {tag: 'ScenarioCountDecrementBy', contents: [{ 'tag': 'Barriers', 'contents': area.split('--') }, 1]})">x</button>
            </div>
          </template>

          <template v-if="scenario.usesGrid">
            <template v-for="u in unusedLabels" :key="u">
              <div
                v-if="unusedCanInteract(u) !== -1"
                class="empty-grid-position card"
                :class="{ 'can-interact': unusedCanInteract(u) !== -1}"
                :style="{ 'grid-area': u}"
                @click="choose(unusedCanInteract(u))"
                >
              </div>
            </template>
          </template>
        </transition-group>
        </div>
        <div v-if="playerLocationZones.length > 0" class="player-location-zones">
          <section
            v-for="zone in playerLocationZones"
            :key="zone.investigatorId"
            class="player-location-zone"
          >
            <h3>{{ zone.name }}</h3>
            <div class="player-location-zone__cards">
              <Location
                v-for="location in zone.locations"
                :key="location.id"
                class="player-area-location"
                :game="game"
                :playerId="playerId"
                :location="location"
                @choose="choose"
                @show="doShowCards"
              />
            </div>
          </section>
        </div>
        </div>
      </div>

      <div id="player-zone">
        <PlayerTabs
          :game="game"
          :playerId="playerId"
          :players="players"
          :playerOrder="playerOrder"
          :activePlayerId="activePlayerId"
          :tarotCards="props.scenario.tarotCards"
          @choose="choose"
        >
          <div class="zoom-control">
            <button class="zoom-btn" @pointerdown.stop="startHold(decreaseZoom)" @pointerup="stopHold" @pointerleave="stopHold">−</button>
            <input v-model.number="locationsZoom" type="range" min="0.25" max="6" step="0.05" class="zoom-slider" />
            <button class="zoom-btn" @pointerdown.stop="startHold(increaseZoom)" @pointerup="stopHold" @pointerleave="stopHold">+</button>
            <button
              class="zoom-btn"
              :class="{ 'zoom-btn--active': locationsUnlocked }"
              @click.stop="toggleLocationsUnlocked"
              v-tooltip="locationsUnlocked ? 'Lock locations' : 'Unlock locations to drag'"
            >
              <LockOpenIcon v-if="locationsUnlocked" class="zoom-btn__icon" />
              <LockClosedIcon v-else class="zoom-btn__icon" />
            </button>
            <button
              v-if="hasAnyOffset"
              class="zoom-btn"
              @click.stop="resetLocationsLayout"
              v-tooltip="'Reset location positions'"
            >
              <ArrowUturnLeftIcon class="zoom-btn__icon" />
            </button>
          </div>
        </PlayerTabs>
        <div id="totals">
          <PoolItem type="doom" :amount="game.totalDoom" tooltip="Total Doom" />
          <PoolItem type="clue" :amount="game.totalClues" tooltip="Total Spendable Clues" />
          <PoolItem v-if="blessTokens > 0" type="ct_bless" :amount="blessTokens" />
          <PoolItem v-if="curseTokens > 0" type="ct_curse" :amount="curseTokens" />
          <PoolItem v-if="frostTokens > 0" type="ct_frost" :amount="frostTokens" />
        </div>
      </div>
    </div>
    <div class="phases">
      <div class="phase" :class="{ 'active-phase': phase == 'MythosPhase' }">
        <div class="subphases">
          <div v-tooltip.left="$t('phase.mythosPhaseBeginsStep')" :class="{'current': phaseStep?.contents === 'MythosPhaseBeginsStep' }">1.1</div>
          <div v-tooltip.left="$t('phase.placeDoomOnAgendaStep')" :class="{'current': phaseStep?.contents === 'PlaceDoomOnAgendaStep'}">1.2</div>
          <div v-tooltip.left="$t('phase.checkDoomThresholdStep')" :class="{'current': phaseStep?.contents === 'CheckDoomThresholdStep'}">1.3</div>
          <div v-tooltip.left="$t('phase.eachInvestigatorDrawsEncounterCardStep')" :class="{'current': phaseStep?.contents === 'EachInvestigatorDrawsEncounterCardStep'}">1.4</div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'MythosPhaseWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.mythosPhaseEndsStep')" :class="{'current': phaseStep?.contents === 'MythosPhaseEndsStep'}">1.5</div>
        </div>
        <div>{{$t('phase.mythosPhase')}}</div>
      </div>
      <div class="phase" :class="{ 'active-phase': phase == 'InvestigationPhase' }">
        <div class="subphases">
          <div v-tooltip.left="$t('phase.investigationPhaseBeginsStep')" :class="{'current': phaseStep?.contents === 'InvestigationPhaseBeginsStep'}">2.1</div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'InvestigationPhaseBeginsWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.nextInvestigatorsTurnBeginsStep')" :class="{'current': phaseStep?.contents === 'NextInvestigatorsTurnBeginsStep'}">2.2</div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'NextInvestigatorsTurnBeginsWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.investigatorTakesActionStep')" :class="{'current': phaseStep?.contents === 'InvestigatorTakesActionStep'}">2.2.1</div>
          <div v-tooltip.left="$t('phase.investigatorsTurnEndsStep')" :class="{'current': phaseStep?.contents === 'InvestigatorsTurnEndsStep'}">2.2.2</div>
          <div v-tooltip.left="$t('phase.investigationPhaseEndsStep')" :class="{'current': phaseStep?.contents === 'InvestigationPhaseEndsStep'}">2.3</div>
        </div>
        <div>{{$t('phase.investigationPhase')}}</div>
      </div>
      <div class="phase" :class="{ 'active-phase': phase == 'EnemyPhase' }">
        <div class="subphases">
          <div v-tooltip.left="$t('phase.enemyPhaseBeginsStep')" :class="{'current': phaseStep?.contents === 'EnemyPhaseBeginsStep'}">3.1</div>
          <div v-tooltip.left="$t('phase.hunterEnemiesMoveStep')" :class="{'current': phaseStep?.contents === 'HunterEnemiesMoveStep'}">3.2 <span v-if="phaseStep?.contents === 'HunterEnemiesMoveStep'">{{$t('phase.hunterEnemiesMoveStep')}}</span></div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'ResolveAttacksWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.resolveAttacksStep')" :class="{'current': phaseStep?.contents === 'ResolveAttacksStep'}">3.3</div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'AfterResolveAttacksWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.enemyPhaseEndsStep')" :class="{'current': phaseStep?.contents === 'EnemyPhaseEndsStep'}">3.4</div>
        </div>  
        <div>{{$t('phase.enemyPhase')}}</div>
      </div>
      <div class="phase" :class="{ 'active-phase': phase == 'UpkeepPhase' }">
        <div class="subphases">
          <div v-tooltip.left="$t('phase.upkeepPhaseBeginsStep')" :class="{'current': phaseStep?.contents === 'UpkeepPhaseBeginsStep'}">4.1</div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'UpkeepPhaseBeginsWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.resetActionsStep')" :class="{'current': phaseStep?.contents === 'ResetActionsStep'}">4.2</div>
          <div v-tooltip.left="$t('phase.readyExhaustedStep')" :class="{'current': phaseStep?.contents === 'ReadyExhaustedStep'}">4.3</div>
          <div v-tooltip.left="$t('phase.drawCardAndGainResourceStep')" :class="{'current': phaseStep?.contents === 'DrawCardAndGainResourceStep'}">4.4</div>
          <div v-tooltip.left="$t('phase.checkHandSizeStep')" :class="{'current': phaseStep?.contents === 'CheckHandSizeStep'}">4.5</div>
          <div v-tooltip.left="$t('phase.upkeepPhaseEndsStep')" :class="{'current': phaseStep?.contents === 'UpkeepPhaseEndsStep'}">4.6</div>
        </div>
        <div>{{$t('phase.upkeepPhase')}}</div>
      </div>
    </div>
  </div>

  <Teleport to="body">
    <ScenarioDebug
      v-if="debug.active && showScenarioDebugOptions"
      :game="game"
      :scenario="scenario"
      @close="showScenarioDebugOptions = false"
    />
  </Teleport>
</template>

<style scoped>
.card {
  border-radius: 5px;
  width: var(--card-width);
  height: auto;
  aspect-ratio: var(--card-aspect);
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}

.card--sideways {
  width: auto;
  height: calc(var(--card-width) * 2);
  aspect-ratio: var(--card-sideways-ratio);
}

.deck-size {
  position: absolute;
  font-weight: bold;
  font-size: 1.2em;
  color: rgba(255, 255, 255, 1);
  left: 50%;
  bottom: 55%;
  transform: translateX(-50%) translateY(-50%);
  pointer-events: none;
  -webkit-text-stroke: 1px black;
}

.scenario-cards {
  display: flex;
  align-self: center;
  align-items: flex-start;
  justify-content: center;
  padding: 10px 0;
  position: relative;
  width: 100%;
  gap: 10px;
  z-index: var(--z-index-neg-2);
  background: rgba(0, 0, 0, 0.14);
  border-bottom: 1px solid rgba(255, 255, 255, 0.08);
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.18);

  @media (max-width: 800px) and (orientation: portrait) {
    padding-top: 10px;
    padding-bottom: 0;
    min-height: calc(var(--card-height) + 10px);
  }
}

/* A revealed (slid-out) treachery extends down into the board's region; lift the
   whole scenario-cards layer above the board (normally z-index: var(--z-index-neg-2), behind it) so
   the revealed card and its buttons stay clickable. */
.scenario-cards:has(.treachery-group.is-revealed) {
  z-index: var(--z-index-1);
}

.scenario-cards--has-badges {
  z-index: calc(var(--z-index-9999) + 2);
  padding-top: 56px;
}

.clue {
  position: relative;
  width: 57px;
  height: 54px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: black;
  font-weight: 900;
  font-size: 1.5em;

  img {
    position: absolute;
    top: 0;
    bottom: 0;
    left: 0;
    right: 0;
    margin: auto;
    z-index: var(--z-index-neg-1);
  }
}

.scenario-body {
  background: var(--background);
  z-index: var(--z-index-1);
  width: 100%;
  flex: 1;
  inset: 0;
  position: relative;

  display: grid;
  grid-template-rows: auto 1fr;

  &.scenario-body--notifier-overlays {
    z-index: auto;
  }

  &.split-view {
    grid-template-columns: 1fr 2fr;
    grid-template-rows: 1fr 3fr;
    padding-bottom: 10px;
    row-gap: 30px;

    &:deep(.player-info) {
      grid-column: 1;
      grid-row: 2 / 5;
      display: flex;
      flex-direction: column;

      .tab {
        display: flex;
        flex-direction: column;
        flex: 1;
        border-top-right-radius: 10px;
        overflow: hidden;
      }

      .player-cards {
        overflow: auto;
        display: flex;
        flex-direction: column;
        flex: 1;
        border-top-right-radius: 10px;
      }

      .player {
        display: flex;
        flex-direction: column;
        gap: 10px;
        width: 100%;
        flex: 1;
      }
    }

    .scenario-cards {
      grid-column: 1;
      grid-row: 1 / 2;
      flex-wrap: wrap;
    }

    .location-cards-container {
      grid-column: 2;
      grid-row: 1 / 5;
    }
  }
}

.location-cards-scroller {
  flex: 1;
  min-height: 0;
  min-width: 0;
  overflow: auto;
  touch-action: manipulation;
  scrollbar-gutter: stable both-edges;
  scroll-padding: 30%;
  padding: 24px;
  box-sizing: border-box;
  display: flex;
  flex-direction: column;
  gap: 16px;
  align-items: safe center;
  justify-content: safe center;
}

.player-location-zones {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 12px;
  flex-shrink: 0;
  max-width: 100%;
  padding: 0 12px 12px;
}

.player-location-zone {
  display: flex;
  flex-direction: column;
  gap: 6px;
  padding: 8px;
  border: 1px solid rgba(255, 255, 255, 0.18);
  border-radius: 8px;
  background: rgba(0, 0, 0, 0.28);

  h3 {
    margin: 0;
    color: white;
    font-size: 0.85rem;
    font-weight: 600;
    text-align: center;
  }
}

.player-location-zone__cards {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  align-items: flex-start;
  gap: 12px;

  &:deep(.location) {
    min-width: calc(var(--card-width) + 120px);
  }
}

.location-cards-stage {
  position: relative;
  display: grid;
  flex-shrink: 0;
  width: max-content;
  height: max-content;
  overflow: hidden;
}

.location-cards {
  display: grid;
  grid-area: 1 / 1;
  position: relative;
  z-index: 1;
  transition: transform 0.2s ease;
}

.location-cards-container {
  display: flex;
  overflow: hidden;
  flex: 1;
  position: relative;
  @media (max-width: 800px) and (orientation: portrait) {
    padding-top: 5px;
    padding-bottom: 5px;
  }
}

.portrait {
  border-radius: 3px;
}

.portrait--can-move {
  cursor: pointer;
  border: 3px solid var(--select);
}

.location--can-move-to {
  border: 3px solid var(--select);
  cursor: pointer;
}

.agenda-container, .act-container {
  align-self: flex-start;
}

.discard {
  height: 100%;
  position: relative;
  display: flex;
  flex-direction: column;
  gap: 5px;
  &:deep(.card) {
    margin: 0;
    box-shadow: none;
  }
  .buttons {
    display: flex;
    flex-direction: column;
    gap: 5px;
  }
}

.discard-card {
  position: relative;
  width: fit-content;
  line-height: 0;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  .card {
    box-shadow: unset;
  }
  &::after {
    border-radius: 6px;
    pointer-events: none;
    content: "";
    position: absolute;
    inset: 0;
    background-color: #FFF;
    opacity: .85;
    mix-blend-mode: saturation;
  }
}


.view-out-of-play-button {
  text-decoration: none;
  position: absolute;
  transform: translate(100%, -50%) rotate(90deg) translate(0%, 50%) translate(0%, 10px);
  svg {
    transform: rotate(-90deg)
  }
  transform-origin: center left;
  top: 0px;
  right: 0px;
  border: 0;
  color: white;
  background: var(--background-mid);
  font-size: 1.2em;
  padding: 5px 15px;
}

.view-removed-from-play-button {
  text-decoration: none;
  position: absolute;
  transform: translate(100%, -50%) rotate(90deg) translate(0%, 50%) translate(0%, 50px);
  svg {
    transform: rotate(-90deg)
  }
  transform-origin: center left;
  top: 0px;
  right: 0px;
  border: 0;
  color: white;
  background: #a5b5bc;
  font-size: 1.2em;
  padding: 5px 15px;
}

.phases {
  display: flex;
  align-items: flex-end;
  writing-mode: vertical-rl;
  text-orientation: mixed;
  justify-content: space-around;
  background-color: #b8c1c6;
  text-transform: uppercase;
  font-family: Arial;
  > div {
    flex: 1;
    text-align: center;
  }

  @media (max-width: 768px) and (orientation: portrait) {
    display: none;
  }
}


.phase {
  display: flex;
  flex-direction: column;
  width: 100%;
}

.subphases {
  position: relative;
  font-size: 0.7em;
  flex: 1;
  writing-mode: lr-tb;
  text-orientation: revert;
  display: flex;
  min-width: min-content;
  flex-direction: column;
  height: 100%;
  justify-content: space-around;
  color: #b8c1c6;
  background: #484E51;
  text-transform: uppercase;
  font-family: Arial;
  .current {
    background: rgba(0, 0, 0, 0.5) !important;
    position: relative;
    span {
      position: absolute;
      right: 100%;
      z-index: var(--z-index-100000);
      background: var(--neutral-extra-dark);
      height: 100%;
      display: flex;
      align-items: center;
      justify-content: center;
      padding-inline: 10px;
      pointer-events: none;
    }
  }
  > div {
    width: 100%;
    padding: 0 5px;
    display: flex;
    justify-content: center;
    flex: 1;
    align-items: center;
    &:hover {
      background: rgba(0, 0, 0, 0.5);
    }
  }
  > div:nth-of-type(2n) {
    background: #5a6062;
    &:hover {
      background: rgba(0, 0, 0, 0.5);
    }
  }
}

.scenario {
  display: flex;
  user-select: none;
  width: 100%;
  height: 100%;
  flex: 1;
}

.active-phase {
  font-weight: bold;
  background-color: #8e9ca4;
}

.scenario-guide {
  display: flex;
  flex-direction: column;
  position: relative;
  isolation: isolate;
}

.scenario-guide-main {
  position: relative;
  width: fit-content;
}

.scenario-badges {
  position: absolute;
  top: 0;
  right: 0;
  left: 0;
  z-index: calc(var(--z-index-9999) + 2);
  display: flex;
  align-items: center;
  justify-content: flex-start;
  gap: 6px;
  min-height: 34px;
  padding: 5px 10px;
  overflow: visible;
  pointer-events: none;
  background: rgba(0, 0, 0, 0.2);
  border-bottom: 1px solid rgba(255, 255, 255, 0.08);
}

.scenario-badge {
  position: relative;
  display: inline-flex;
  pointer-events: auto;
  align-items: center;
  gap: 7px;
  min-width: 0;
  max-width: 260px;
  border: 1px solid rgb(255 255 255 / 16%);
  border-left: 3px solid rgb(160 185 190 / 55%);
  border-radius: 6px;
  background: rgb(18 21 24 / 92%);
  color: white;
  padding: 4px 8px 4px 6px;
  box-shadow: 0 1px 3px rgb(0 0 0 / 18%);
}


.scenario-badge-icon {
  flex: 0 0 auto;
  font-size: 0.95rem;
  line-height: 1;
  white-space: nowrap;
}

.scenario-badge-text {
  display: flex;
  flex-direction: column;
  gap: 1px;
  min-width: 0;
  line-height: 1.05;
}

.scenario-badge-text strong {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  font-size: 0.68rem;
  letter-spacing: 0.01em;
}

.scenario-badge-text small {
  overflow: hidden;
  opacity: 0.68;
  text-overflow: ellipsis;
  white-space: nowrap;
  font-size: 0.58rem;
}

.reality-acid-light-switch-anchor {
  min-width: 136px;
  visibility: hidden;
}

.reality-acid-light-switch {
  z-index: calc(var(--z-index-9999) + 3);
  isolation: isolate;
  pointer-events: auto;
  cursor: pointer;
  border-color: rgb(255 255 255 / 36%);
  border-left-color: rgb(255 225 105 / 95%);
  background: rgb(32 36 42 / 98%);
  color: #fff;
  text-shadow: 0 1px 2px rgb(0 0 0 / 90%);
  box-shadow: 0 2px 8px rgb(0 0 0 / 65%);
}

.reality-acid-light-switch--on {
  box-shadow:
    inset 0 0 12px rgb(255 225 105 / 18%),
    0 0 0 1px rgb(255 225 105 / 16%),
    0 0 20px rgb(255 225 105 / 42%),
    0 2px 8px rgb(0 0 0 / 65%);
}

.reality-acid-light-switch--floating {
  position: fixed;
  z-index: 2147483647;
  max-width: none;
}

.reality-acid-light-switch-track {
  position: relative;
  flex: 0 0 auto;
  width: 34px;
  height: 18px;
  border-radius: 999px;
  background: #d6c36a;
  box-shadow: inset 0 0 0 1px rgb(0 0 0 / 35%);
}

.reality-acid-light-switch-knob {
  position: absolute;
  top: 3px;
  left: 18px;
  width: 12px;
  height: 12px;
  border-radius: 50%;
  background: white;
  box-shadow: 0 1px 3px rgb(0 0 0 / 50%);
  transition: left 120ms ease;
}

.reality-acid-light-switch--on .reality-acid-light-switch-track {
  background: #263241;
}

.reality-acid-light-switch--on .reality-acid-light-switch-knob {
  left: 4px;
}

.reality-acid-light-switch .scenario-badge-text strong {
  color: #fff;
  font-size: 0.74rem;
}

.reality-acid-light-switch .scenario-badge-text small {
  color: #ffe078;
  opacity: 1;
}

.reality-acid-light-switch-label {
  text-align: left;
}

.scenario-cards-under {
  align-self: center;
  margin-top: 2px;
}

/* A single-cell grid: the card stack and every counter overlay share the same
   cell, so tokens always land on the card no matter what siblings (pool, keys,
   cards-underneath button) render around it. */
.scenario-guide-card-wrapper {
  display: grid;
  width: fit-content;

  > * {
    grid-area: 1 / 1;
  }

  .depth, .civilians-slain, .targets, .scraps, .switches, .darkness-level, .strength-of-the-abyss {
    align-self: end;
    justify-self: end;
    pointer-events: none;
    z-index: var(--z-index-10);
  }

  .signOfTheGods {
    align-self: end;
    justify-self: end;
    pointer-events: none;
    z-index: var(--z-index-10);
  }

  .distortion {
    align-self: end;
    justify-self: end;
    pointer-events: none;
    z-index: var(--z-index-10);
  }

  .spiritualDisturbance {
    align-self: end;
    justify-self: end;
    pointer-events: none;
    z-index: var(--z-index-10);
  }

  .pool {
    align-self: end;
    justify-self: end;
    pointer-events: none;
    z-index: var(--z-index-10);
  }

  .spent-keys {
    align-self: end;
    justify-self: center;
    margin-bottom: 20px;
    pointer-events: none;
    z-index: var(--z-index-10);
  }
}

.map-move {
  transition: all 0.6s cubic-bezier(0.23, 1, 0.32, 1);
}

.map-leave-to {
  opacity: 0;
}

.map-leave-active {
  position: absolute;
}

.scenario-decks {
  gap: 5px;
  @media (max-width: 800px) and (orientation: portrait) {
    display:flex !important;
  }
}

.scenario-encounter-decks {
  display: grid;
  grid-template: "encounterDiscard encounterDeck" "spectralDiscard spectralDeck";
  gap: 10px;
}

.empty-grid-position {
  content: " ";
  box-shadow: unset;
  justify-self: center;
}

.can-interact {
  background: rgba(0, 0, 0, 0.5);
  border: 2px solid var(--select);
  cursor: pointer;
}

.pool {
  position: absolute;
  top: 10%;
  align-items: center;
  display: flex;
  align-self: flex-start;
  align-items: flex-end;
  pointer-events: none;

  * {
    transform: scale(0.9);
  }
}

.tarot-card {
  width: var(--card-width);
  aspect-ratio: var(--card-tarot-aspect);
  margin: 0;
}

.tarot-card-choices {
  background: v-bind(tarotCardBackground);
  background-position: center;
  background-size: contain;
  position: absolute;
  z-index: var(--z-index-1000);
  margin: auto;
  inset: 0;
  width: fit-content;
  height: fit-content;
  display: flex;
  gap: 10px;
  padding: 10px;
}

.tarot-card-container {
  transition: transform 0.5s ease-in-out;
  display:flex;
  position: relative;
  border-radius: 5px;

  &.Reversed {
    transform: rotateZ(180deg);
    &:before {
      transform-origin: center;
      animation-fill-mode: forwards;
      animation: shadow-rotate 0.5s linear;
      transform: translate(0, -12px);
    }
  }
}

@keyframes shadow-rotate {
  0% {
    transform: translate(0, 12px);
  }
  25% {
    transform: translate(6px, 12px);
  }
  50% {
    transform: translate(12px, 0px);
  }
  75% {
    transform: translate(6px, -12px);
  }
  100% {
    transform: translate(0, -12px);
  }
}

.tarot-cards {
  display: flex;
  gap: 10px;
  margin-inline: 10px;
}

/* We lower the margin so things line up a bit better. */
[data-scenario='c06333'] .location-cards:deep(.location-container) {
  margin: 20px !important;
}

.buttons-row {
  padding: 10px;
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: space-around;

  .blessed {
    background-color: var(--blessed);
  }

  .cursed {
    background-color: var(--cursed);
  }

  .auto-fail-button {
    background-color: var(--auto-fail);
  }

  .elder-sign-button {
    background-color: var(--elder-sign);
  }

  .frost {
    background-color: var(--frost);
  }

  button {
    margin: 0;
  }
}

.button {
  padding: 5px 10px;
  font-size: 1em;
}

.card-row-cards {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 10px;
  gap: 2px;
  flex-wrap: wrap;

  .card-row-card {
    position: relative;
  }
}

.location {
  &:hover {
    z-index: var(--z-index-100);
  }
}

.button{
  border: 0;
  margin-top: 2px;
  color: #fff;
  cursor: pointer;
  border-radius: 4px;
  background-color: var(--button);
  z-index: var(--z-index-1000);
  width: 100%;
  min-width: max-content;
}

.keys {
  display: flex;
  flex-direction: row;
  gap: 2px;
}

.scenario-guide-card {
  position: relative;
}

.debug-difficulty {
  display: flex;
  flex-direction: column;
  align-self: center;
  gap: 2px;
  margin-top: 2px;
  width: var(--card-width);
  color: white;
  font-size: 0.65rem;
  font-weight: 700;
  text-transform: uppercase;
}

.debug-difficulty select {
  width: 100%;
  min-width: 0;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: rgba(0, 0, 0, 0.65);
  color: white;
  font-size: 0.75rem;
}

.scenario-debug-toggle {
  align-self: center;
  width: var(--card-width);
  margin-top: 4px;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: var(--button);
  color: white;
  cursor: pointer;
  font-size: 0.7rem;
  font-weight: 700;
  text-transform: uppercase;
}


.spent-keys {
  pointer-events: none;
  display: flex;
  flex-direction: row;
  gap: 2px;

  &:deep(img) {
    width: 10px;
  }
}

.zoom-control {
  display: flex;
  align-items: center;
  gap: 6px;
  padding: 2px 8px;
  flex-shrink: 0;
  align-self: center;

  @media (pointer: coarse) {
    display: none;
  }
}

@media not screen {
  .zoom-control { display: none }
}

.zoom-btn {
  background: none;
  border: none;
  color: var(--title);
  font-size: 18px;
  width: 22px;
  height: 22px;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 50%;
  line-height: 1;
  padding: 0;
  transition: background 0.15s, color 0.15s;
  flex-shrink: 0;

  &:hover {
    background: var(--background-mid);
    color: var(--spooky-green);
  }

  &:active {
    background: var(--button-1-highlight);
    color: white;
  }
}

.zoom-btn--active {
  background: var(--button-1-highlight);
  color: white;
}

.zoom-btn__icon {
  width: 14px;
  height: 14px;
}

.location-cell {
  /* Grid placement + TransitionGroup FLIP target. The inner .location carries
     the user's drag offset transform, so rotation reshuffles (which FLIP-
     animate the wrapper) don't wipe that offset.

     The cell's untransformed grid box can overlap a different translated
     location. Do not let that invisible/original box win hit-testing; only the
     translated visible wrapper should receive pointer events. */
  display: block;
  position: relative;
  pointer-events: none;
}

.location-cell--can-interact {
  z-index: var(--z-index-20);
}

/* While a swarm is fanned open (hovering the swarm, or its abilities menu is open),
   lift the whole cell above its neighbours so the fanned cards aren't occluded by an
   adjacent location's wrapper — otherwise sweeping across the fan would lose hover. */
.location-cell:has(.swarm:hover),
.location-cell:has(.enemy--swarming.showAbilities) {
  z-index: var(--z-index-30);
}

.location-wrapper {
  width: fit-content;
}

.abyss-location-count {
  display: block;
  width: fit-content;
  margin: 0 auto 6px;
  padding: 2px 8px;
  border-radius: 999px;
  background: rgba(10, 13, 25, 0.9);
  border: 1px solid rgba(111, 225, 210, 0.8);
  box-shadow: 0 0 8px rgba(111, 225, 210, 0.45);
  color: white;
  font-size: 0.85rem;
  font-weight: bold;
  cursor: help;
}

.location-cell > .location-wrapper {
  pointer-events: auto;

  /* Animate the offset along with the wrapper's FLIP move during rotation so
     the offset doesn't snap to its rotated value before the wrapper slides
     into place. Same easing/duration as .map-move keeps them in sync. */
  transition: transform 0.6s cubic-bezier(0.23, 1, 0.32, 1);
}

.location--unlocked {
  cursor: grab;
  outline: 1px dashed var(--spooky-green);
  outline-offset: 4px;
  border-radius: 6px;
  touch-action: none;
}

.location--dragging {
  cursor: grabbing;
  z-index: var(--z-index-50);
  transition: none !important;
}

.zoom-slider {
  -webkit-appearance: none;
  appearance: none;
  width: 90px;
  height: 4px;
  background: var(--box-border);
  border-radius: 2px;
  outline: none;
  cursor: pointer;
}

.zoom-slider::-webkit-slider-thumb {
  -webkit-appearance: none;
  appearance: none;
  width: 14px;
  height: 14px;
  border-radius: 50%;
  background: #5a9465;
  cursor: pointer;
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.5);
  transition: background 0.15s, transform 0.1s;
}

.zoom-slider::-webkit-slider-thumb:hover {
  background: #6aaa75;
  transform: scale(1.2);
}

.zoom-slider::-moz-range-thumb {
  width: 14px;
  height: 14px;
  border-radius: 50%;
  background: #5a9465;
  cursor: pointer;
  border: none;
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.5);
}

.barrier {
  display: flex;
  flex-direction: column;
  width: calc(var(--card-width) / 4);
  height: calc(var(--card-width) / var(--card-aspect));
  align-self: flex-start;
  justify-content: center;

  img {
    width: 20px;
  }

  &.vertical {
    flex-direction: row;
    height: 40px;
    width: 100px;
    justify-self: center;
    img {
      height: 20px;
    }
  }
}

.treacheries {
  position:relative;
  display: flex;
  flex-direction: column;
  gap: 5px;
}

.treachery-group {
  display: flex;
  gap: 5px;
  flex-direction: row;
  &:not(:first-of-type) {
    margin-top: -50px;
  }
  /*position: inherit; */
  transition: margin-top 0.3s;
  position: relative;

  &:hover {
    margin-top: 0px;
    .treachery-card {
      margin-left: 0;
    }
  }
}

.treachery {
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}

.treachery-card {
  margin-left: -50px;
  transition: margin-left 0.3s;
  &:first-of-type {
    margin-left: 0;
  }
}

.scenario-guide-card {
  display: flex;
  flex-direction: column;
  img:nth-of-type(1) {
    z-index: var(--z-index-2);
  }

  img:not(:nth-of-type(1)) {
    z-index: var(--z-index-1);
    margin-top: calc((var(--card-width) / (3 / 2)) * -1);
  }
}

#player-zone {
  display: flex;
  flex-direction: row;
  background: #181c2a;
  border-top: 1px solid rgba(255, 255, 255, 0.08);
  box-shadow: 0 -2px 8px rgba(0, 0, 0, 0.4);
  .player-info {
    flex: 1;
  }
  @media (max-width: 800px) {
    padding-bottom: 50px;
  }
}

#totals {
  display: flex;
  flex-direction: column;
  gap: 5px;
  padding: 5px;
  background: darkslategrey;
  margin-top: 10px;
  border-top-left-radius: 10px;
  box-shadow: -1px 1px 3px rgba(0, 0, 0, 0.8);
}

.tri-button {
  background-color: var(--button);
  color: white;
  padding: 0;
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: center;
  border-radius: 4px;
  overflow: hidden;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  .bless-icon, .curse-icon {
    color: white;
    min-width: fit-content;
  }
  span {
    height: 100%;
    aspect-ratio: 1 / 1;
    padding: 0 5px;
    border-left: 1px solid rgba(0, 0, 0, 0.2);
    border-right: 1px solid rgba(0, 0, 0, 0.2);
    justify-content: center;
    display: flex;
    align-items: center;
  }

  .skull-icon {
    color: var(--skull);
    text-shadow: 0px 0px 2px rgba(255, 255, 255, 0.5);
  }

  .tablet-icon {
    color: var(--tablet);
    text-shadow: 0px 0px 2px rgba(255, 255, 255, 0.5);
  }

  .cultist-icon {
    color: var(--cultist);
    text-shadow: 0px 0px 2px rgba(255, 255, 255, 0.5);
  }

  .elder-thing-icon {
    color: var(--elder-thing);
    text-shadow: 0px 0px 2px rgba(255, 255, 255, 0.5);
  }

  button {
    width: 1em;
    border-radius: 0;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  button:hover {
    background: rgba(0, 0, 0, 0.2);
  }
}

.close-button{
  background: var(--button);
  &:hover {
    background: var(--button-highlight);
  }
}

.in-the-shadows {
  background: rgba(0, 0, 0, 0.5);
  padding: 10px;
  min-width: 10vw;
  border-radius: 1vw;
  display: flex;
  flex-direction: row;
  gap: 10px;
  &:deep(.location) {
    min-width: unset;
  }
  &:deep(.location-container) {
    grid-template-columns: 0 1fr 0;
    min-height: unset;
    grid-column-gap: 0;
  }
}

.concealed-card {
  width: calc(var(--card-width) * 0.55);
  border-radius: 3px;
}

.concealed-card-stack {
  position: relative;
  display: grid;
  grid-template-areas: "stack";
  align-items: center;
  justify-items: center;
  > * {
    grid-area: stack;
    justify-self: center;
  }
  .count {
    align-self: start;
    margin-top: 5%;
    font-weight: bold;
    border-radius: 100vw;
    background-color: rgba(255, 255, 255, 0.6);
    width: auto;
    height: 1.2em;
    display: grid;
    aspect-ratio: 1 / 1;
    text-align: center;
    align-content: center;
    justify-content: center;
    pointer-events: none;
  }
}

.concealed-card-group {
  display: grid;
  place-content: center;
}
</style>
