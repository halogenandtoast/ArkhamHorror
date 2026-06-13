<script lang="ts" setup>
import { nextTick, ref, onMounted, onBeforeUnmount, useId } from 'vue'

const props = withDefaults(defineProps<{
  centerInSelector?: string
  avoidSelector?: string
  avoidPadding?: number
  clickThroughChrome?: boolean
}>(), { avoidPadding: 8, clickThroughChrome: false })

const id = useId()
const draggable = ref<HTMLElement | null>(null)
const isMinimized = ref(false)
const initialMouseX = ref(0)
const initialMouseY = ref(0)
const initialLeft = ref(0)
const initialTop = ref(0)
const viewportMargin = 16
const anchorX = ref(0)
const anchorY = ref(0)

// Variables to store the modal's position and size before minimizing
const originalLeft = ref(0)
const originalTop = ref(0)
const originalWidth = ref(0)
const originalHeight = ref(0)

// drag state
let raf = 0
let lastClientX = 0
let lastClientY = 0
let dragPointerId: number | null = null
let resizeObserver: ResizeObserver | null = null
const hasBeenDragged = ref(false)
const isDragging = ref(false)

function clamp(v: number, min: number, max: number) {
  return Math.min(Math.max(v, min), max)
}

function viewportBounds(el: HTMLElement) {
  const windowWidth = window.innerWidth
  const windowHeight = window.innerHeight
  const modalWidth = el.offsetWidth
  const modalHeight = el.offsetHeight

  return {
    maxLeft: Math.max(viewportMargin, windowWidth - modalWidth - viewportMargin),
    maxTop: Math.max(viewportMargin, windowHeight - modalHeight - viewportMargin),
    modalWidth,
    modalHeight,
    windowWidth,
    windowHeight,
  }
}

function setAnchorFromRect(rect: DOMRect) {
  anchorX.value = rect.left + rect.width / 2
  anchorY.value = rect.top + rect.height / 2
}

function preferredBounds() {
  if (props.centerInSelector) {
    const el = document.querySelector(props.centerInSelector) as HTMLElement | null
    const rect = el?.getBoundingClientRect()
    if (rect && rect.width > 0 && rect.height > 0) return rect
  }

  return new DOMRect(0, 0, window.innerWidth, window.innerHeight)
}

function setAnchorToViewportCenter() {
  const bounds = preferredBounds()
  anchorX.value = bounds.left + bounds.width / 2
  anchorY.value = bounds.top + bounds.height / 2
}

function overlapArea(a: { left: number; top: number; right: number; bottom: number }, b: DOMRect) {
  const padding = props.avoidPadding
  const left = Math.max(a.left, b.left - padding)
  const right = Math.min(a.right, b.right + padding)
  const top = Math.max(a.top, b.top - padding)
  const bottom = Math.min(a.bottom, b.bottom + padding)

  return Math.max(0, right - left) * Math.max(0, bottom - top)
}

function avoidOverlapPosition(modalWidth: number, modalHeight: number, maxLeft: number, maxTop: number) {
  if (!props.avoidSelector) return null

  const avoided = [...document.querySelectorAll(props.avoidSelector)]
    .map((el) => el.getBoundingClientRect())
    .filter((rect) => rect.width > 0 && rect.height > 0)

  if (avoided.length === 0) return null

  const bounds = preferredBounds()
  const centerLeft = bounds.left + (bounds.width - modalWidth) / 2
  const centerTop = bounds.top + (bounds.height - modalHeight) / 2
  const candidates = [
    [centerLeft, centerTop],
    [bounds.left + viewportMargin, bounds.top + viewportMargin],
    [bounds.right - modalWidth - viewportMargin, bounds.top + viewportMargin],
    [bounds.left + viewportMargin, bounds.bottom - modalHeight - viewportMargin],
    [bounds.right - modalWidth - viewportMargin, bounds.bottom - modalHeight - viewportMargin],
    [centerLeft, bounds.top + viewportMargin],
    [centerLeft, bounds.bottom - modalHeight - viewportMargin],
    [bounds.left + viewportMargin, centerTop],
    [bounds.right - modalWidth - viewportMargin, centerTop],
  ].map(([left, top]) => ({
    left: clamp(left, viewportMargin, maxLeft),
    top: clamp(top, viewportMargin, maxTop),
  }))

  let best: { left: number; top: number; score: number } | null = null
  for (const candidate of candidates) {
    const rect = {
      left: candidate.left,
      top: candidate.top,
      right: candidate.left + modalWidth,
      bottom: candidate.top + modalHeight,
    }
    const overlap = avoided.reduce((total, avoidedRect) => total + overlapArea(rect, avoidedRect), 0)
    const distance = Math.hypot(candidate.left - centerLeft, candidate.top - centerTop)
    const score = overlap * 1000 + distance
    if (!best || score < best.score) best = { ...candidate, score }
  }

  return best
}

function placeModal({ resetAnchor = false } = {}) {
  const el = draggable.value
  if (!el || isMinimized.value || isDragging.value) return

  const { maxLeft, maxTop, modalWidth, modalHeight } = viewportBounds(el)

  if (resetAnchor || !hasBeenDragged.value) {
    setAnchorToViewportCenter()
  }

  // Position from the modal center, not its top edge. This keeps the modal
  // visually oriented around the same point as its contents grow or shrink.
  let nextLeft = clamp(anchorX.value - modalWidth / 2, viewportMargin, maxLeft)
  let nextTop = clamp(anchorY.value - modalHeight / 2, viewportMargin, maxTop)

  if (!hasBeenDragged.value) {
    const avoidedPosition = avoidOverlapPosition(modalWidth, modalHeight, maxLeft, maxTop)
    if (avoidedPosition) {
      nextLeft = avoidedPosition.left
      nextTop = avoidedPosition.top
      anchorX.value = nextLeft + modalWidth / 2
      anchorY.value = nextTop + modalHeight / 2
    }
  }

  el.style.left = `${nextLeft}px`
  el.style.top = `${nextTop}px`
  el.style.position = 'absolute'
  el.style.transform = 'none'
}

function drag(e: PointerEvent) {
  const target = e.target as HTMLElement | null
  if (!target || target.closest('.minimize-btn')) return
  if (!target.closest('header') || isMinimized.value) return

  const el = draggable.value
  if (!el) return

  e.preventDefault()
  isDragging.value = true
  dragPointerId = e.pointerId
  el.setPointerCapture(e.pointerId)

  const rect = el.getBoundingClientRect()
  initialMouseX.value = e.clientX
  initialMouseY.value = e.clientY
  initialLeft.value = rect.left
  initialTop.value = rect.top

  el.style.transform = 'none'
  el.style.transition = 'none'
  document.body.style.userSelect = 'none'

  el.addEventListener('pointermove', elementDrag, { passive: false })
  el.addEventListener('pointerup', stopDrag, { once: true })
}

function elementDrag(e: PointerEvent) {
  const el = draggable.value
  if (!el) return

  e.preventDefault()
  lastClientX = e.clientX
  lastClientY = e.clientY

  if (raf) return
  raf = requestAnimationFrame(() => {
    raf = 0
    const deltaX = lastClientX - initialMouseX.value
    const deltaY = lastClientY - initialMouseY.value

    const { maxLeft, maxTop } = viewportBounds(el)

    const newLeft = clamp(initialLeft.value + deltaX, viewportMargin, maxLeft)
    const newTop  = clamp(initialTop.value + deltaY, viewportMargin, maxTop)

    el.style.left = `${newLeft}px`
    el.style.top = `${newTop}px`
  })
}

function stopDrag() {
  hasBeenDragged.value = true
  const el = draggable.value
  if (el) {
    el.style.transition = ''
    setAnchorFromRect(el.getBoundingClientRect())
    el.removeEventListener('pointermove', elementDrag as any)
    if (dragPointerId !== null) {
      try { el.releasePointerCapture(dragPointerId) } catch {}
    }
  }
  isDragging.value = false
  dragPointerId = null
  if (raf) {
    cancelAnimationFrame(raf)
    raf = 0
  }
  document.body.style.userSelect = ''
}

function minimize() {
  const doMinimize = () => {
    const el = draggable.value
    if (!el) return

    //void el.offsetWidth
    if (!isMinimized.value) {
      // Minimizing

      const rect = el.getBoundingClientRect()
      originalLeft.value = rect.left
      originalTop.value = rect.top
      originalWidth.value = rect.width
      originalHeight.value = rect.height

      el.style.position = 'fixed'
      el.style.left = `${rect.left}px`
      el.style.top = `${rect.top}px`
      el.style.width = `${rect.width}px`
      el.style.height = `${rect.height}px`

      // kick off transition from current rect to bottom-right
      el.classList.add('minimized')
      el.style.right = '20px'
      el.style.bottom = '20px'
      el.style.left = ''
      el.style.top = ''
      el.style.width = 'fit-content'
      el.style.height = 'fit-content'

      isMinimized.value = true
    } else {
      el.style.right = ''
      el.style.bottom = ''
      el.classList.remove('minimized')
      el.style.position = 'absolute'
      el.style.width = `${originalWidth.value}px`
      el.style.height = `${originalHeight.value}px`
      el.style.left = `${originalLeft.value}px`
      el.style.top = `${originalTop.value}px`

      el.style.width = ''
      el.style.height = ''

      // Restoring
      isMinimized.value = false
    }
  }

  if (!document.startViewTransition) {
    doMinimize()
    return
  }

  document.startViewTransition(() => doMinimize())
}

onMounted(async () => {
  const el = draggable.value
  if (el) {
    await nextTick()
    placeModal({ resetAnchor: true })
    moveUp()

    resizeObserver = new ResizeObserver(() => {
      requestAnimationFrame(() => placeModal())
    })
    resizeObserver.observe(el)
    window.addEventListener('resize', handleWindowResize)
  }
})

function handleWindowResize() {
  placeModal()
}

onBeforeUnmount(() => {
  const el = draggable.value
  if (el) {
    el.removeEventListener('pointermove', elementDrag as any)
  }
  resizeObserver?.disconnect()
  window.removeEventListener('resize', handleWindowResize)
  document.body.style.userSelect = ''
  if (raf) {
    cancelAnimationFrame(raf)
    raf = 0
  }
})

function moveUp() {
  const modals = document.querySelectorAll('.draggable')
  modals.forEach(modal => { (modal as HTMLElement).style.zIndex = '99' })

  const el = draggable.value
  if (el) {
    el.style.zIndex = '100'
  }
}
</script>

<template>
  <Teleport to="#modal">
  <div
    @pointerdown="moveUp"
    class="draggable"
    :class="{ 'click-through-chrome': props.clickThroughChrome }"
    ref="draggable"
    :id="id"
    :style="{ 'view-transition-name': id }"
  >
    <header @pointerdown="drag" @click.stop="isMinimized && minimize()">
        <span class="header-title">
          <slot name="handle"></slot>
        </span>
        <button class="minimize-btn" @click.stop="minimize">
          <svg v-if="isMinimized" width="12" height="12" viewBox="0 0 24 24">
            <path d="M12 9l-6 6h12l-6-6z" fill="currentColor" />
          </svg>
          <svg v-else width="12" height="12" viewBox="0 0 24 24">
            <rect y="11" width="24" height="2" fill="currentColor" />
          </svg>
        </button>
      </header>
      <div class="content" v-show="!isMinimized">
        <slot></slot>
      </div>
    </div>
  </Teleport>
</template>

<style scoped>
.draggable {
  position: absolute;
  background: rgba(94, 123, 115, 0.5);
  border-radius: 16px;
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
  border: 1px solid rgba(255, 255, 255, 0.3);
  z-index: 10;
  overflow: hidden;
  backdrop-filter: blur(5px);
  -webkit-backdrop-filter: blur(5px);
  width: clamp(300px, 50vw, 80%);
  max-width: fit-content;
  max-height: calc(100dvh - 32px);
  display: flex;
  flex-direction: column;

  @media (max-width: 768px) {
    max-width: 100%;
  }

  &.click-through-chrome {
    pointer-events: none;

    > .content {
      pointer-events: auto;
    }

    header {
      pointer-events: none;

      .minimize-btn {
        pointer-events: auto;
      }
    }
  }

  &:has(> .content > .settings),
  &:has(> .content > .shortcuts-modal) {
    width: min(640px, 92vw);
    max-width: 92vw;
    background: var(--background);
    border-color: var(--box-border);

    > .content {
      margin: 0;
      border-radius: 0 0 16px 16px;
      background: var(--background);
      overflow: hidden;
      height: auto;
    }
  }

  &:not(.minimized) {
    &:has(p.file) {
      box-shadow: none;
      filter: drop-shadow(2px 4px 6px rgba(0,0,0,0.3));
      overflow: visible;
      background: transparent;
      border: none;
      position: relative;
      :deep(.intro-text) {
        box-shadow: 1px 2px 4px rgba(0,0,0,0.7);
      }
      .content {
        background: none;
        position: relative;
        isolation: isolate;
        &::before {
          mix-blend-mode: darken;
          /* a linear gradiant from transparent to black */
          background: linear-gradient(180deg, rgba(0,0,0,0) 0%, rgba(0,0,0,0.1) 100%);

          content: '';
          inset: 0;
          position: absolute;
          z-index: -1;
        }
        &::after {
          background-color: #89745D;
          content: '';
          inset: 0;
          position: absolute;
          z-index: -2;
          clip-path: polygon(
            0 0,
            calc(100% - 30px) 0,
            calc(100% - 15px) 20px,
            100% 20px,
            100% 100%,
            0 100%
          );
        }
        margin: 0;
        padding: 10px;
        border-top-right-radius: 16px;
      }
      header {
        position: absolute;
        bottom: calc(100% - 1px);
        background-color: #89745D;
        align-items: flex-start;
        width: fit-content;

        .header-title {
          &::before {
            background: linear-gradient(180deg, #DFD2AF, #C4B59C);
            content: '';
            position: absolute;
            inset: 0;
            z-index: -1;
          }
          isolation: isolate;
          position: relative;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.5);
          padding: 5px 10px;
          color: black;
          text-transform: none;
          font-family: Arno, serif;
          width: fit-content;
          text-align: left;
          background: #DFD2AF;
          display: block;
          max-width: fit-content;
          transform: rotate(-1deg)
        }
      }
    }
  }

  &.minimized {
    header .header-title {
      transform: translateX(0);
    }
  }

  header {
    display: flex;
    align-items: center;
    position: relative;
    padding: 5px 10px;
    font-family: Teutonic;
    background: rgba(0, 0, 0, 0.5);
    color: white;
    text-transform: uppercase;
    font-size: 1.2em;
    cursor: move;
    border-radius: 16px 16px 0 0;
    backdrop-filter: blur(10px);
    -webkit-backdrop-filter: blur(10px);

    .header-title {
      flex: 1;
      text-align: center;
      margin: 0;
      margin-inline: 18px;
      pointer-events: none;
      transform: translateX(12px);
    }

    .minimize-btn {
      border: none;
      color: white;
      border-radius: 50%;
      width: min(24px, 2vw);
      height: min(24px, 2vw);
      aspect-ratio: 1;
      display: flex;
      align-items: center;
      justify-content: center;
      align-content: flex-end;
      align-self: flex-end;
      justify-self: flex-end;
      cursor: pointer;
      pointer-events: auto;
      background: #3C4F5A;
      &:hover {
        background: #546E7A;
      }

      svg {
        fill: currentColor;
      }
    }
  }

  .content {
    height: 100%;
    overflow: auto;
    border-radius: 0 0 16px 16px;
    display: flex;
    flex-direction: column;
    margin: 10px;
    &:has(button.close) {
      margin: 0;
    }
    &:has(> .skill-test) {
      margin: 0px;
    }
    &:has(.amount-modal) {
      margin: 0px;
    }
    &:has(.chaos-bag) {
      margin: 0px;
    }
    &:has(.bug-form) {
      margin: 0px;
    }
    &:has(.haunted) {
      margin: 0;
      border-radius: 0 0 16px 16px;
      overflow: hidden;
    }
  }

  &:has(.haunted) {
    background: transparent;
    backdrop-filter: none;
    -webkit-backdrop-filter: none;
    border: 0;
    border-radius: 16px;
    box-shadow: 0 12px 50px rgba(0, 0, 0, 0.85);
    max-width: min(900px, 92vw);
    width: min(900px, 92vw);

    > header {
      border-radius: 16px 16px 0 0;
      background: #0a0d10;
      backdrop-filter: none;
      -webkit-backdrop-filter: none;

      :deep(h1) {
        color: #c9d2a8;
        text-shadow:
          0 0 6px rgba(131, 137, 56, 0.55),
          0 1px 2px rgba(0, 0, 0, 0.9);
      }
    }
  }
}

</style>
