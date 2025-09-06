<script lang="ts" setup>
import { ref, onMounted, onBeforeUnmount } from 'vue'
import { IsMobile } from '@/arkham/isMobile';

const draggable = ref<HTMLElement | null>(null)
const emit = defineEmits(['minimize'])
const isMinimized = ref(false)
const initialMouseX = ref(0)
const initialMouseY = ref(0)
const initialLeft = ref(0)
const initialTop = ref(0)
const { isMobile } = IsMobile()

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

function clamp(v: number, min: number, max: number) {
  return Math.min(Math.max(v, min), max)
}

function drag(e: PointerEvent) {
  const target = e.target as HTMLElement | null
  if (!target || target.closest('.minimize-btn')) return
  if (!target.closest('header') || isMinimized.value) return

  const el = draggable.value
  if (!el) return

  e.preventDefault()
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

    const windowWidth = window.innerWidth
    const windowHeight = window.innerHeight

    const modalWidth = el.offsetWidth
    const modalHeight = el.offsetHeight

    const maxLeft = Math.max(0, windowWidth - modalWidth)
    const maxTop  = Math.max(0, windowHeight - modalHeight)

    const newLeft = clamp(initialLeft.value + deltaX, 0, maxLeft)
    const newTop  = clamp(initialTop.value + deltaY, 0, maxTop)

    el.style.left = `${newLeft}px`
    el.style.top = `${newTop}px`
  })
}

function stopDrag() {
  const el = draggable.value
  if (el) {
    el.style.transition = ''
    el.removeEventListener('pointermove', elementDrag as any)
    if (dragPointerId !== null) {
      try { el.releasePointerCapture(dragPointerId) } catch {}
    }
  }
  dragPointerId = null
  if (raf) {
    cancelAnimationFrame(raf)
    raf = 0
  }
  document.body.style.userSelect = ''
}

function minimize() {
  const el = draggable.value
  if (!el) return

  if (!isMinimized.value) {
    // Minimizing
    isMinimized.value = true
    emit('minimize', true)

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
    void el.offsetWidth
    el.classList.add('minimized')
    el.style.right = '20px'
    el.style.bottom = '20px'
    el.style.left = ''
    el.style.top = ''
    el.style.width = 'fit-content'
    el.style.height = 'fit-content'
  } else {
    // Restoring
    isMinimized.value = false
    emit('minimize', false)

    el.style.right = ''
    el.style.bottom = ''
    el.style.left = `${originalLeft.value}px`
    el.style.top = `${originalTop.value}px`
    el.style.width = `${originalWidth.value}px`
    el.style.height = `${originalHeight.value}px`

    el.addEventListener('transitionend', function handler() {
      const node = draggable.value
      if (node) {
        node.classList.remove('minimized')
        node.style.position = 'absolute'
        node.style.width = ''
        node.style.height = ''
      }
      node?.removeEventListener('transitionend', handler)
    })
  }
}

onMounted(() => {
  const el = draggable.value
  if (el) {
    const rect = el.getBoundingClientRect()
    const windowWidth = window.innerWidth
    const windowHeight = window.innerHeight

    const initialLeftPosition = (windowWidth - rect.width) / 2
    let initialTopPosition = (windowHeight - rect.height) / 2
    if (isMobile.value) {
      initialTopPosition = 60
    }
    el.style.left = `${initialLeftPosition}px`
    el.style.top = `${initialTopPosition}px`
    el.style.position = 'absolute'
    el.style.transform = 'none'
  }
})

onBeforeUnmount(() => {
  const el = draggable.value
  if (el) {
    el.removeEventListener('pointermove', elementDrag as any)
  }
  document.body.style.userSelect = ''
  if (raf) {
    cancelAnimationFrame(raf)
    raf = 0
  }
})
</script>

<template>
  <Teleport to="#modal">
    <div class="draggable" ref="draggable">
      <header @pointerdown="drag" @click="isMinimized && minimize()">
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

<style lang="scss" scoped>
.draggable {
  position: absolute;
  background: rgba(94, 123, 115, 0.5);
  border-radius: 16px;
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
  border: 1px solid rgba(255, 255, 255, 0.3);
  z-index: 10;
  overflow: hidden;
  transition: left .25s ease, top .25s ease, right .25s ease, bottom .25s ease, width .25s ease, height .25s ease, transform .25s ease;
  backdrop-filter: blur(5px);
  -webkit-backdrop-filter: blur(5px);
  transition-behavior: allow-discrete;
  width: clamp(300px, 50vw, 80%);
  max-width: fit-content;
  max-height: 80%;
  display: flex;
  flex-direction: column;

  @media (max-width: 768px) {
    max-width: 100%;
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
    gap: 10px;
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
  }
}
</style>
