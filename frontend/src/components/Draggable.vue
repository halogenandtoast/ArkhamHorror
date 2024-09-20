<script lang="ts" setup>
import { ref, onMounted, nextTick } from 'vue'

const draggable = ref<HTMLElement | null>(null)
const isMinimized = ref(false)
const initialMouseX = ref(0)
const initialMouseY = ref(0)
const initialLeft = ref(0)
const initialTop = ref(0)

// Variables to store the modal's position and size before minimizing
const originalLeft = ref(0)
const originalTop = ref(0)
const originalWidth = ref(0)
const originalHeight = ref(0)

function drag(e: MouseEvent) {
  if (
    e.target instanceof HTMLElement &&
    e.target.closest('header') &&
    !isMinimized.value
  ) {
    e.preventDefault()
    const el = draggable.value
    if (!el) return

    const rect = el.getBoundingClientRect()
    initialMouseX.value = e.clientX
    initialMouseY.value = e.clientY
    initialLeft.value = rect.left
    initialTop.value = rect.top

    // Remove any transforms
    el.style.transform = 'none'

    el.style.transition = 'none' // Disable transitions during drag
    document.addEventListener('mousemove', elementDrag)
    document.addEventListener('mouseup', stopDrag)
  }
}

function elementDrag(e: MouseEvent) {
  const el = draggable.value
  if (!el) return
  e.preventDefault()
  const deltaX = e.clientX - initialMouseX.value
  const deltaY = e.clientY - initialMouseY.value

  let newLeft = initialLeft.value + deltaX
  let newTop = initialTop.value + deltaY

  // Get window dimensions
  const windowWidth = window.innerWidth
  const windowHeight = window.innerHeight

  // Get modal dimensions
  const modalWidth = el.offsetWidth
  const modalHeight = el.offsetHeight

  // Ensure the modal stays within the window bounds
  if (newLeft < 0) {
    newLeft = 0
  } else if (newLeft + modalWidth > windowWidth) {
    newLeft = windowWidth - modalWidth
  }

  if (newTop < 0) {
    newTop = 0
  } else if (newTop + modalHeight > windowHeight) {
    newTop = windowHeight - modalHeight
  }

  el.style.left = `${newLeft}px`
  el.style.top = `${newTop}px`
}

function stopDrag() {
  const el = draggable.value
  if (el) {
    // Re-enable transitions after drag
    el.style.transition = ''
  }
  document.removeEventListener('mousemove', elementDrag)
  document.removeEventListener('mouseup', stopDrag)
}

async function minimize() {
  const el = draggable.value
  if (!el) return

  if (!isMinimized.value) {
    // Minimizing
    isMinimized.value = true

    // Save the original position and size
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

    // Force reflow to apply the current styles before transition
    void el.offsetWidth

    // Apply minimized styles
    el.classList.add('minimized')
    el.style.width = 'fit-content'                    // Adjust as needed
    el.style.height = 'fit-content'                    // Adjust as needed

    await nextTick()

    const minimizedRect = el.getBoundingClientRect()

    el.style.left = `calc(100% - ${minimizedRect.width}px - 20px)` // Adjust as needed
    el.style.top = `calc(100% - ${minimizedRect.height}px - 20px)`   // Adjust as needed
  } else {
    // Restoring
    isMinimized.value = false

    // Restore original styles
    el.style.left = `${originalLeft.value}px`
    el.style.top = `${originalTop.value}px`
    el.style.width = `${originalWidth.value}px`
    el.style.height = `${originalHeight.value}px`

    // Remove minimized class after transition
    el.addEventListener('transitionend', function handler() {
      const el = draggable.value
      if (el) {
        el.classList.remove('minimized')
        el.style.position = 'absolute'
        el.style.width = ''
        el.style.height = ''
      }
      el?.removeEventListener('transitionend', handler)
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
    const initialTopPosition = (windowHeight - rect.height) / 2

    el.style.left = `${initialLeftPosition}px`
    el.style.top = `${initialTopPosition}px`
    el.style.position = 'absolute'
    el.style.transform = 'none' // Remove initial transform
  }
})
</script>

<template>
  <Teleport to="#modal">
    <div class="draggable" ref="draggable">
      <header @mousedown="drag" @click="isMinimized && minimize()">
        <span class="header-title">
          <slot name="handle"></slot>
        </span>
        <button class="minimize-btn" @click.stop="minimize">
          <svg v-if="isMinimized" width="12" height="12" viewBox="0 0 24 24">
            <!-- macOS-style Maximize Icon -->
            <path d="M12 9l-6 6h12l-6-6z" fill="currentColor" />
          </svg>         <!-- Minimize SVG Icon -->
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
  transition: all 0.3s ease; /* Animate all properties */
  backdrop-filter: blur(5px);
  -webkit-backdrop-filter: blur(5px); /* Safari support */
  transition-behavior: allow-discrete;
  max-width: 50%;

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
    backdrop-filter: blur(10px); /* Glassmorphism blur */
    -webkit-backdrop-filter: blur(10px); /* Safari support */

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
      width: 24px;
      height: 24px;
      display: flex;
      align-items: center;
      justify-content: center;
      cursor: pointer;
      pointer-events: auto;
      background: #3C4F5A; /* Updated button background color */
      /* ... */
      &:hover {
        background: #546E7A; /* Slightly lighter on hover */
      }

      svg {
        fill: currentColor;
      }
    }
  }

  .content {
    overflow: auto;
    border-radius: 0 0 16px 16px;
  }
}
</style>
