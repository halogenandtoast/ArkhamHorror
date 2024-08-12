<script lang="ts" setup>
import { ref } from 'vue'

const posX = ref(0)
const posY = ref(0)

function drag(e: MouseEvent) {
  if (e.target instanceof HTMLElement && e.target.tagName === "HEADER") {
    const parent = e.target.parentElement
    if(parent) {
      e.preventDefault()
      posX.value = e.clientX
      posY.value = e.clientY
      document.onmouseup = stopDrag
      document.onmousemove = elementDrag(parent)
    }
  }
}

function stopDrag() {
  document.onmouseup = null
  document.onmousemove = null
}

function elementDrag(el: HTMLElement) {
  return (e: MouseEvent) => {
    e.preventDefault()
    const x = posX.value - e.clientX
    const y = posY.value - e.clientY
    posX.value = e.clientX
    posY.value = e.clientY
    if (el.offsetTop - (el.offsetHeight / 2) - y > 0) {
      if (el.offsetTop + (el.offsetHeight / 2) - y < document.body.offsetHeight) {
        el.style.top = (el.offsetTop - y) + "px"
      }
    }
    if (el.offsetLeft - (el.offsetWidth / 2) - x > 0) {
      if (el.offsetLeft + (el.offsetWidth / 2) - x < document.body.offsetWidth) {
        el.style.left = (el.offsetLeft - x) + "px"
      }
    }
  }
}
</script>

<template>
  <Teleport to="#modal">
    <div class="draggable">
      <header @mousedown="drag"><slot name="handle"></slot></header>
      <slot></slot>
    </div>
  </Teleport>
</template>

<style lang="scss" scoped>
.draggable > header {
  background: rgba(0 0 0 / 50%);
}

@keyframes fadeIn {
  from {
    opacity: 0;
  }
  to {
    opacity: 1;
  }
}

.draggable {
  animation: fadeIn 0.3s;
  position: absolute;
  width: fit-content;
  max-width: 70%;
  min-width: max-content;
  top: 50%;
  left: 50%;
  overflow: hidden;
  transform: translateX(-50%) translateY(-50%);

  background: hsl(0.0 100% 100% / 30%);
  border-radius: 16px;
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
  backdrop-filter: blur(20px);
  -webkit-backdrop-filter: blur(20px);
  border: 1px solid rgba(255, 255, 255, 0.3);
  z-index: 10;

  &:deep(header) {
    text-transform: uppercase;
    border-radius: 16px 16px 0 0;
    font-size: 1.2em;
    color: white;
    text-align: center;
    padding: 5px;
    font-family: Teutonic;
    > * {
      padding: 0;
      margin: 0;
      pointer-events: none;
    }
  }
  &:deep(button) {
    width: 100%;
    background: var(--button-2);
    transition: background-color 0.3s linear;
    border-radius: 0;
    margin-bottom: 0;
    &:hover {
      background: var(--button-2-highlight);
    }
  }
}


</style>
