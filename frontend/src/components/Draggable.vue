<script lang="ts" setup>
import { ref } from 'vue';

const posX = ref(0)
const posY = ref(0)

function drag(e) {
  e.preventDefault()
  posX.value = e.clientX
  posY.value = e.clientY
  document.onmouseup = stopDrag
  document.onmousemove = elementDrag(e.target)
}

function stopDrag() {
  document.onmouseup = null
  document.onmousemove = null
}

function elementDrag(el) {
  return (e) => {
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
  <div class="draggable" @mousedown="drag">
    <header><slot name="handle"></slot></header>
    <slot></slot>
  </div>
</template>

<style lang="scss">
.draggable > header {
  pointer-events: none;
  background: rgba(0 0 0 / 50%);
}

.draggable {
  position: absolute;
  width: 80%;
  top: 50%;
  left: 50%;
  background: hsl(150.9 13.6% 52.4% / 80%);
  transform: translateX(-50%) translateY(-50%);

  background: rgba(94,123,115,0.5);
  border-radius: 16px;
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
  backdrop-filter: blur(5px);
  -webkit-backdrop-filter: blur(5px);
  border: 1px solid rgba(255, 255, 255, 0.3);
  z-index: 1000000;
}

.draggable header {
  background: rgba(0 0 0 / 50%);
  border-bottom: 1px solid rgba(255 255 255 / 40%);
  text-transform: uppercase;
  border-radius: 16px 16px 0 0;
  font-size: 0.8em;
  color: white;
  text-align: center;
  margin-bottom: 10px;
  padding: 5px;
  > * {
    padding: 0;
    margin: 0;
  }
}

</style>
