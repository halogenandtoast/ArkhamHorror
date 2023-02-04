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
    el.style.top = (el.offsetTop - y) + "px"
    el.style.left = (el.offsetLeft - x) + "px"
  }
}
</script>

<template>
  <div @mousedown="drag">
    <slot></slot>
  </div>
</template>
