<script setup lang="ts">
import { onMounted, onUnmounted } from 'vue'
import { hideTip, showTip, tip } from '@/game/tooltip'

const target = (e: Event) => (e.target as Element | null)?.closest?.<HTMLElement>('[data-tip]') ?? null
const onOver = (e: Event) => {
  const el = target(e)
  if (el) showTip(el)
  else if (tip.value) hideTip()
}
const onOut = (e: Event) => {
  if (target(e)) hideTip()
}
onMounted(() => {
  document.addEventListener('pointerover', onOver)
  document.addEventListener('pointerout', onOut)
  window.addEventListener('scroll', hideTip, true)
  window.addEventListener('blur', hideTip)
})
onUnmounted(() => {
  document.removeEventListener('pointerover', onOver)
  document.removeEventListener('pointerout', onOut)
  window.removeEventListener('scroll', hideTip, true)
  window.removeEventListener('blur', hideTip)
  hideTip()
})
</script>

<template>
  <div
    v-if="tip"
    class="tip"
    :class="{ below: tip.below }"
    role="tooltip"
    :style="{ left: `${tip.x}px`, top: `${tip.y}px` }"
  >
    <b>{{ tip.title }}</b>
    <span v-if="tip.body">{{ tip.body }}</span>
  </div>
</template>
