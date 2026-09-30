<script setup lang="ts">
import { nextTick, onMounted, onUnmounted, ref, watch } from 'vue'
import { hideTip, placeTip, showTip, tip } from '@/game/tooltip'

const el = ref<HTMLElement | null>(null)
const pos = ref<{ left: number; top: number; arrow: number; below: boolean } | null>(null)

// measure first, then place: the box's own size decides whether it fits above
watch(tip, async (t) => {
  if (!t) return (pos.value = null)
  pos.value = null
  await nextTick()
  const box = el.value?.getBoundingClientRect()
  if (box) pos.value = placeTip(t.anchor, box)
})

const target = (e: Event) => (e.target as Element | null)?.closest?.<HTMLElement>('[data-tip]') ?? null
const onOver = (e: Event) => {
  const t = target(e)
  if (t) showTip(t)
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
    ref="el"
    class="tip"
    :class="{ below: pos?.below, placed: !!pos }"
    role="tooltip"
    :style="
      pos
        ? { left: `${pos.left}px`, top: `${pos.top}px`, '--arrow': `${pos.arrow}px` }
        : { left: '0px', top: '0px', visibility: 'hidden' }
    "
  >
    <b>{{ tip.title }}</b>
    <span v-if="tip.body">{{ tip.body }}</span>
  </div>
</template>
