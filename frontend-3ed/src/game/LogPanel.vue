<script setup lang="ts">
import { computed, nextTick, onMounted, ref, watch } from 'vue'
import { useGame } from '@/game/context'
import LogEntry from '@/game/LogEntry.vue'

const ctx = useGame()
const lines = computed(() => ctx.game.value?.log ?? [])
const list = ref<HTMLElement | null>(null)

const scrollLog = () => {
  const l = list.value
  if (l) l.scrollTop = l.scrollHeight
}

let first = true
watch(
  () => lines.value.length,
  async (n) => {
    const l = list.value
    const atBottom = !l || l.scrollHeight - l.scrollTop - l.clientHeight < 40
    // everything logged before we arrived counts as read
    if (first) ctx.logSeen.value = n
    first = false
    if (ctx.logOpen.value) ctx.logSeen.value = n
    await nextTick()
    if (atBottom || ctx.logOpen.value) scrollLog()
  },
  { immediate: true },
)
watch(ctx.logOpen, (open) => {
  if (open) void nextTick(scrollLog)
})
onMounted(scrollLog)
</script>

<template>
  <aside id="logPanel" class="log-panel" aria-label="Game log">
    <div class="log-head">
      <h2>Game log</h2>
      <button class="log-close" aria-label="Close log" @click="ctx.toggleLog(false)">✕</button>
    </div>
    <ol id="log" ref="list" class="log-list">
      <LogEntry v-for="(line, k) in lines" :key="k" :line="line" />
      <li v-if="!lines.length" class="log-empty">Nothing has happened yet.</li>
    </ol>
  </aside>
  <div id="logBackdrop" class="log-backdrop" @click="ctx.toggleLog(false)"></div>
</template>
