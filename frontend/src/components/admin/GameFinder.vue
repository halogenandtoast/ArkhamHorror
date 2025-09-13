<script lang="ts" setup>
import { ref } from 'vue'
import { findGame } from '@/arkham/api'
import GameRow from '@/arkham/components/GameRow.vue'
import type { GameDetails } from '@/arkham/types/Game'

const dragging = ref(false)
const playerIds = ref<string[]>([])
const errorMsg = ref<string | null>(null)
const gameDetails = ref<GameDetails | null>(null)

function onDragOver(e: DragEvent) {
  e.preventDefault()
  dragging.value = true
}

function onDragLeave() {
  dragging.value = false
}

function onDrop(e: DragEvent) {
  e.preventDefault()
  dragging.value = false
  const file = e.dataTransfer?.files?.[0]
  if (file) processFile(file)
}

function openPicker(input: HTMLInputElement | null) {
  input?.click()
}

function onPick(e: Event) {
  const input = e.target as HTMLInputElement
  const file = input.files?.[0]
  if (file) processFile(file)
  // reset so picking the same file again retriggers change
  if (input) input.value = ''
}

async function processFile(file: File) {
  errorMsg.value = null
  playerIds.value = []
  try {
    const text = await file.text()
    const json = JSON.parse(text)
    const ids = collectPlayerIds(json)
    playerIds.value = Array.from(new Set(ids))
    if (playerIds.value.length === 0) {
      errorMsg.value = 'No playerIds found.'
    }

    gameDetails.value = await findGame(playerIds.value[0])

  } catch (e: any) {
    errorMsg.value = `Failed to parse JSON: ${e?.message ?? e}`
  }
}

// Walk the whole JSON tree and collect any values where key === "playerId"
function collectPlayerIds(node: unknown): string[] {
  const out: string[] = []
  const stack: unknown[] = [node]
  while (stack.length) {
    const cur = stack.pop()
    if (Array.isArray(cur)) {
      for (const v of cur) stack.push(v)
    } else if (cur && typeof cur === 'object') {
      for (const [k, v] of Object.entries(cur as Record<string, unknown>)) {
        if (k === 'playerId' && typeof v === 'string') out.push(v)
        stack.push(v)
      }
    }
  }
  return out
}
</script>

<template>
  <div class="uploader">
    <div
      class="dropzone"
      :class="{ dragging }"
      @dragover="onDragOver"
      @dragleave="onDragLeave"
      @drop="onDrop"
      @click="openPicker($refs.file as HTMLInputElement)"
      role="button"
      tabindex="0"
    >
      <p><strong>Drop your JSON here</strong> or click to choose a file</p>
      <input
        ref="file"
        type="file"
        accept="application/json,.json"
        class="hidden"
        @change="onPick"
      />
    </div>

    <div v-if="errorMsg" class="error">{{ errorMsg }}</div>

    <div v-if="gameDetails" class="results">
      <GameRow :key="gameDetails.id" :game="gameDetails" :admin="true" />
    </div>
  </div>
</template>

<style scoped>
.uploader { display: grid; gap: 1rem; margin: 10px; }
.dropzone {
  border: 2px dashed #888;
  border-radius: 12px;
  padding: 2rem;
  text-align: center;
  cursor: pointer;
  transition: border-color .2s, background-color .2s;
}
.dropzone.dragging {
  border-color: #4b9cff;
  background-color: rgba(75,156,255,.08);
}
.hidden { display: none; }
.results ul {
  margin: 0;
  padding-left: 1.25rem;
  line-height: 1.6;
}
.error { color: #c0392b; }
</style>
