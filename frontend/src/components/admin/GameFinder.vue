<script lang="ts" setup>
import { ref } from 'vue'
import { findGame } from '@/arkham/api'
import GameRow from '@/arkham/components/GameRow.vue'
import type { GameDetailsEntry } from '@/arkham/types/Game'

const dragging = ref(false)
const playerIds = ref<string[]>([])
const errorMsg = ref<string | null>(null)
const gameDetails = ref<GameDetailsEntry | null>(null)

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
      return
    }

    gameDetails.value = await findGame(playerIds.value[0])
  } catch (e: unknown) {
    const message = e instanceof Error ? e.message : String(e)
    errorMsg.value = `Failed to parse JSON: ${message}`
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
  <div class="uploader box">
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
      <strong>Drop JSON here</strong>
      <span>or click to choose a file</span>
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
      <GameRow v-if="gameDetails.tag === 'game'" :key="gameDetails.id" :game="gameDetails" :admin="true" />
      <div v-else class="error">{{ gameDetails.error }}</div>
    </div>
  </div>
</template>

<style scoped>
.uploader {
  display: grid;
  gap: 10px;
}

.dropzone {
  align-items: center;
  border: 1px dashed color-mix(in srgb, var(--spooky-green) 65%, var(--box-border));
  border-radius: 5px;
  color: var(--title);
  cursor: pointer;
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  justify-content: center;
  padding: 18px;
  text-align: center;
  transition: background-color 0.15s ease, border-color 0.15s ease;
}

.dropzone strong {
  color: color-mix(in srgb, var(--spooky-green) 80%, white);
  text-transform: uppercase;
}

.dropzone span {
  opacity: 0.75;
}

.dropzone:hover,
.dropzone.dragging {
  background-color: rgba(255, 255, 255, 0.04);
  border-color: var(--spooky-green);
}

.hidden {
  display: none;
}

.results {
  display: grid;
  gap: 10px;
}

.error {
  background: color-mix(in srgb, var(--delete) 18%, transparent);
  border: 1px solid color-mix(in srgb, var(--delete) 55%, var(--box-border));
  border-radius: 5px;
  color: color-mix(in srgb, var(--delete) 30%, white);
  padding: 10px;
}
</style>
