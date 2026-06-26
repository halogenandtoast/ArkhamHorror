<script lang="ts" setup>
import { computed, ref, watch, onUnmounted } from 'vue'
import type { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers'

const props = defineProps<{
  game: Game
  playerId: string
}>()

const pending = computed(() => ArkhamGame.damageAssignmentTokens(props.game, props.playerId))

// Between assignment steps the client briefly clears `question` (game "lock"),
// which would make the tokens blip out and back in. Update instantly when there
// are tokens, but defer hiding so a transient lock gap doesn't flicker them.
const tokens = ref(pending.value)
let hideTimer: ReturnType<typeof setTimeout> | null = null

watch(pending, (next) => {
  if (next) {
    if (hideTimer) { clearTimeout(hideTimer); hideTimer = null }
    tokens.value = next
  } else if (!hideTimer) {
    hideTimer = setTimeout(() => { tokens.value = null; hideTimer = null }, 600)
  }
})

onUnmounted(() => { if (hideTimer) clearTimeout(hideTimer) })

const damageToken = imgsrc('damage-token.png')
const horrorToken = imgsrc('horror-token.png')
</script>

<template>
  <transition name="pending-tokens-fade">
    <div
      v-if="tokens && (tokens.damage > 0 || tokens.horror > 0)"
      class="pending-damage-tokens"
      v-tooltip="'Tokens to assign'"
    >
      <span v-if="tokens.damage > 0" class="token-entry">
        <img :src="damageToken" class="token" />
        <span class="count">{{ tokens.damage }}</span>
      </span>
      <span v-if="tokens.horror > 0" class="token-entry">
        <img :src="horrorToken" class="token" />
        <span class="count">{{ tokens.horror }}</span>
      </span>
    </div>
  </transition>
</template>

<style scoped lang="scss">
.pending-damage-tokens {
  display: inline-flex;
  align-items: center;
  gap: 9px;
  padding: 3px 10px;
  background: rgba(12, 12, 16, 0.78);
  border: 1px solid rgba(255, 255, 255, 0.14);
  border-radius: 11px;
  box-shadow: 0 1px 6px rgba(0, 0, 0, 0.5);
  backdrop-filter: blur(2px);
  -webkit-backdrop-filter: blur(2px);
}

.token-entry {
  display: inline-flex;
  align-items: center;
  gap: 4px;

  .token {
    height: 20px;
    width: auto;
    max-width: none;
    flex: none;
    filter: drop-shadow(0 1px 2px rgba(0, 0, 0, 0.5));
  }

  .count {
    color: #f5f5f5;
    font: 600 13px/1 'Noto Sans', sans-serif;
    font-variant-numeric: tabular-nums;
  }
}

.pending-tokens-fade-enter-active,
.pending-tokens-fade-leave-active {
  transition: opacity 0.18s ease;
}

.pending-tokens-fade-enter-from,
.pending-tokens-fade-leave-to {
  opacity: 0;
}
</style>
