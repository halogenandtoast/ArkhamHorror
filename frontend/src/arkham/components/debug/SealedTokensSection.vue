<script lang="ts" setup>
/* A card's sealed chaos tokens: each token is its own release button, pool
 * tokens such as bless and curse can instead go out of play. Always shown, even
 * with nothing sealed, so sealing is one click away. */
import { ref } from 'vue'
import { useDebug } from '@/arkham/debug'
import type { Game } from '@/arkham/types/Game'
import { chaosTokenImage, type ChaosToken } from '@/arkham/types/ChaosToken'
import {
  isPooledFace,
  releaseToPoolMessage,
  unsealMessage,
  type SealableTarget,
} from '@/arkham/debugSeals'
import SealTokenPicker from '@/arkham/components/debug/SealTokenPicker.vue'

const props = defineProps<{
  game: Game
  tokens: ChaosToken[]
  target: SealableTarget
}>()

const debug = useDebug()
const picking = ref(false)

const release = (token: ChaosToken) => debug.send(props.game.id, unsealMessage(token))
const releaseToPool = (token: ChaosToken) => debug.send(props.game.id, releaseToPoolMessage(token))
</script>

<template>
  <section class="sealed-section">
    <span class="sealed-section__label">{{ $t('debug.seals.sealedTokens') }}</span>
    <p v-if="tokens.length === 0" class="sealed-section__none">{{ $t('debug.seals.none') }}</p>
    <div v-else class="sealed-section__row">
      <div v-for="token in tokens" :key="token.id" class="sealed-section__token">
        <button
          type="button"
          class="sealed-token"
          v-tooltip="$t('debug.seals.release')"
          @click="release(token)"
        >
          <img :src="chaosTokenImage(token.face)" :alt="token.face" />
          <span class="sealed-token__x" aria-hidden="true">×</span>
        </button>
        <button
          v-if="isPooledFace(token.face)"
          type="button"
          class="sealed-section__pool"
          v-tooltip="$t('debug.seals.releaseToPool')"
          @click="releaseToPool(token)"
        >{{ $t('debug.seals.toPool') }}</button>
      </div>
    </div>
    <div class="sealed-section__row">
      <button type="button" class="sealed-section__seal" @click="picking = true">{{ $t('debug.seals.seal') }}</button>
    </div>
    <SealTokenPicker v-if="picking" :game="game" :target="target" @close="picking = false" />
  </section>
</template>

<style scoped>
.sealed-section {
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 10px 12px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  color: var(--text);
  font-size: 0.85rem;
  font-weight: normal;
}

.sealed-section__label {
  font-family: teutonic, sans-serif;
  font-size: 0.95rem;
  letter-spacing: 0.04em;
  color: rgba(255, 255, 255, 0.72);
}

.sealed-section__none {
  margin: 0;
  color: rgba(255, 255, 255, 0.55);
}

.sealed-section__row {
  display: flex;
  flex-flow: row wrap;
  align-items: center;
  gap: 6px;
}

.sealed-section__token {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 3px;
}

.sealed-section button {
  min-width: 0;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: var(--background-dark);
  color: var(--text);
  font-size: 0.8rem;
  padding: 7px 10px;
  cursor: pointer;
  transition: background-color 0.12s ease, border-color 0.12s ease;
}

.sealed-section button:hover {
  background: var(--button);
  border-color: var(--select);
}

.sealed-section button.sealed-section__pool {
  padding: 1px 6px;
  font-size: 0.7rem;
}

/* A sealed token doubles as its own release button: the × only shows on hover so
   the row still reads as the card's sealed pool at a glance. */
.sealed-section button.sealed-token {
  position: relative;
  padding: 3px;
  display: grid;
  place-items: center;
  border-radius: 999px;
}

.sealed-token img {
  width: 30px;
  height: auto;
  display: block;
}

.sealed-token__x {
  position: absolute;
  inset: 0;
  display: grid;
  place-items: center;
  border-radius: 999px;
  background: rgba(0, 0, 0, 0.6);
  color: #fff;
  font-size: 1.1rem;
  font-weight: 700;
  line-height: 1;
  opacity: 0;
  transition: opacity 0.12s ease;
}

.sealed-token:hover .sealed-token__x {
  opacity: 1;
}
</style>
