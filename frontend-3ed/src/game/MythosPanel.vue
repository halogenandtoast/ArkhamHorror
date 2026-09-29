<script setup lang="ts">
import { computed, ref } from 'vue'
import { useGame } from '@/game/context'
import MythosTok from '@/game/MythosTok.vue'

const ctx = useGame()
const g = computed(() => ctx.game.value!)

// one row of tokens, either side of the cup, rather than a row each
const side = ref<'cup' | 'drawn'>('drawn')
const tokens = computed(() => (side.value === 'cup' ? g.value.cup : g.value.drawnTokens))
</script>

<template>
  <section>
    <h2>Mythos</h2>
    <div id="codex" class="mythos">
      <div class="mythos-switch" role="group" aria-label="Mythos tokens">
        <button :class="{ on: side === 'cup' }" :aria-pressed="side === 'cup'" @click="side = 'cup'">
          In the cup <b>{{ g.cup.length }}</b>
        </button>
        <button :class="{ on: side === 'drawn' }" :aria-pressed="side === 'drawn'" @click="side = 'drawn'">
          Drawn <b>{{ g.drawnTokens.length }}</b>
        </button>
      </div>
      <div class="cup">
        <MythosTok v-for="(t, k) in tokens" :key="k" :token="t" :size="24" />
        <span v-if="!tokens.length" class="waiting">—</span>
      </div>
    </div>
  </section>
</template>
