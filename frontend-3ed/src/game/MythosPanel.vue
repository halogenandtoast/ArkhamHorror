<script setup lang="ts">
import { computed, ref } from 'vue'
import { useGame } from '@/game/context'
import MythosTok from '@/game/MythosTok.vue'

const ctx = useGame()
const g = computed(() => ctx.game.value!)

/* Two drawers side by side: what the mythos drew lies open, and the cup is pulled
shut against the right edge with only its handle showing. Taking the cup's handle
slides it across; taking the other one sends it back. */
const side = ref<'drawn' | 'cup'>('drawn')
</script>

<template>
  <section>
    <h2>Mythos</h2>
    <div id="codex" class="mythos">
      <div class="mythos-drawer">
        <button
          class="mythos-tab"
          :class="{ on: side === 'drawn' }"
          :aria-expanded="side === 'drawn'"
          :title="`${g.drawnTokens.length} drawn`"
          @click="side = 'drawn'"
        >
          Drawn
        </button>
        <div class="cup">
          <MythosTok v-for="(t, k) in g.drawnTokens" :key="`d${k}`" :token="t" :size="24" />
          <span v-if="!g.drawnTokens.length" class="waiting">Nothing drawn yet</span>
        </div>
      </div>
      <div class="mythos-drawer sliding" :class="{ open: side === 'cup' }">
        <button
          class="mythos-tab"
          :class="{ on: side === 'cup' }"
          :aria-expanded="side === 'cup'"
          :title="`${g.cup.length} in the cup`"
          @click="side = 'cup'"
        >
          Cup
        </button>
        <div class="cup">
          <MythosTok v-for="(t, k) in g.cup" :key="`c${k}`" :token="t" :size="24" />
          <span v-if="!g.cup.length" class="waiting">The cup is empty</span>
        </div>
      </div>
    </div>
  </section>
</template>
