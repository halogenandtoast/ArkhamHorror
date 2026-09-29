<script setup lang="ts">
import { computed, ref } from 'vue'
import { useGame } from '@/game/context'
import MythosTok from '@/game/MythosTok.vue'

const ctx = useGame()
const g = computed(() => ctx.game.value!)

/* Two drawers, one behind the other: the tab you pick slides its row of tokens
into the panel. Drawn is what the mythos just did, so it opens on that. */
const side = ref<'drawn' | 'cup'>('drawn')
</script>

<template>
  <section>
    <h2>Mythos</h2>
    <div id="codex" class="mythos">
      <div class="mythos-tabs" role="tablist" aria-label="Mythos tokens">
        <button
          class="mythos-tab"
          role="tab"
          :class="{ on: side === 'drawn' }"
          :aria-selected="side === 'drawn'"
          @click="side = 'drawn'"
        >
          Drawn <b>{{ g.drawnTokens.length }}</b>
        </button>
        <button
          class="mythos-tab"
          role="tab"
          :class="{ on: side === 'cup' }"
          :aria-selected="side === 'cup'"
          @click="side = 'cup'"
        >
          Cup <b>{{ g.cup.length }}</b>
        </button>
      </div>
      <div class="mythos-drawer">
        <div class="mythos-track" :class="{ 'show-cup': side === 'cup' }">
          <div class="cup">
            <MythosTok v-for="(t, k) in g.drawnTokens" :key="`d${k}`" :token="t" :size="24" />
            <span v-if="!g.drawnTokens.length" class="waiting">Nothing drawn yet</span>
          </div>
          <div class="cup">
            <MythosTok v-for="(t, k) in g.cup" :key="`c${k}`" :token="t" :size="24" />
            <span v-if="!g.cup.length" class="waiting">The cup is empty</span>
          </div>
        </div>
      </div>
    </div>
  </section>
</template>
