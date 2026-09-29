<script setup lang="ts">
import { computed } from 'vue'
import { useGame } from '@/game/context'
import MythosTok from '@/game/MythosTok.vue'

const ctx = useGame()
const g = computed(() => ctx.game.value!)
</script>

<template>
  <section>
    <h2>Mythos</h2>
    <div id="codex">
      <div class="cup">
        <span>Cup ({{ g.cup.length }})</span><MythosTok v-for="(t, k) in g.cup" :key="k" :token="t" :size="24" />
      </div>
      <div class="cup">
        <span>Drawn</span><MythosTok v-for="(t, k) in g.drawnTokens" :key="k" :token="t" :size="24" /><template
          v-if="!g.drawnTokens.length"
          >—</template
        >
      </div>
    </div>
  </section>
</template>
