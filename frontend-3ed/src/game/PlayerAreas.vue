<script setup lang="ts">
import { computed } from 'vue'
import { useGame } from '@/game/context'
import PlayerTabs from '@/game/PlayerTabs.vue'

const ctx = useGame()
const g = computed(() => ctx.game.value!)
</script>

<template>
  <section class="rail-players">
    <div id="investigators">
      <PlayerTabs v-if="Object.keys(g.investigators).length" />
      <em v-else>None yet.</em>
    </div>
  </section>
</template>
