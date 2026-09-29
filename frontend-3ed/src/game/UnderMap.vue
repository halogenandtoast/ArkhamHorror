<script setup lang="ts">
/* The two rows of cards that belong together: what the codex holds, and what is
for sale. Which column they sit in depends on how much room the table has. */
import CodexPanel from '@/game/CodexPanel.vue'
import Decks from '@/game/Decks.vue'
</script>

<template>
  <div class="under-map">
    <CodexPanel />
    <section>
      <h2>Display</h2>
      <Decks part="display" />
    </section>
  </div>
</template>
