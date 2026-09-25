<script setup lang="ts">
import { watchEffect } from 'vue'
import { TOKEN } from '@/game/util'
import { sessionState } from '@/session'
import SignedOut from '@/views/SignedOut.vue'

// the lead player's tab wears the first-player token, which lives on the asset host
watchEffect(() => {
  document.documentElement.style.setProperty('--first-player-img', `url('${TOKEN('first-player-active')}')`)
})
</script>

<template>
  <div v-if="sessionState === 'loading'" class="screen"><p class="waiting">Loading…</p></div>
  <SignedOut v-else-if="sessionState !== 'signedIn'" />
  <RouterView v-else />
</template>
