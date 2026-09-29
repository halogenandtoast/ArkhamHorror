<script setup lang="ts">
import { sessionState } from '@/session'
import SignedOut from '@/views/SignedOut.vue'
</script>

<template>
  <div v-if="sessionState === 'loading'" class="screen"><p class="waiting">Loading…</p></div>
  <SignedOut v-else-if="sessionState !== 'signedIn'" />
  <RouterView v-else />
</template>
