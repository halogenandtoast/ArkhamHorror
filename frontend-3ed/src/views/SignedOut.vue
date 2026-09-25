<script setup lang="ts">
import { getToken } from '@/authToken'
import { loadSession, sessionError, sessionState, signInUrl, signOut } from '@/session'

const hasToken = () => !!getToken()
</script>

<template>
  <header>
    <h1>Arkham Horror 3e</h1>
  </header>
  <div class="screen signed-out">
    <template v-if="sessionState === 'error'">
      <h2>Can't reach the server</h2>
      <p class="sub err">{{ sessionError }}</p>
      <button class="primary" @click="loadSession">Try again</button>
    </template>
    <template v-else>
      <h2>Sign in to play</h2>
      <p class="sub">Third edition games use your arkhamhorror.app account. Sign in on the main site, then come back here.</p>
      <a class="sign-in" :href="signInUrl"><span class="button-like primary">Sign in on arkhamhorror.app</span></a>
    </template>
    <p v-if="hasToken()" class="sub">
      Signed in as someone else, or stuck? <button @click="signOut">Sign out</button>
    </p>
  </div>
</template>
