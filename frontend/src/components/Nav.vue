<template>
  <div id="nav">
    <router-link to="/" class="home-link">Home</router-link>

    <span>
      <template v-if="currentUser">
        Welcome {{currentUser.username}}
        <router-link to="/decks">Decks</router-link>{{' '}}
        <a href="#" @click="logout">Logout</a>
      </template>
      <template v-else>
        <router-link to="/sign-in">Login</router-link>{{' '}}
        <router-link to="/sign-up">Register</router-link>
      </template>
    </span>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { useStore } from 'vuex';
import { User } from '@/types';

export default defineComponent({
  setup() {
    const store = useStore()
    const currentUser = computed<User | null>(() => store.getters.currentUser)
    async function logout() {
      store.dispatch('logout')
    }
    return { currentUser, logout }
  }
})
</script>
