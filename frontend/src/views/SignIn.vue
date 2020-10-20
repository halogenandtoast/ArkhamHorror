<template>
  <form @submit.prevent="authenticate">
    <div>
      <input
        v-model="credentials.email"
        type="email"
        placeholder="Email"
      />
    </div>
    <div>
      <input
        v-model="credentials.password"
        type="password"
        placeholder="Password"
      />
    </div>
    <div>
      <button>Submit</button>
    </div>
  </form>
</template>

<script lang="ts">
import { useStore } from 'vuex'
import { useRoute, useRouter } from 'vue-router'
import { defineComponent, reactive } from 'vue'
import { Credentials } from '../types'

export default defineComponent({
  setup() {
    const store = useStore()
    const route = useRoute()
    const router = useRouter()
    const credentials: Credentials = reactive({
      email: '',
      password: '',
    })

    async function authenticate() {
      await store.dispatch('authenticate', credentials)
      const { nextUrl } = route.query
      if (nextUrl) {
        router.push({ path: nextUrl as string })
      } else {
        router.push({ path: '/' })
      }
    }

    return { credentials, authenticate }
  }
})
</script>
