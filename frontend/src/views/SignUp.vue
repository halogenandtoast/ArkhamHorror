<script lang="ts" setup>
import { ref, reactive } from 'vue'
import { useUserStore } from '@/stores/user'
import { useRoute, useRouter } from 'vue-router'
import type { Registration } from '@/types'

const route = useRoute()
const router = useRouter()
const store = useUserStore()
const registration = reactive<Registration>({
  username: '',
  email: '',
  password: '',
})
const signUpError = ref<string|null>(null)

async function register() {
  signUpError.value = null
  try {
    await store.register(registration)
    const { nextUrl } = route.query
    if (nextUrl) {
      router.push({ path: nextUrl as string })
    } else {
      router.push({ path: '/' })
    }
  } catch {
    signUpError.value = "Username or Email already taken"
  }
}
</script>

<template>
  <form @submit.prevent="register">
    <header><i class="secret"></i></header>
    <section>
      <div>
        <input
          v-model="registration.username"
          type="text"
          placeholder="Username"
        />
      </div>
      <div>
        <input
          v-model="registration.email"
          type="email"
          placeholder="Email"
        />
      </div>
      <div>
        <input
          v-model="registration.password"
          type="password"
          placeholder="Password"
        />
      </div>
      <div>
        <button>Register</button>
      </div>
    </section>
  </form>
</template>

<style scoped lang="scss">
form {
  margin: 0 auto;
  margin-top: 10vh;
  width: 50vw;
  max-width: 400px;
}

section {
  background-color: #15192C;
  border-radius: 3px;
  padding: 10px;
}

header {
  text-align: center;
}

input {
  outline: 0;
  border: 1px solid #000;
  padding: 15px;
  background: #F2F2F2;
  width: 100%;
  box-sizing: border-box;
  margin-bottom: 10px;
}

button {
  outline: 0;
  padding: 15px;
  background: #6E8640;
  text-transform: uppercase;
  color: white;
  border: 0;
  width: 100%;
  &:hover {
    background: darken(#6E8640, 7%);
  }
}

i.secret {
  font-family: 'Arkham';
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  font-size: 5em;
  color: #15192C;
  -webkit-font-smoothing: antialiased;
  position: relative;

  &:before {
    font-family: "Arkham";
    content: "\0048";
  }
}
</style>
