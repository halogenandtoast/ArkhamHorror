<script lang="ts" setup>
import { useStore } from 'vuex'
import { useRoute, useRouter } from 'vue-router'
import { reactive } from 'vue'
import { Credentials } from '../types'

const store = useStore()
const route = useRoute()
const router = useRouter()
const credentials = reactive<Credentials>({
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
</script>

<template>
  <form @submit.prevent="authenticate">
    <header><i class="secret"></i></header>
    <section>
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
        <button>Log in</button>
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
  speak: none;
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  font-size: 5em;
  color: #16192B;
  -webkit-font-smoothing: antialiased;
  position: relative;

  &:before {
    font-family: "Arkham";
    content: "\0048";
  }
}
</style>
