<script lang="ts" setup>
import { useUserStore } from '@/stores/user'
import { useRoute, useRouter } from 'vue-router'
import { ref, reactive } from 'vue'
import { Credentials } from '../types'
import { useI18n } from 'vue-i18n'

const { t } = useI18n()
const store = useUserStore()
const route = useRoute()
const router = useRouter()
const credentials = reactive<Credentials>({
  email: '',
  password: '',
})
const signInError = ref<string|null>(null)

const health = ref<boolean>(true)

const checkHealth = async () => {
  try {
    const response = await fetch("/health")
    health.value = response.ok
  } catch {
    health.value = false
  }

  setTimeout(checkHealth, 5000)
}

await checkHealth()

async function authenticate() {
  signInError.value = null
  try {
    await store.authenticate(credentials)
    const { nextUrl } = route.query
    if (nextUrl) {
      router.push({ path: nextUrl as string })
    } else {
      router.push({ path: '/' })
    }
  } catch {
    signInError.value = t("invalidEmailOrPassword")
  }
}
</script>

<template>
  <form v-if="health" @submit.prevent="authenticate">
    <header><i class="secret"></i></header>
    <div class="error" v-if="signInError">{{signInError}}</div>
    <section>
      <div>
        <input
          v-model="credentials.email"
          type="email"
          :placeholder="$t('email')"
        />
      </div>
      <div>
        <input
          v-model="credentials.password"
          type="password"
          :placeholder="$t('password')"
        />
      </div>
      <div>
        <button>{{$t('logIn')}}</button>
      </div>
    </section>
    <section>
      <router-link to="/password-reset">{{$t('forgotPassword')}}</router-link>
    </section>
  </form>
  <div v-else class="service-down">
    <div class="service-down-card">
      <h1>{{$t('serviceUnavailable')}}</h1>
      <p>{{$t('pleaseTryAgainLater')}}</p>
    </div>
  </div>
</template>

<style scoped lang="scss">
form {
  margin: 0 auto;
  margin-top: 10vh;
  width: 50vw;
  max-width: 400px;
}

section {
  border-radius: 3px;
  padding: 10px;
}

header {
  text-align: center;
}

input {
  outline: 0;
  border: 0;
  padding: 15px;
  background: var(--background-dark);
  width: 100%;
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
    background: hsl(80, 35%, 32%);
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
  color: #16192B;
  -webkit-font-smoothing: antialiased;
  position: relative;

  &:before {
    font-family: "Arkham";
    content: "\0048";
  }
}

.error {
  background:#E3CCCD;
  color: #900000;
  border-radius: 5px;
  margin: 10px 5px;
  padding: 5px 10px;
}

.service-down {
  display: flex;
  justify-content: center;
  align-items: center;
  height: 100vh;
  background: linear-gradient(135deg, var(--background-dark), var(--background));
}

.service-down-card {
  background: var(--background-dark);
  color: #2a2f47;
  padding: 2rem 3rem;
  border-radius: 12px;
  box-shadow: var(--card-shadow);
  text-align: center;
  max-width: 400px;
}

.service-down-card h1 {
  text-transform: uppercase;
  font-family: 'Teutonic', sans-serif;
  font-size: 1.8rem;
  margin-bottom: 0.75rem;
  color: var(--title);
}

.service-down-card p {
  font-size: 1rem;
  color: var(--text);
}
</style>
