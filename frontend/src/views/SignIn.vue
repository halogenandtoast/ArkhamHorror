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
  <form @submit.prevent="authenticate">
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
</style>
