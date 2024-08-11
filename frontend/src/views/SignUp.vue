<script lang="ts" setup>
import { ref, reactive } from 'vue'
import { useUserStore } from '@/stores/user'
import { useRoute, useRouter } from 'vue-router'
import type { Registration } from '@/types'
import { useI18n } from 'vue-i18n'

const route = useRoute()
const router = useRouter()
const store = useUserStore()
const registration = reactive<Registration>({
  username: '',
  email: '',
  password: '',
})
const signUpError = ref<string|null>(null)

const { t } = useI18n()

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
    signUpError.value = t("usernameOrEmailAlreadyTaken")
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
          :placeholder="$t('username')"
        />
      </div>
      <div>
        <input
          v-model="registration.email"
          type="email"
          :placeholder="$t('email')"
        />
      </div>
      <div>
        <input
          v-model="registration.password"
          type="password"
          :placeholder="$t('password')"
        />
      </div>
      <div>
        <button>{{$t('register')}}</button>
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
