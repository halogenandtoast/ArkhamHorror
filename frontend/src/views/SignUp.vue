<script lang="ts" setup>
import { ref, reactive } from 'vue'
import { useUserStore } from '@/stores/user'
import { useSiteSettingsStore } from '@/stores/site_settings'
import { useRoute, useRouter } from 'vue-router'
import { storeToRefs } from 'pinia'
import type { Registration } from '@/types'
import { useI18n } from 'vue-i18n'
import { isAxiosError } from 'axios'

const route = useRoute()
const router = useRouter()
const store = useUserStore()
/* The server refuses the POST on its own; this is so someone who kept the URL
 * is told why instead of filling in a form that cannot succeed. A 403 from the
 * POST flips it too, for the case where the flag was turned on after this tab
 * loaded. */
const { inviteOnly } = storeToRefs(useSiteSettingsStore())
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
  } catch (error) {
    if (isAxiosError(error) && error.response?.status === 403) {
      inviteOnly.value = true
    } else {
      signUpError.value = t("usernameOrEmailAlreadyTaken")
    }
  }
}
</script>

<template>
  <div v-if="inviteOnly" class="closed">
    <header><i class="secret"></i></header>
    <section>
      <h1>{{ $t('registrationClosed.title') }}</h1>
      <p>{{ $t('registrationClosed.body') }}</p>
      <router-link to="/sign-in">{{ $t('logIn') }}</router-link>
    </section>
  </div>
  <form v-else @submit.prevent="register">
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

<style scoped>
form, .closed {
  margin: 0 auto;
  margin-top: 10vh;
  width: 50vw;
  max-width: 400px;
}

.closed {
  text-align: center;

  h1 {
    font-size: 1.3em;
    margin: 0 0 10px;
  }

  p {
    margin: 0 0 15px;
  }

  a {
    color: var(--spooky-green);
  }
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
  background: var(--button-1);
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
  color: #15192C;
  -webkit-font-smoothing: antialiased;
  position: relative;

  &:before {
    font-family: "Arkham";
    content: "\0048";
  }
}
</style>
