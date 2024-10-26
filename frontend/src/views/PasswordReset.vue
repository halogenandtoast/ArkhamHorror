<script lang="ts" setup>
import { reactive, ref } from 'vue'
import { useToast } from "vue-toastification";
import { useI18n } from 'vue-i18n';
import api from '@/api';

const submitted = ref(false)

interface PasswordReset {
  email: string
}

const reset = reactive<PasswordReset>({
  email: '',
})

const { t } = useI18n()

async function resetPassword() {
  submitted.value = true
  await api.post('password-reset', { email : reset.email })
  toast.success(t("passwordResetRequested"), { timeout: 3000 })
}

const toast = useToast()
</script>

<template>
  <form v-if="!submitted" @submit.prevent="resetPassword">
    <header><i class="secret"></i></header>
    <section>
      <div>
        <input
          v-model="reset.email"
          type="email"
          :placeholder="$t('email')"
        />
      </div>
      <div>
        <button>{{$t('resetPassword')}}</button>
      </div>
    </section>
  </form>
  <div v-else class="container box">
    <p>Check your email inbox (and spam folder) for instructions on how to reset your password.</p>
  </div>
</template>

<style scoped lang="scss">
form, .container {
  margin: 0 auto;
  margin-top: 10vh;
  width: 50vw;
  max-width: 400px;
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
</style>
