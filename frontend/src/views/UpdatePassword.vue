<script lang="ts" setup>
import { reactive } from 'vue'
import { useToast } from "vue-toastification";
import { useI18n } from 'vue-i18n';
import api from '@/api';

export interface Props {
  resetId: string
}

const props = defineProps<Props>()

interface UpdatePassword {
  password: string
}

const reset = reactive<UpdatePassword>({
  password: '',
})

const { t } = useI18n()

async function updatePassword() {
  await api.put(`password-reset/${props.resetId}`, { password : reset.password })
  toast.success(t("passwordUpdatedSuccessfully"), { timeout: 3000 })
}

const toast = useToast()
</script>

<template>
  <form @submit.prevent="updatePassword">
    <header><i class="secret"></i></header>
    <section>
      <div>
        <input
          v-model="reset.password"
          type="password"
          :placeholder="$t('newPassword')"
        />
      </div>
      <div>
        <button>{{$t('updatePassword')}}</button>
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
