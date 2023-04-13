<script lang="ts" setup>
import { reactive } from 'vue'
import { useToast } from "vue-toastification";
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

async function updatePassword() {
  await api.put(`password-reset/${props.resetId}`, { password : reset.password })
  toast.success("Password Updated Successfully", { timeout: 3000 })
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
          placeholder="New Password"
        />
      </div>
      <div>
        <button>Update Password</button>
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

.error {
  background:#E3CCCD;
  color: #900000;
  border-radius: 5px;
  margin: 10px 5px;
  padding: 5px 10px;
}
</style>
