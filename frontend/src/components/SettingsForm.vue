<script lang="ts" setup>
import { ref } from 'vue';
import type { User } from '@/types';

export interface Props {
  user: User
  updateBeta: (setting: boolean) => void
}

const props = defineProps<Props>()
const beta = ref(props.user.beta ? "On" : "Off")

const betaUpdate = async () => props.updateBeta(beta.value == "On")
</script>

<template>
  <div class="settings">
    <h1>Settings</h1>
    <fieldset>
      <legend>Enroll in beta</legend>
      <p>Beta features are likely very broken and games may be unrecoverable, please only enable this if you are willing to provide feedback</p>
      <label>On <input type="radio" name="beta" value="On" v-model="beta" @change="betaUpdate" /></label>
      <label>Off <input type="radio" name="beta" value="Off" v-model="beta" @change="betaUpdate" /></label>
    </fieldset>
  </div>
</template>

<style lang="scss" scoped>
.settings {
  width: 75vw;
  margin: 0 auto;
  margin-top: 20px;
  background: rgba(255, 255, 255, 0.7);
  padding: 10px;
  border-radius: 10px;
}

h1 {
  margin: 0;
  padding: 0;
}
</style>
