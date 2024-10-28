<script lang="ts" setup>
import { ref } from 'vue';
import type { User } from '@/types';

const props = defineProps<{
  user: User
  updateBeta: (setting: boolean) => void
}>()

const beta = ref(props.user.beta ? "On" : "Off")

const betaUpdate = async () => props.updateBeta(beta.value == "On")

const updateLanguage = (a: Event) => {
  const target = a.target as HTMLInputElement;
  localStorage.setItem('language', target.value)
}
</script>

<template>
  <div class="page-container">
    <div class="page-content column">
      <h2 class="title">{{$t('settings')}}</h2>

      <fieldset class="box column">
        <legend>{{$t('language')}}</legend>
        <p>This will change the language of the cards and app, but will default to English if a card or text is not available in the selected language.</p>
        <select v-model="$i18n.locale" @change="updateLanguage">
          <option value="en">English</option>
          <option value="it">Italiano</option>
          <option value="es">Español</option>
          <option value="zh">中文</option>
        </select>
      </fieldset>

      <fieldset class="box column">
        <legend>Enroll in beta</legend>
        <p>Beta features are likely very broken and games may be unrecoverable, please only enable this if you are willing to provide feedback.</p>
        <div class="row">
          <label>On <input type="radio" name="beta" value="On" v-model="beta" @change="betaUpdate" /></label>
          <label>Off <input type="radio" name="beta" value="Off" v-model="beta" @change="betaUpdate" /></label>
        </div>
      </fieldset>
    </div>
  </div>
</template>

<style lang="scss" scoped>
input[type="radio"] {
  display: unset;
}

legend {
 font-size: 1.1em;
 font-weight: bolder;
}
</style>
