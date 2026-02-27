<script lang="ts" setup>
import { ref } from 'vue';
import type { User } from '@/types';
import { useDbCardStore } from '@/stores/dbCards'
import { checkImageExists } from '@/arkham/helpers'

const props = defineProps<{
  user: User
  updateBeta: (setting: boolean) => void
  deleteAccount: () => void
}>()

const store = useDbCardStore()
const beta = ref(props.user.beta ? "On" : "Off")
const showDeleteConfirm = ref(false)

const betaUpdate = async () => props.updateBeta(beta.value == "On")

const updateLanguage = async (a: Event) => {
  const target = a.target as HTMLInputElement;
  localStorage.setItem('language', target.value)
  await store.initDbCards()
  await checkImageExists()
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
          <option value="de">Deutsch/German</option>
          <option value="en">English</option>
          <option value="es">Español/Spanish</option>
          <option value="fr">Français/French</option>
          <option value="it">Italiano/Italian</option>
          <option value="ko">한국어/Korean</option>
          <option value="pl">Polski/Polish</option>
          <option value="po">Português/Portuguese</option>
          <option value="ru">Русский/Russian</option>
          <option value="uk">українська/Ukrainian</option>
          <option value="zh">中文/Chinese</option>
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

      <fieldset class="box column danger-zone">
        <legend>Danger Zone</legend>
        <p>Permanently delete your account and all associated data, including games and decks. <strong>This cannot be undone.</strong></p>
        <div v-if="!showDeleteConfirm">
          <button class="btn-danger" @click="showDeleteConfirm = true">Delete Account</button>
        </div>
        <div v-else class="column">
          <p class="warning">Are you sure? All your games, decks, and account data will be permanently lost and cannot be recovered.</p>
          <div class="row">
            <button class="btn-danger" @click="props.deleteAccount()">Yes, permanently delete my account</button>
            <button @click="showDeleteConfirm = false">Cancel</button>
          </div>
        </div>
      </fieldset>
    </div>
  </div>
</template>

<style scoped>
input[type="radio"] {
  display: unset;
}

legend {
 font-size: 1.1em;
 font-weight: bolder;
}

.danger-zone {
  border-color: #c0392b;
}

.danger-zone legend {
  color: #c0392b;
}

.btn-danger {
  background-color: #c0392b;
  color: white;
  border: none;
  padding: 0.5em 1em;
  cursor: pointer;
  border-radius: 4px;
}

.btn-danger:hover {
  background-color: #a93226;
}

.warning {
  color: #c0392b;
  font-weight: bold;
}
</style>
