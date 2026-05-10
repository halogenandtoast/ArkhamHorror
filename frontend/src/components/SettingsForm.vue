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

      <section class="box column">
        <h3>{{$t('language')}}</h3>
        <p>{{ $t('settingsForm.languageHelp') }}</p>
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
      </section>

      <section class="box column">
        <h3>{{ $t('settingsForm.enrollInBeta') }}</h3>
        <p>{{ $t('settingsForm.betaWarning') }}</p>
        <div class="row">
          <label class="radio-label">
            <input type="radio" name="beta" value="On" v-model="beta" @change="betaUpdate" />
            {{ $t('On') }}
          </label>
          <label class="radio-label">
            <input type="radio" name="beta" value="Off" v-model="beta" @change="betaUpdate" />
            {{ $t('Off') }}
          </label>
        </div>
      </section>

      <section class="box column danger-zone">
        <h3 class="danger-title">{{ $t('settingsForm.dangerZone') }}</h3>
        <p>{{ $t('settingsForm.dangerZoneDescription') }} <strong>{{ $t('settingsForm.cannotBeUndone') }}</strong></p>
        <div v-if="!showDeleteConfirm">
          <button class="btn-danger" @click="showDeleteConfirm = true">{{ $t('settingsForm.deleteAccount') }}</button>
        </div>
        <div v-else class="column">
          <p class="warning">{{ $t('settingsForm.deleteConfirm') }}</p>
          <div class="row">
            <button class="btn-danger" @click="props.deleteAccount()">{{ $t('settingsForm.confirmPermanentDelete') }}</button>
            <button @click="showDeleteConfirm = false">{{ $t('cancel') }}</button>
          </div>
        </div>
      </section>
    </div>
  </div>
</template>

<style scoped>
h3 {
  font-size: 1.1em;
  font-weight: bold;
  color: var(--title);
  text-transform: uppercase;
  font-family: teutonic, sans-serif;
  font-size: 1.4em;
}

p {
  color: var(--title);
  opacity: 0.8;
}

select {
  background-color: var(--background-dark);
  color: var(--title);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  padding: 6px 10px;
  font-size: 1em;
  width: fit-content;
}

input[type="radio"] {
  display: unset;
  accent-color: var(--spooky-green);
}

.radio-label {
  display: flex;
  align-items: center;
  gap: 6px;
  color: var(--title);
  cursor: pointer;
}

.danger-zone {
  border-color: var(--delete);
}

.danger-title {
  color: var(--delete);
}

.btn-danger {
  background-color: var(--delete);
  color: white;
  border: none;
  padding: 8px 16px;
  cursor: pointer;
  border-radius: 4px;
  font-size: 1em;
  text-transform: uppercase;
}

.btn-danger:hover {
  background-color: #a32929;
}

.warning {
  color: var(--delete);
  font-weight: bold;
}
</style>
