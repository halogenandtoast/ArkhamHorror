<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { storeToRefs } from 'pinia'
import type { User } from '@/types'
import { useDbCardStore } from '@/stores/dbCards'
import { useSettings } from '@/stores/settings'
import { checkImageExists } from '@/arkham/helpers'
import { isDevBuild } from '@/arkham/displayRules'
import { loadLocaleMessages, normalizeLocale } from '@/locales/messages'

const props = defineProps<{
  user: User
  updateBeta: (setting: boolean) => void
  deleteAccount: () => void
}>()

const store = useDbCardStore()
const settings = useSettings()
const { epicMultiplayerStored, customCardsEnabled } = storeToRefs(settings)
const dev = isDevBuild()
const { availableLocales, locale, setLocaleMessage } = useI18n({ useScope: 'global' })
const language = ref(localStorage.getItem('language') || locale.value)
const beta = ref(props.user.beta ? 'On' : 'Off')
const showDeleteConfirm = ref(false)
const tabs = ['account', 'features'] as const
const activeTab = ref<(typeof tabs)[number]>('account')

function navigateTabs(event: KeyboardEvent) {
  const index = tabs.indexOf(activeTab.value)
  let next: number
  switch (event.key) {
    case 'ArrowRight':
      next = (index + 1) % tabs.length
      break
    case 'ArrowLeft':
      next = (index + tabs.length - 1) % tabs.length
      break
    case 'Home':
      next = 0
      break
    case 'End':
      next = tabs.length - 1
      break
    default:
      return
  }
  event.preventDefault()
  activeTab.value = tabs[next]
  document.getElementById(`settings-tab-${activeTab.value}`)?.focus()
}

const betaUpdate = async () => props.updateBeta(beta.value == 'On')

// Dev-only Epic Multiplayer flag, bound to the persisted store value via On/Off.
const epicMultiplayer = computed({
  get: () => (epicMultiplayerStored.value ? 'On' : 'Off'),
  set: (value: string) => settings.setEpicMultiplayerEnabled(value === 'On')
})

const customCards = computed({
  get: () => (customCardsEnabled.value ? 'On' : 'Off'),
  set: (value: string) => settings.setCustomCardsEnabled(value === 'On')
})

const revisedCoreArt = computed({
  get: () => settings.useVariants.includes('revised'),
  set: (enabled: boolean) =>
    settings.setUseVariants(
      enabled ? [...settings.useVariants, 'revised'] : settings.useVariants.filter((variant) => variant !== 'revised')
    )
})

const updateLanguage = async (a: Event) => {
  const target = a.target as HTMLSelectElement
  const selectedLanguage = target.value
  const uiLocale = normalizeLocale(selectedLanguage)

  if (!availableLocales.includes(uiLocale)) {
    const messages = await loadLocaleMessages(uiLocale)
    setLocaleMessage(messages.locale, messages.messages)
  }

  language.value = selectedLanguage
  locale.value = uiLocale
  localStorage.setItem('language', selectedLanguage)
  await store.initDbCards()
  await checkImageExists()
}
</script>

<template>
  <div class="page-container">
    <div class="page-content column">
      <h2 class="title">{{ $t('settings') }}</h2>

      <div class="settings-tabs" role="tablist" :aria-label="$t('settings')" @keydown="navigateTabs">
        <button
          v-for="tab in tabs"
          :key="tab"
          :id="`settings-tab-${tab}`"
          type="button"
          role="tab"
          :aria-selected="activeTab === tab"
          :aria-controls="`settings-panel-${tab}`"
          :tabindex="activeTab === tab ? 0 : -1"
          @click="activeTab = tab"
        >
          {{ $t(`settingsForm.${tab}`) }}
        </button>
      </div>

      <div
        v-show="activeTab === 'account'"
        id="settings-panel-account"
        class="column settings-panel"
        role="tabpanel"
        aria-labelledby="settings-tab-account"
        tabindex="0"
      >
        <section class="box column">
          <h3>{{ $t('language') }}</h3>
          <p>{{ $t('settingsForm.languageHelp') }}</p>
          <select :value="language" @change="updateLanguage">
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
            <option value="zh-cn">简体中文/Simplified Chinese</option>
            <option value="zh">中文/Chinese</option>
          </select>
        </section>

        <section class="box column danger-zone">
          <h3 class="danger-title">{{ $t('settingsForm.dangerZone') }}</h3>
          <p>
            {{ $t('settingsForm.dangerZoneDescription') }} <strong>{{ $t('settingsForm.cannotBeUndone') }}</strong>
          </p>
          <div v-if="!showDeleteConfirm">
            <button class="btn-danger" @click="showDeleteConfirm = true">{{ $t('settingsForm.deleteAccount') }}</button>
          </div>
          <div v-else class="column">
            <p class="warning">{{ $t('settingsForm.deleteConfirm') }}</p>
            <div class="row">
              <button class="btn-danger" @click="props.deleteAccount()">
                {{ $t('settingsForm.confirmPermanentDelete') }}
              </button>
              <button @click="showDeleteConfirm = false">{{ $t('cancel') }}</button>
            </div>
          </div>
        </section>
      </div>

      <div
        v-show="activeTab === 'features'"
        id="settings-panel-features"
        class="column settings-panel"
        role="tabpanel"
        aria-labelledby="settings-tab-features"
        tabindex="0"
      >
        <section class="box column">
          <label class="radio-label">
            <input type="checkbox" v-model="revisedCoreArt" aria-describedby="revised-core-art-description" />
            {{ $t('settingsForm.usedRevisedCoreArt') }}
          </label>
          <p id="revised-core-art-description">{{ $t('settingsForm.revisedCoreArtDescription') }}</p>
        </section>

        <section class="box column experiments" aria-labelledby="experiments-title">
          <h3 id="experiments-title">{{ $t('settingsForm.experiments') }}</h3>
          <p class="experiments-warning">
            <font-awesome-icon icon="flask" />
            {{ $t('settingsForm.experimentsWarning') }}
          </p>
          <section class="experiment column">
            <h4>{{ $t('settingsForm.enrollInBeta') }}</h4>
            <p>{{ $t('settingsForm.betaHelp') }}</p>
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

          <section class="experiment column">
            <h4>{{ $t('settingsForm.customCards') }}</h4>
            <i18n-t keypath="settingsForm.customCardsHelp" tag="p" scope="global">
              <template #icon>
                <font-awesome-icon icon="layer-group" class="inline-icon" />
              </template>
            </i18n-t>
            <div class="row">
              <label class="radio-label">
                <input type="radio" name="customCards" value="On" v-model="customCards" />
                {{ $t('On') }}
              </label>
              <label class="radio-label">
                <input type="radio" name="customCards" value="Off" v-model="customCards" />
                {{ $t('Off') }}
              </label>
            </div>
            <router-link v-if="customCardsEnabled" to="/card-builder" class="builder-link">
              {{ $t('settingsForm.openCardBuilder') }}
            </router-link>
          </section>

          <section v-if="dev" class="experiment column">
            <h4>{{ $t('settingsForm.epicMultiplayer') }}</h4>
            <p>{{ $t('settingsForm.epicMultiplayerWarning') }}</p>
            <div class="row">
              <label class="radio-label">
                <input type="radio" name="epicMultiplayer" value="On" v-model="epicMultiplayer" />
                {{ $t('On') }}
              </label>
              <label class="radio-label">
                <input type="radio" name="epicMultiplayer" value="Off" v-model="epicMultiplayer" />
                {{ $t('Off') }}
              </label>
            </div>
          </section>
        </section>
      </div>
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

input[type='radio'] {
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

/* The overlay button wears this icon, so the help can point straight at it. */
.inline-icon {
  color: var(--title);
  margin: 0 0.15em;
}

.builder-link {
  color: var(--spooky-green);
  width: fit-content;
}

.settings-tabs {
  display: flex;
  gap: 0.5rem;
  border-bottom: 1px solid var(--box-border);
}

.settings-tabs button {
  background: transparent;
  color: var(--title);
  border: 0;
  border-bottom: 3px solid transparent;
  padding: 0.75rem 1rem;
  cursor: pointer;
  font: inherit;
}

.settings-tabs button[aria-selected='true'] {
  border-bottom-color: var(--spooky-green);
  font-weight: bold;
}

.settings-tabs button:hover {
  background: var(--background-dark);
}

.settings-tabs button:focus-visible,
.settings-panel:focus-visible {
  outline: 2px solid var(--spooky-green);
  outline-offset: 2px;
}

.settings-panel {
  gap: 1rem;
}

.experiments-warning {
  color: #f5d76e;
  background: #342c14;
  border-left: 3px solid #f5d76e;
  padding: 0.75rem 1rem;
  opacity: 1;
}

.experiment {
  border-top: 1px solid var(--box-border);
  padding-top: 1rem;
  margin-top: 0.5rem;
}

.experiment h4 {
  margin: 0 0 4px;
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 1.2em;
  text-transform: uppercase;
}
</style>
