<script lang="ts" setup>
import { watch, ref, computed } from 'vue'
import scenarioJSON from '@/arkham/data/scenarios.json'
import {toCapitalizedWords} from '@/arkham/helpers'
import {updateStandaloneSettings} from '@/arkham/api'
import { Game } from '../types/Game'
import { Scenario } from '../types/Scenario'
import { StandaloneSetting } from '../types/StandaloneSetting'

const props = defineProps<{
  game: Game
  scenario: Scenario
  investigatorId: string
}>()
const standaloneSettings = ref<StandaloneSetting[]>([])

// computed standaloneSettings is a bit of a hack, because nested values change by value
// when we change standaloneSettings they are "cached" so to avoid this we deep copy the
// standaloneSettings in order to never alter its original value.
const computedStandaloneSettings = computed<StandaloneSetting[]>(() => {
  const s = scenarioJSON.find((s) => s.id === props.scenario.id.replace(/^c/, ''))
  return s ? s.settings as StandaloneSetting[] : []
})

watch(computedStandaloneSettings, (newSettings) => {
  standaloneSettings.value = newSettings
}, { immediate: true })

const submit = () => updateStandaloneSettings(props.game.id, standaloneSettings.value)
</script>

<template>
  <div class="container">
    <p>Standalone Settings</p>
    <div v-for="setting in standaloneSettings" :key="setting.key">
      <div v-if="setting.type === 'ToggleKey'" class="options">
        <input type="checkbox" v-model="setting.content" :id="setting.key"/>
        <label :for="setting.key"> {{toCapitalizedWords(setting.key)}}</label>
      </div>
      <div v-else-if="setting.type === 'ToggleOption'" class="options">
        <input type="checkbox" v-model="setting.content" :id="setting.key"/>
        <label :for="setting.key"> {{toCapitalizedWords(setting.key)}}</label>
      </div>
      <div v-else-if="setting.type === 'PickKey'" class="options">
        <template v-for="key in setting.keys" :key="`${setting.key}${key}`">
          <input
            type="radio"
            v-model="setting.content"
            :value="key"
            :name="setting.key"
            :id="`${setting.key}${key}`"
            :checked="key === setting.content"
          />
          <label :for="`${setting.key}${key}`"> {{toCapitalizedWords(key)}}</label>
        </template>
      </div>
      <div v-else-if="setting.type === 'ToggleCrossedOut'">
        {{toCapitalizedWords(setting.key)}}
        <div class="options">
          <template v-for="option in setting.content" :key="option.key">
            <input
              type="checkbox"
              v-model="option.content"
              :id="`${option.key}${option.label}`"
              class="invert"
              :checked="option.content"
            />
            <label :for="`${option.key}${option.label}`">
              <s v-if="option.content">{{option.label}}</s>
              <span v-else>{{option.label}}</span>
            </label>
          </template>
        </div>
      </div>
    </div>
    <button @click="submit">Begin</button>
  </div>
</template>

<style lang="scss">
.container {
  width: 100%;
  max-width: 800px;
  margin: 0 auto;
  margin-top: 10px;
}
.options {
  display: flex;
  margin-bottom: 10px;
  label {
    flex: 1;
    text-align: center;
    margin-left: 10px;
    &:nth-of-type(1) {
      margin-left: 0;
    }
  }
}

input[type=radio] {
  display: none;
  /* margin: 10px; */
}

input[type=radio] + label {
  display:inline-block;
  padding: 4px 12px;
  background-color: desaturate(#6E8640, 30%);
  &:hover {
    background-color: desaturate(#6E8640, 20%);
  }
  border-color: #ddd;
}

input[type=radio]:checked + label {
  background: #6E8640;
}

input[type=checkbox] {
  display: none;
  /* margin: 10px; */
}

input[type=checkbox] + label {
  display:inline-block;
  padding: 4px 12px;
  background-color: desaturate(#6E8640, 30%);
  &:hover {
    background-color: desaturate(#6E8640, 20%);
  }

  &.invert {
    background: #6E8640;
    &:hover {
      background: #6E8640;
    }
  }
  border-color: #ddd;
}

input[type=checkbox]:checked + label {
  background: #6E8640;
  &.invert {
    background-color: desaturate(#6E8640, 30%);
  }
}

.invert[type=checkbox] + label {
    background: #6E8640;
    &:hover {
      background: #6E8640;
    }
}

.invert[type=checkbox]:checked + label {
  background-color: desaturate(#6E8640, 30%);
}
</style>
