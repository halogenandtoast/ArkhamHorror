<script lang="ts" setup>
import { watch, ref, computed } from 'vue'
import scenarioJSON from '@/arkham/data/scenarios'
import {updateStandaloneSettings} from '@/arkham/api'
import { Game } from '../types/Game'
import { Scenario } from '../types/Scenario'
import { StandaloneSetting, RecordedContent, SettingCondition } from '../types/StandaloneSetting'
import ScenarioSetting from '@/arkham/components/ScenarioSetting.vue'

const props = defineProps<{
  game: Game
  scenario: Scenario
  playerId: string
}>()
const standaloneSettings = ref<StandaloneSetting[]>([])

// computed standaloneSettings is a bit of a hack, because nested values change by value
// when we change standaloneSettings they are "cached" so to avoid this we deep copy the
// standaloneSettings in order to never alter its original value.
const computedStandaloneSettings = computed<StandaloneSetting[]>(() => {
  const s = scenarioJSON.find((s) => s.id === props.scenario.id.replace(/^c/, ''))
  return s?.settings ? s.settings as StandaloneSetting[] : []
})

watch(computedStandaloneSettings, (newSettings) => {
  standaloneSettings.value = newSettings
}, { immediate: true })

const submit = async () => {
  // When we submit we want to remove any settings which won't be active
  // because of how we set settings for standalone scenarios, the selected
  // element is in the reactive data. So we create a deep copy and then remove
  // anything that is inactive
  let settings = JSON.parse(JSON.stringify(standaloneSettings.value))

  settings = settings.filter((setting: StandaloneSetting) => {
    const {ifRecorded} = setting
    if (ifRecorded) {
      return !ifRecorded.some((cond) => inactive(cond))
    }

    return true
  })

  updateStandaloneSettings(props.game.id, settings)
}

const inactive = (cond: SettingCondition): boolean => {
  if (cond.type === 'inSet') {
    const {key, content} = cond
    const setting = standaloneSettings.value.find((s) => s.key === key)
    if (!setting) return false

    const check = setting.key !== 'ToggleCrossedOut'

    if (setting.type === "ToggleCrossedOut") {
      return setting.content.some((c) => c.content === check && c.key == content)
    }

    if (setting.type === "ToggleRecords") {
      return !setting.content.some((c) => {
        return c.content && c.key == content
      })
    }


    throw new Error(`Unhandled setting type ${setting.type}`)
  }

  if (cond.type === 'not') {
    return !inactive(cond.content)
  }

  if (cond.type === 'or') {
    return !cond.content.some((c) => !inactive(c))
  }

  if (cond.type === 'option') {
    const k = standaloneSettings.value.find((s) => s.key === cond.key)
    if (!k) return false
    if (k.type === 'ToggleOption') {
      if (k.ifRecorded) {
        return !k.content && !k.ifRecorded.some((cond) => inactive(cond))
      }
      return !k.content
    }

    return false
  }

  throw new Error(`Unknown condition type ${JSON.stringify(cond)}`)
}

const activeSettings = computed(() => {
  return standaloneSettings.value.filter((setting) => {
    const {ifRecorded} = setting
    if (ifRecorded) {
      return !ifRecorded.some((cond) => inactive(cond))
    }

    return true
  })
})
</script>

<template>
  <div class="container scroll-container">
    <h2>Standalone Settings</h2>
    <div v-if="activeSettings.length == 0">
      <p>There are currently no standalone settings available for this scenario.</p>
    </div>
    <div v-for="setting in activeSettings" :key="setting.key">
      <ScenarioSetting :setting="setting" :scenario="scenario" :game="game" :playerId="playerId" />
    </div>
    <button @click="submit">Begin</button>
  </div>
</template>

<style lang="scss" scoped>
.container {
  width: 100%;
  height: fit-content;
  max-width: 800px;
  margin: 0 auto;
  margin-top: 10px;
  padding: 10px;
  background-color: #3E485C;
  border-radius: 5px;
  font-size: 1.5em;
  color: #B6B6B6;
  box-shadow: 1px 1px 6px rgba(15,17,23,0.45);
  display: flex;
  flex-direction: column;
  gap: 10px;
}

h2 {
  padding: 0;
  margin: 0;
  text-transform: uppercase;
  font-family: Teutonic;
  font-size: 1.5em;
}

button {
  width: 100%;
  background-color: var(--button-1);
  border: 0;
  text-transform: uppercase;
  color: white;
  padding: 10px;
}

.options {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
  label {
    flex: 1;
    min-width: fit-content;
    text-align: center;
  }
}

input[type=radio] {
  display: none;
  /* margin: 10px; */
}

input[type=radio] + label {
  display:inline-block;
  padding: 4px 12px;
  background-color: hsl(80, 5%, 39%);
  border-color: #ddd;
  &:hover {
    background-color: hsl(80, 15%, 39%);
  }
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
  background-color: hsl(80, 5%, 39%);
  &:hover {
    background-color: hsl(80, 15%, 39%);
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
    background-color: hsl(80, 5%, 39%);
  }
}

.invert[type=checkbox] + label {
    background: #6E8640;
    &:hover {
      background: #6E8640;
    }
}

.invert[type=checkbox]:checked + label {
  background-color: hsl(80, 15%, 39%);
}

.partner {
  display: flex;
  flex-direction: column;
  gap: 10px;
  background: rgba(0, 0, 0, 0.4);
  padding: 10px;
  border-radius: 5px;
  .options {
    justify-content: flex-start;
    label {
      text-align: left;
      flex: 0;
    }
    input {
      width: 100px;
    }
  }
}

.partner-name {
  text-align: right;
}

.records {
  background: rgba(0, 0, 0, 0.4);
  border-radius: 5px;
  padding: 10px;
  display: flex;
  flex-direction: column;
  gap: 10px;
}
</style>
