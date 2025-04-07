<script lang="ts" setup>
import { formatKey } from '@/arkham/types/Log';
import { useI18n } from 'vue-i18n';
import { ref, computed } from 'vue'
import {toCapitalizedWords} from '@/arkham/helpers'
import { Game } from '../types/Game'
import ScenarioSetting from './ScenarioSetting.vue'
import { Scenario, scenarioToKeyI18n } from '../types/Scenario'
import { StandaloneSetting, RecordedContent, SettingCondition } from '../types/StandaloneSetting'

const props = defineProps<{
  game: Game
  scenario: Scenario
  setting: StandaloneSetting
  playerId: string
}>()
const standaloneSettings = ref<StandaloneSetting[]>([])
const { t } = useI18n()

const flattenedSettings = computed(() => {
  const flattenSettings = (settings: StandaloneSetting[]): StandaloneSetting[] => {
    return settings.reduce((acc: StandaloneSetting[], setting) => {
      if (setting.type === 'Group') {
        return acc.concat(flattenSettings(setting.content))
      }
      const {ifRecorded} = setting
      if (ifRecorded) {
        if(!ifRecorded.some((cond) => inactive(cond))) acc.push(setting)
      } else {
        acc.push(setting)
      }
      return acc
    }, [])
  }

  return flattenSettings(standaloneSettings.value)
})

const findSetting = (key: string) => {
  return (flattenedSettings.value || []).find((s) => s.key === key)
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

  if (cond.type === 'survivedPlaneCrash') {
    const k = findSetting("KilledInPlaneCrash")
    if (!k) return false
    return k.content === cond.key
  }

  throw new Error(`Unknown condition type ${JSON.stringify(cond)}`)
}

const optionActive = (entry: RecordedContent) => {
  const {ifRecorded} = entry
  if (ifRecorded) {
    return !ifRecorded.some((cond) => inactive(cond))
  }

  return true
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

const displayKey = (key: string) => {
  const prefix = scenarioToKeyI18n(props.scenario)
  return t(formatKey({tag: prefix, contents: key}))
}
</script>

<template>
  <div v-if="setting.type === 'ChooseRecord'" class="options">
    <fieldset>
      <legend>{{toCapitalizedWords(setting.label)}}</legend>
      <template v-for="item in setting.content" :key="item.key">
        <input
          type="radio"
          v-model="setting.selected"
          :id="`${setting.key}${setting.label}${item.key}`"
          :value="item.key"
        />
        <label :for="`${setting.key}${setting.label}${item.key}`"> {{toCapitalizedWords(item.key)}}</label>
      </template>
    </fieldset>
  </div>
  <div v-if="setting.type === 'Group'" class="group-options">
    <h2 :for="setting.key">{{toCapitalizedWords(setting.key)}}</h2>
    <div class="group">
      <ScenarioSetting v-for="groupSetting in setting.content" :setting="groupSetting" :scenario="scenario" :game="game" :playerId="playerId" />
    </div>
  </div>
  <div v-if="setting.type === 'ChooseNum'" class="options">
    <input type="number" v-model="setting.content" :id="setting.key" :max="setting.max" :min="setting.min || 0" />
    <label :for="setting.key"> {{toCapitalizedWords(setting.key)}}</label>
  </div>
  <div v-if="setting.type === 'ToggleKey'" class="options">
    <input type="checkbox" v-model="setting.content" :id="setting.key"/>
    <label :for="setting.key"> {{toCapitalizedWords(setting.key)}}</label>
  </div>
  <div v-else-if="setting.type === 'ToggleOption'" class="options">
    <input type="checkbox" v-model="setting.content" :id="setting.key"/>
    <label :for="setting.key"> {{toCapitalizedWords(setting.key)}}</label>
  </div>
  <div v-else-if="setting.type === 'PickKey'">
    <h3 v-if="setting.content">{{toCapitalizedWords(setting.key)}}</h3>
    <div class="options">
      <template v-for="key in setting.keys" :key="`${setting.key}${key}`">
        <input
          type="radio"
          v-model="setting.content"
          :value="key"
          :name="setting.key"
          :id="`${setting.key}${key}`"
          :checked="key === setting.content"
        />
        <label :for="`${setting.key}${key}`">{{displayKey(key)}}</label>
      </template>
    </div>
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
  <div v-else-if="setting.type === 'ToggleRecords'" class="records">
    {{toCapitalizedWords(setting.key)}}
    <div class="options">
      <template v-for="option in setting.content" :key="option.key">
        <input
          type="checkbox"
          v-model="option.content"
          :id="`${setting.key}${option.key}`"
          :checked="option.content"
          v-if="optionActive(option)"
        />
        <label :for="`${setting.key}${option.key}`">{{option.label}}</label>
      </template>
    </div>
  </div>
  <div v-else-if="setting.type === 'SetPartnerKilled'" class="options">
    <input
      type="radio"
      v-model="setting.content"
      value="08720"
      :id="`${setting.key}-kensler`"
    />
    <label :for="`${setting.key}-kensler`">Dr. Amy Kensler</label>
    <input
      type="radio"
      v-model="setting.content"
      value="08714"
      :id="`${setting.key}-dyer`"
    />
    <label :for="`${setting.key}-dyer`">Prof. William Dyer</label>
    <input
      type="radio"
      v-model="setting.content"
      value="08715"
      :id="`${setting.key}-danforth`"
    />
    <label :for="`${setting.key}-danforth`">Danforth</label>
    <input
      type="radio"
      v-model="setting.content"
      value="08721"
      :id="`${setting.key}-ellsworth`"
    />
    <label :for="`${setting.key}-ellsworth`">Roald Ellsworth</label>
    <input
      type="radio"
      v-model="setting.content"
      value="08722"
      :id="`${setting.key}-takada`"
    />
    <label :for="`${setting.key}-takada`">Takada Hiroko</label>
    <input
      type="radio"
      v-model="setting.content"
      value="08718"
      :id="`${setting.key}-claypool`"
    />
    <label :for="`${setting.key}-claypool`">Avery Claypool</label>
    <input
      type="radio"
      v-model="setting.content"
      value="08717"
      :id="`${setting.key}-sinha`"
    />
    <label :for="`${setting.key}-sinha`">Dr. Mala Sinha</label>
    <input
      type="radio"
      v-model="setting.content"
      value="08719"
      :id="`${setting.key}-cookie`"
    />
    <label :for="`${setting.key}-cookie`">James "Cookie" Fredericks</label>
    <input
      type="radio"
      v-model="setting.content"
      value="08716"
      :id="`${setting.key}-ashevak`"
    />
    <label :for="`${setting.key}-ashevak`">Eliyah Ashevak</label>
  </div>
  <div v-else-if="setting.type === 'SetPartnerDetails'" class="partner">
    <h3 class="partner-name">{{setting.key}}</h3>
    <div class="options">
      <label :for="`${setting.key}-damage`">Damage:</label>
      <input type="number" v-model="setting.content.damage" :id="`${setting.key}-damage`" :min="0" :max="setting.maxDamage" />
      <label :for="`${setting.key}-horror`">Horror:</label>
      <input type="number" v-model="setting.content.horror" :id="`${setting.key}-horror`" :min="0" :max="setting.maxHorror" />
      <select v-model="setting.content.status" :id="`${setting.key}-status`">
        <option value="Safe">Safe</option>
        <option value="Resolute">Resolute</option>
        <option value="Eliminated">Eliminated</option>
      </select>
    </div>
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

.group, .group-options {
  display: flex;
  flex-direction: column;
  gap: 10px;
}
</style>
