<script lang="ts" setup>
import { computed } from 'vue';
import {
  type CampaignSetting,
  type ChooseKey,
  type CampaignLogSettings,
  type CampaignScenario,
  type CampaignOption,
  settingActive,
  anyForced,
  isForcedKey
} from '@/arkham/types/CampaignSettings'
import { toCapitalizedWords } from '@/arkham/helpers'

const props = defineProps<{
  step: CampaignScenario,
  campaignLog: CampaignLogSettings
}>()

defineEmits<{
  'toggle:key': [step: CampaignScenario, setting: CampaignSetting],
  'toggle:option': [setting: CampaignSetting],
  'toggle:set': [setting: CampaignSetting],
  'set:key': [step: CampaignScenario, setting: CampaignSetting, value: string],
  'set:option': [setting: CampaignSetting, option: CampaignOption],
  'toggle:crossout': [key: string, value: string],
  'set:num': [setting: CampaignSetting, value: number],
  'set:record': [setting: CampaignSetting, value: string]
  'toggle:record': [setting: CampaignSetting, value: string]
}>()

const activeSettings = computed(() => props.step.settings.filter((s) => settingActive(props.campaignLog, s)))

const isRecorded = (value: string) => props.campaignLog.keys.some((c) => c.key === value && c.scope === props.step.key)
const isOption = (option: CampaignOption) => props.campaignLog.options.includes(option)
const crossedOut = (key: string, value: string) => props.campaignLog.sets[key]?.entries.find((e) => e.value === value)?.tag === "CrossedOut"
const chosenNum = (option: CampaignOption) => props.campaignLog.counts[option.key]
const inSet = function(key: string, value: string) {
  const set = props.campaignLog.sets[key]
  if (set) {
    return set.entries.find((e) => e.value === value) !== undefined
  }

  return false
}

const forceDisabled = (setting: CampaignSetting, option: ChooseKey) => {
  return anyForced(props.campaignLog, setting) && !isForcedKey(props.campaignLog, option)
}

</script>

<template>
  <div class="settings-group">
    <header><h3>{{step.key}}</h3></header>
    <div v-for="setting in activeSettings" :key="setting.key">
      <div v-if="setting.type === 'SetKey'">
        <div class="options">
          <input type="checkbox" :name="`${step.key}${setting.key}`" :id="`${step.key}${setting.key}`" @change.prevent="$emit('toggle:key', step, setting)" :checked="isRecorded(setting.ckey)" />
          <label :for="`${step.key}${setting.key}`">{{toCapitalizedWords(setting.key)}}</label>
        </div>
      </div>
      <div v-else-if="setting.type === 'Option'">
        <div class="options">
          <input
            type="checkbox"
            :name="`${step.key}${setting.key}`"
            :id="`${step.key}${setting.key}`"
            @change.prevent="$emit('toggle:option', setting)"
            :checked="isOption({ key: setting.key, ckey: setting.ckey })"
          />
          <label :for="`${step.key}${setting.key}`">{{toCapitalizedWords(setting.key)}}</label>
        </div>
      </div>
      <div v-else-if="setting.type === 'ForceKey'">
        <div class="options">
          <input type="checkbox" :name="`${step.key}${setting.key}`" :id="`${step.key}${setting.key}`" checked disabled />
          <label :for="`${step.key}${setting.key}`">{{toCapitalizedWords(setting.key)}}</label>
        </div>
      </div>
      <div v-else-if="setting.type === 'ForceRecorded'">
        <div class="options">
          <input type="checkbox" :name="`${step.key}${setting.key}`" :id="`${step.key}${setting.key}`" checked disabled />
          <label :for="`${step.key}${setting.key}`">{{toCapitalizedWords(setting.key)}}</label>
        </div>
      </div>
      <div v-else-if="setting.type === 'ChooseKey'">
        {{toCapitalizedWords(setting.key)}}
        <div class="options">
          <template v-for="option in setting.content" :key="option.key">
            <input type="radio" :value="option.key" :name="`${step.key}${setting.key}`" :id="`${step.key}${setting.key}${option.key}`" @change.prevent="$emit('set:key', step, setting, option.key)" :checked="isRecorded(option.key)" :disabled="forceDisabled(setting, option)" />
            <label :for="`${step.key}${setting.key}${option.key}`">{{toCapitalizedWords(option.key)}}</label>
          </template>
        </div>
      </div>
      <div v-else-if="setting.type === 'ChooseOption'">
        {{toCapitalizedWords(setting.key)}}
        <div class="options">
          <template v-for="option in setting.content" :key="option.key">
            <input type="radio" :value="option.key" :name="`${step.key}${setting.key}`" :id="`${step.key}${setting.key}${option.key}`" @change.prevent="$emit('set:option', setting, option)" :checked="isOption(option)" />
            <label :for="`${step.key}${setting.key}${option.key}`">{{toCapitalizedWords(option.key)}}</label>
          </template>
        </div>
      </div>
      <div v-else-if="setting.type === 'Record'">
        {{toCapitalizedWords(setting.key)}}
        <div class="options">
          <input
            v-if="settingActive(campaignLog, setting.content)"
            type="checkbox"
            :name="`${step.key}${setting.key}${setting.content.key}`"
            :id="`${step.key}${setting.key}${setting.content.key}`"
            @change.prevent="$emit('toggle:record', setting, setting.content.content)"
            :checked="inSet(setting.key, setting.content.content)"
          />
          <label :for="`${step.key}${setting.key}${setting.content.key}`">
            {{toCapitalizedWords(setting.content.key)}}
          </label>
        </div>
      </div>
      <div v-else-if="setting.type === 'CrossOut'">
        {{toCapitalizedWords(setting.key)}}
        <div class="options">
          <template v-for="option in setting.content" :key="option.key">
            <input type="checkbox" :name="`${step.key}${setting.key}${option.key}`" :id="`${step.key}${setting.key}${option.key}`" class="invert" @change.prevent="$emit('toggle:crossout', setting.key, option.content)" :checked="crossedOut(setting.key, option.content)"/>
            <label :for="`${step.key}${setting.key}${option.key}`">
              <s v-if="crossedOut(setting.key, option.content)">{{toCapitalizedWords(option.key)}}</s>
              <span v-else>{{toCapitalizedWords(option.key)}}</span>
            </label>
          </template>
        </div>
      </div>
      <div v-else-if="setting.type === 'ChooseNum'">
        {{toCapitalizedWords(setting.key)}}
        <input
          type="number"
          :name="`${step.key}${setting.key}`"
          :id="`${step.key}${setting.key}`"
          :min="setting.min ? setting.min : 0"
          :max="setting.max"
          :value="chosenNum(setting)"
          @change.prevent="$emit('set:num', setting, parseInt(($event.target as HTMLInputElement)?.value || '0'))"
        />
      </div>
      <div v-else-if="setting.type === 'SetRecordable'" class="options">
        <input
          type="checkbox"
          :name="`${step.key}${setting.key}`"
          :id="`${step.key}${setting.key}`"
          @change.prevent="$emit('toggle:set', setting)"
          :checked="inSet(setting.ckey, setting.content)"
        />
        <label :for="`${step.key}${setting.key}`">{{toCapitalizedWords(setting.key)}}</label>
      </div>
      <div v-else-if="setting.type === 'ChooseRecordable'">
        {{toCapitalizedWords(setting.key)}}
        <div class="options">
          <template v-for="option in setting.content" :key="option.key">
            <input type="radio" :name="`${step.key}${setting.key}`" :id="`${step.key}${setting.key}${option.key}`" @change.prevent="$emit('set:record', setting, option.content)" :checked="inSet(setting.ckey, option.key)"/>
            <label :for="`${step.key}${setting.key}${option.key}`">{{toCapitalizedWords(option.key)}}</label>
          </template>
        </div>
      </div>
      <div v-else-if="setting.type === 'ChooseRecordables'">
        {{toCapitalizedWords(setting.key)}}
        <div class="options">
          <template v-for="option in setting.content" :key="option.key">
            <input type="checkbox" :name="`${step.key}${setting.key}`" :id="`${step.key}${setting.key}${option.key}`" @change.prevent="$emit('set:record', setting, option.content)" :checked="inSet(setting.ckey, option.key)"/>
            <label :for="`${step.key}${setting.key}${option.key}`">{{toCapitalizedWords(option.key)}}</label>
          </template>
        </div>
      </div>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.settings-group {
  background: rgba(255, 255, 255, 0.1);
  padding: 10px;
  margin-bottom: 10px;
  border-radius: 5px;
  header {
    h3 {
      padding: 0;
      margin: 0;
      text-transform: uppercase;
      color: rgba(255, 255, 255, 0.3) !important;
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

input[type=radio]:disabled + label {
  background: #999;
  color: #333;
  text-decoration: line-through;
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
</style>
