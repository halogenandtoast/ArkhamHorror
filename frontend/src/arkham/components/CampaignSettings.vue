<script lang="ts" setup>
import { watch, ref, computed } from 'vue'
import campaignJSON from '@/arkham/data/campaigns.json'
import scenarioJSON from '@/arkham/data/scenarios.json'
import {toCapitalizedWords} from '@/arkham/helpers'
import {updateCampaignSettings} from '@/arkham/api'
import {
  CampaignLogSettings,
  CampaignOption,
  CampaignScenario,
  CampaignSetting,
  settingActive,
  completedCampaignScenarioSetting,
  isForcedKey,
  anyForced
} from '@/arkham/types/CampaignSettings'
import CampaignScenarioSetting from '@/arkham/components/CampaignScenarioSetting.vue'

export interface Props {
  game: Game
  campaign: Campaign
  investigatorId: string
}

const props = defineProps<Props>()
defineEmits(['update', 'choose'])

const campaignLog = ref<CampaignLogSettings>({
  keys: [],
  counts: {},
  sets: {},
  options: []
})

const selectedScenario = computed(() => props.campaign.step.contents.replace(/^c/, ''))

const scenario = computed(() => scenarioJSON.find((s) => s.id === selectedScenario.value))

const campaignSettings = ref<CampaignScenario[]>([])

const activeSettings = computed(() => {
  const allActive = campaignSettings.value.filter((s) => settingActive(campaignLog.value, s))
  const firstNotCompleted = allActive.findIndex((s) => !completedCampaignScenarioSetting(campaignLog.value, s))

  return firstNotCompleted === -1
    ? allActive
    : campaignSettings.value.filter((s) => settingActive(campaignLog.value, s)).slice(0, firstNotCompleted + 1)
})

// computed standaloneSettings is a bit of a hack, because nested values change by value
// when we change standaloneSettings they are "cached" so to avoid this we deep copy the
// standaloneSettings in order to never alter its original value.
const computedCampaignSettings = computed<CampaignScenario[]>(() => {
  const c = campaignJSON.find((c) => c.id == props.campaign.id)
  return c.settings || []
})

const completedCampaignSettings = computed(() => {
  return campaignSettings.value.every((s) =>
    settingActive(campaignLog.value, s)
      ? completedCampaignScenarioSetting(campaignLog.value, s)
      : true
  )
})

const isForced = (s) => s.scenarioId == selectedScenario.value

watch(computedCampaignSettings, (newSettings) => {
  const idx = newSettings.
    findIndex((s) => s.scenarioId === selectedScenario.value)

  const relevantSettings = newSettings.
    filter((s, i) => i < idx || s.force && isForced(s.force))

  const crossOut = relevantSettings.
    flatMap((s) => s.settings.
    filter(s => s.type === "CrossOut"))

  const sets = crossOut.reduce((a, s) => ({
    ...a,
    [s.key]: {
      recordable: s.recordable,
      entries: s.content.map((c) => ({ tag: "Recorded", value: c.content }))
    }
  }), {})

  const counts = relevantSettings.
    flatMap((s) => s.settings.filter(s => s.type === "ChooseNum")).
    reduce((a, s) => ({ ...a, [s.key]: 0 }), {})

  campaignLog.value = { keys: [], counts, options: [], sets }

  campaignSettings.value = relevantSettings
}, { immediate: true })

// remove any associated recorded sets or keys for settings that are no longer valid
const filterSettings = function() {
  const active = activeSettings.value.reduce((a, s) => {
    s.settings.forEach((t) => {
      if(settingActive(campaignLog.value, t)) {
        if (t.type === 'ChooseRecordable') {
          a.sets.push(t.ckey)
        }
        if (t.type === 'Record') {
          a.sets.push(t.key)
        }
        if (t.type === 'ForceRecorded') {
          a.sets.push(t.ckey)
          const current = a.forcedSets[t.ckey]
          const entry = { tag: "Recorded", value: t.content }
          a.forcedSets[t.ckey] = current
            ? { ...current, entries: [...current.entries, entry] }
            : { recordable: t.recordable, entries: [entry] }
        }
        if (t.type === 'SetRecordable') {
          a.sets.push(t.ckey)
        }
        if (t.type === 'SetKey') {
          a.keys.push({ key: t.ckey, scope: s.key })
        }
        if (t.type === 'ForceKey') {
          a.keys.push({ key: t.key, scope: s.key })
          a.forcedKeys.push({ key: t.key, scope: s.key })
        }
        if (t.type === 'ChooseKey') {
          t.content.forEach((c) => {
            if(anyForced(campaignLog.value, t)) {
              if (isForcedKey(campaignLog.value, c)) {
                a.keys.push({ key: c.key, scope: s.key })
                a.forcedKeys.push({ key: c.key, scope: s.key })
              }
            } else {
              a.keys.push({ key: c.key, scope: s.key })
            }
          })
        }
        if (t.type === 'Option') {
          a.options.push(t.key)
        }
        if (t.type === 'ChooseNum') {
          a.counts.push(t.key)
        }
        if (t.type === 'ChooseOption') {
          t.content.forEach((o) => a.options.push(o.key))
        }
      }
    })
    return a
  }, { keys: [], sets: [], options: [], counts: [], forcedKeys: [], forcedSets: {} })

  const onlyUnique = (value, index, self) => {
    const idx = self.findIndex((s) =>
      s.scope && value.scope
        ? s.key === value.key && s.scope === value.scope
        : s.key === value.key
    )
    return idx === index
  }

  const keys = [...campaignLog.value.keys, ...active.forcedKeys].
    filter((k) => active.keys.some((a) => a.key === k.key && a.scope == k.scope))

  const options = campaignLog.value.options.
    filter((k) => active.options.includes(k.key))

  const counts = {...Object.fromEntries(Object.entries(campaignLog.value.counts).filter(([k]) => active.counts.includes(k)))}
  const sets =
    Object.entries(active.forcedSets).reduce(
      (current, [setKey, value]) => {
        const currentSet = current[setKey]
        current[setKey] = currentSet
          ? { ...currentSet, entries: [...currentSet.entries, ...value.entries] }
          : { recordable: t.recordable, entries: value.entries }
        return current
      },
      {...Object.fromEntries(Object.entries(campaignLog.value.sets).filter(([k]) => active.sets.includes(k)))}
    )

  campaignLog.value = {
    keys: keys.filter(onlyUnique),
    sets,
    counts,
    options: options.filter(onlyUnique)
  }
}

const setKey = function(step: CampaignScenario, setting: CampaignSetting, key: string) {
  const current = campaignLog.value
  if (setting.type === 'ChooseKey') {
    const keysToRemove = setting.content.filter((k) => k.key !== key).map((k) => k.key)
    campaignLog.value = { ...current, keys: [...current.keys.filter((k) => !keysToRemove.includes(k.key)), {key, scope: step.key }] }
    filterSettings()
  }
}

const setOption = function(setting: CampaignSetting, option: CampaignOption) {
  const current = campaignLog.value
  if (setting.type === 'ChooseOption') {
    const keysToRemove = setting.content.filter((o) => o.key !== option.key).map((o) => o.key)
    campaignLog.value = { ...current, options: [...current.options.filter((o) => !keysToRemove.includes(o.key)), option] }
    filterSettings()
  }
}

const setNum = function(setting: CampaignSetting, value: number) {
  const current = campaignLog.value
  if (setting.type === 'ChooseNum') {
    campaignLog.value = { ...current, counts: {...current.counts, [setting.ckey]: value}}
  }

  filterSettings()
}

const toggleKey = function(step: CampaignScenario, setting: CampaignSetting) {
  const current = campaignLog.value
  if (current.keys.some((k) => k.key === setting.ckey)) {
    current.keys = current.keys.filter((k) => k.key !== setting.ckey)
  } else {
    current.keys.push({ key: setting.ckey, scope: step.key })
  }

  filterSettings()
}

const toggleOption = function(setting: CampaignSetting) {
  if (setting.type !== 'Option') {
    return
  }
  const current = campaignLog.value
  if (current.options.map((o) => o.key).includes(setting.key)) {
    current.options = current.options.filter((o) => o.key !== setting.key)
  } else {
    const option = setting.ckey ? { key: setting.key, ckey: setting.ckey } : { key: setting.key }
    current.options.push(option)
  }

  filterSettings()
}

const toggleSet = function(setting) {
  if(setting.type === 'SetRecordable') {
    const current = campaignLog.value
    const set = current.sets[setting.ckey]
    if (set) {
      const entry = set.entries.find((e) => e.value === setting.content)
      if (entry) {
        set.entries = set.entries.filter((e) => e.value !== setting.content)
      } else {
        set.entries.push({ tag: "Recorded", value: setting.content })
      }

    } else {
      campaignLog.value = { ...current, sets: {...current.sets, [setting.ckey]: { recordable: setting.recordable, entries: [{ tag: "Recorded", value: setting.content }]}}}
    }
  }

  filterSettings()
}

const chooseRecordable = function(setting, value) {
  if(setting.type === 'ChooseRecordable') {
    const current = campaignLog.value
    const set = current.sets[setting.ckey]
    if (set) {
      const keysToRemove = setting.content.filter((k) => k.content !== value).map((k) => k.content)
      set.entries = [...set.entries.filter((e) => !keysToRemove.includes(e.value)), { tag: "Recorded", value }]
    } else {
      campaignLog.value = { ...current, sets: {...current.sets, [setting.ckey]: { recordable: setting.recordable, entries: [{ tag: "Recorded", value }]}}}
    }
  }

  filterSettings()
}

const toggleRecordable = function(setting, value) {
  if(setting.type === 'Record') {
    const current = campaignLog.value
    const set = current.sets[setting.key]
    if (set) {
      const hasEntry = set.entries.some((e) => e.value === value)
      set.entries = hasEntry ? set.entries.filter((e) => e.value !== value) : [...set.entries, { tag: "Recorded", value }]

      campaignLog.value = { ...current, sets: {...current.sets, [setting.key]: set}}
    } else {
      campaignLog.value = { ...current, sets: {...current.sets, [setting.key]: { recordable: setting.recordable, entries: [{ tag: "Recorded", value }]}}}
    }
  }

  filterSettings()
}

const toggleCrossOut = function (key: string, value: string) {
  if (campaignLog.value.sets[key] === undefined) {
    return
  }

  const {entries} = campaignLog.value.sets[key]

  if (entries) {
    entries.map((e) => e.tag = e.value === value ? (e.tag === "Recorded" ? "CrossedOut" : "Recorded") : e.tag)
  }

}


const submit = () => updateCampaignSettings(props.game.id, campaignLog.value)
</script>

<template>
  <div class="container">
    <p>Campaign Settings</p>
    <CampaignScenarioSetting
      v-for="setting in activeSettings"
      :step="setting"
      :campaignLog="campaignLog"
      :key="setting.key"
      @toggle:key="toggleKey"
      @toggle:option="toggleOption"
      @toggle:set="toggleSet"
      @set:key="setKey"
      @toggle:crossout="toggleCrossOut"
      @set:num="setNum"
      @set:record="chooseRecordable"
      @toggle:record="toggleRecordable"
      @set:option="setOption"
    />
    <button @click="submit" :disabled="!completedCampaignSettings">Begin</button>
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
