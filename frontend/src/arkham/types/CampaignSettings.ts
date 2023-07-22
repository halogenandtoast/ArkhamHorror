export type Resolution = { resolution: string, settings: CampaignOption[] }

export type CampaignScenario = {
  key: string,
  ifRecorded?: SettingCondition[],
  anyRecorded?: SettingCondition[],
  settings: CampaignSetting[],
  resolutions?: Resolution[],
  force?: { scenarioId: string }
}

type RecordableEntry =
  { tag: "Recorded", value: string } |
  { tag: "CrossedOut", value: string }

type RecordableSet = { recordable: string, entries: RecordableEntry[] }

export type CampaignOption = { key: string, ckey?: string }

export type Key = { key: string, scope: string }

export type CampaignLogSettings =
  {
    keys: Key[],
    counts: Record<string, number>,
    sets: Record<string, RecordableSet>,
    options: CampaignOption[]
  }

type Predicate =
  { type: "lte", value: number } |
  { type: "gte", value: number }

type SettingCondition =
  { type: "key", key: string } |
  { type: "inSet", key: string, recordable: string, content: string } |
  { type: "count", key: string, predicate: Predicate } |
  { type: "option", key: string } |
  { type: "and", content: SettingCondition[] } |
  { type: "or", content: SettingCondition[] } |
  { type: "not", content: SettingCondition } |
  { type: "nor", content: SettingCondition[] }

export type Recordable = { key: string, content: string }

export type ForceKey = { type: "key", key: string, scope?: string } | { type: "or", content: ForceKey[] } | { type: "and", content: ForceKey[] } | { type: "always" }


export type ChooseKey = { key: string, forceWhen?: ForceKey }
export type ChooseOption = { key: string }

export type CampaignSetting =
  { type: "CrossOut", key: string, ckey: string, recordable: string, content: Recordable, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ChooseNum", key: string, ckey: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ChooseKey", key: string, content: ChooseKey[], ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[], max?: number, min?: number } |
  { type: "ForceKey", key: string, content: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[]} |
  { type: "SetKey", key: string, ckey: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "Option", key: string, ckey: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ChooseOption", key: string, content: ChooseOption[], ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "SetRecordable", key: string, recordable: string, content: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ChooseRecordable", key: string, ckey: string, recordable: string, content: Recordable[], ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "Record", key: string, ckey: string, recordable: string, content: Recordable, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ForceRecorded", key: string, ckey: string, recordable: string, content: Recordable, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] }

const inactiveCondition = (campaignLog: CampaignLogSettings, condition: SettingCondition): boolean => {
  if (condition.type === 'key') {
    return !campaignLog.keys.some((c) => c.key === condition.key)
  }

  if (condition.type === 'inSet') {
    const set = campaignLog.sets[condition.key]
    return !set || !set.entries.find((e) => e.value === condition.content && e.tag === 'Recorded')
  }

  if (condition.type === 'count') {
    const count = campaignLog.counts[condition.key]
    if (condition.predicate.type === 'lte') {
      return count === undefined || count > condition.predicate.value
    }

    if (condition.predicate.type === 'gte') {
      return count === undefined || count < condition.predicate.value
    }
  }

  if(condition.type === 'option') {
    return !campaignLog.options.map((o) => o.key).includes(condition.key)
  }

  if(condition.type === 'and') {
    return condition.content.some((c) => inactiveCondition(campaignLog, c))
  }

  if(condition.type === 'or') {
    return condition.content.every((c) => inactiveCondition(campaignLog, c))
  }

  if(condition.type === 'not') {
    return !inactiveCondition(campaignLog, condition.content)
  }

  if(condition.type === 'nor') {
    return !condition.content.every((c) => inactiveCondition(campaignLog, c))
  }

  throw new Error(`Unknown condition type ${condition}`)
}

export const settingActive = function(campaignLog: CampaignLogSettings, setting: CampaignSetting | CampaignScenario) {
  if (setting === undefined) {
    return false
  }
  const {ifRecorded, anyRecorded} = setting
  if (ifRecorded) {
    if (ifRecorded.some((cond) => inactiveCondition(campaignLog, cond))) {
      return false
    }
  }

  if (anyRecorded) {
    let found = false
    for (const condition of anyRecorded) {
      if (condition.type === 'key') {
        if (campaignLog.keys.some((c) => c.key === condition.key)) {
          found = true
        }
      } else if (condition.type === 'inSet') {
        const set = campaignLog.sets[condition.key]
        if (set && set.entries.find((e) => e.value === condition.content)) {
          found = true
        }
      }
    }

    if (!found) {
      return false
    }
  }

  return true
}

export const completedCampaignScenarioSetting = (campaignLog: CampaignLogSettings, setting: CampaignScenario) => {
  return setting.settings.every((s) => {
    if(!settingActive(campaignLog, s)) {
      return true
    }

    if (s.type === "ChooseNum") {
      return campaignLog.counts[s.key] !== undefined
    }

    if (s.type === "ChooseKey") {
      return s.content.some((k) => campaignLog.keys.some((c) => c.key === k.key))
    }

    return true
  })
}

const forcedWhen = (campaignLog: CampaignLogSettings, forceWhen: ForceKey): boolean => {
  if (forceWhen.type === "key") {
    return campaignLog.keys.some((c) => {
      if (forceWhen.scope) {
        return c.key === forceWhen.key && c.scope === forceWhen.scope
      } else {
        return c.key === forceWhen.key
      }
    })
  }

  if (forceWhen.type === "or") {
    return forceWhen.content.some((f) => forcedWhen(campaignLog, f))
  }

  if (forceWhen.type === "and") {
    return forceWhen.content.every((f) => forcedWhen(campaignLog, f))
  }

  if (forceWhen.type === "always") {
    return true
  }

  return false
}

export const isForcedKey = (campaignLog: CampaignLogSettings, option: ChooseKey) => {
  const {forceWhen} = option
  if (forceWhen) {
    return forcedWhen(campaignLog, forceWhen)
  }
  return false
}

export const anyForced = (campaignLog: CampaignLogSettings, option : CampaignSetting) => {
  if (option.type === "ChooseKey") {
    return option.content.some((o) => isForcedKey(campaignLog, o))
  }

  return false
}
