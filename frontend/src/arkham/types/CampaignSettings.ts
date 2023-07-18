export type Resolution = { resolution: string, settings: CampaignOption[] }
export type CampaignScenario = { key: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[], settings: CampaignSetting[], resolutions?: Resolution[] }

type RecordableEntry =
  { tag: "Recorded", value: string } |
  { tag: "CrossedOut", value: string }

type RecordableSet = { recordable: string, entries: RecordableEntry[] }

export type CampaignOption = { key: string, ckey?: string }

export type CampaignLogSettings =
  {
    keys: string[],
    counts: Record<string, number>,
    sets: Record<string, RecordableSet>,
    options: CampaignOption[]
  }

type SettingCondition =
  { type: "key", key: string } |
  { type: "inSet", key: string, recordable: string, content: Recordable }

export type Recordable = { key: string, content: string }

export type CampaignSetting =
  { type: "CrossOut", key: string, ckey: string, recordable: string, content: Recordable, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ChooseNum", key: string, ckey: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ChooseKey", key: string, content: string[], ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ForceKey", key: string, content: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[]} |
  { type: "SetKey", key: string, ckey: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "Option", key: string, ckey: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "SetRecordable", key: string, recordable: string, content: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ChooseRecordable", key: string, ckey: string, recordable: string, content: Recordable[], ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] }

export const settingActive = function(campaignLog: CampaignLogSettings, setting: CampaignSetting | CampaignScenario) {
  const {ifRecorded, anyRecorded} = setting
  if (ifRecorded) {
    for (const condition of ifRecorded) {
      if (condition.type === 'key') {
        if (!campaignLog.keys.includes(condition.key)) {
          return false
        }
      } else if (condition.type === 'inSet') {
        const set = campaignLog.sets[condition.key]
        if (!set || !set.entries.find((e) => e.value === condition.content && e.tag === 'Recorded')) {
          return false
        }
      } else if (condition.type === 'count') {
        const count = campaignLog.counts[condition.key]
        if (condition.predicate.type === 'lte') {
          if (count === undefined || count > condition.predicate.value) {
            return false
          }
        } else if (condition.predicate.type === 'gte') {
          if (count === undefined || count < condition.predicate.value) {
            return false
          }
        }
      } else if(condition.type === 'option') {
        if (!campaignLog.options.map((o) => o.key).includes(condition.key)) {
          return false
        }
      }
    }
  }

  if (anyRecorded) {
    let found = false
    for (const condition of anyRecorded) {
      if (condition.type === 'key') {
        if (campaignLog.keys.includes(condition.key)) {
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
