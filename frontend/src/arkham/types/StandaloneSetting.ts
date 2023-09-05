export type CrossOutContent = { label: string, key: string, content: boolean }
export type RecordedContent = { label: string, key: string, content: boolean, ifRecorded?: SettingCondition[] }
export type RecordableType = 'RecordableCardCode' | 'RecordableMemento'
type Predicate =
  { type: "lte", value: number } |
  { type: "gte", value: number }
export type SettingCondition =
  { type: "key", key: string } |
  { type: "inSet", key: string, recordable: string, content: string } |
  { type: "crossedOut", key: string, recordable: string, content: string } |
  { type: "count", key: string, predicate: Predicate } |
  { type: "option", key: string } |
  { type: "and", content: SettingCondition[] } |
  { type: "or", content: SettingCondition[] } |
  { type: "not", content: SettingCondition } |
  { type: "nor", content: SettingCondition[] }
export type Recordable = { key: string, content: string, ifRecorded?: SettingCondition[]}

export type StandaloneSetting
  = {
      type: "ToggleCrossedOut",
      key: string,
      recordable: RecordableType,
      content: CrossOutContent[],
      ifRecorded?: SettingCondition[]
    }
  | {
      type: "ToggleRecords",
      key: string,
      recordable: RecordableType,
      content: RecordedContent[],
      ifRecorded?: SettingCondition[]
    }
  | {
      type: "ToggleKey",
      key: string,
      content: boolean,
      ifRecorded?: SettingCondition[]
    }
  | {
      type: "ToggleOption",
      key: string,
      content: boolean,
      ifRecorded?: SettingCondition[]
    }
  | {
      type: "PickKey",
      key: string,
      keys: string[],
      content: string,
      ifRecorded?: SettingCondition[]
    }
  | {
      type: "ChooseRecord",
      recordable: RecordableType,
      label: string,
      key: string,
      selected: boolean | null
      content: { key: string }[]
      ifRecorded?: SettingCondition[]
    }
