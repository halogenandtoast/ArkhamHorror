export type CrossOutContent = { label: string, key: string, content: boolean }
export type RecordableType = 'RecordableCardCode' | 'RecordableMemento'

export type StandaloneSetting =
  { type: "ToggleCrossedOut", key: string, recordable: RecordableType, content: CrossOutContent } |
  { type: "ToggleKey", key: string, content: boolean }
