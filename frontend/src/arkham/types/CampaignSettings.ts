
type RecordableEntry =
  { tag: "Recorded", value: string } |
  { tag: "CrossedOut", value: string }

type RecordableSet = { recordable: string, entries: RecordableEntry[] }

export type CampaignLogSettings = { keys: string[], counts: Record<string, number>, sets: Record<string, RecordableSet> }
