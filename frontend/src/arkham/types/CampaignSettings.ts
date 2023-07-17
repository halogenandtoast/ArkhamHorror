
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
