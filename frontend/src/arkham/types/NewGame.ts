export type GameMode = 'Campaign' | 'SideStory'
export type MultiplayerVariant = 'WithFriends' | 'Solo'
export type CampaignType = 'FullCampaign' | 'PartialCampaign' | 'Standalone' | 'Option1' | 'Option2'
export type CampaignOption = { tag: string, contents?: string }

// The six AI strategic focuses (lowercase wire keys; see Arkham.Ai.Focus). `auto`
// is a UI-only sentinel meaning "no override" and is never sent to the backend.
export type AiFocus = 'combat' | 'investigate' | 'evade' | 'support' | 'survival' | 'mobility'
export const aiFocuses: AiFocus[] = ['combat', 'investigate', 'evade', 'support', 'survival', 'mobility']

// Per-seat AI configuration sent at creation time, parallel to `deckIds` and
// indexed by seat. A `null` entry (or omitting the whole array) = a normal human
// seat. `focus` omitted = "auto" (let the engine pick); `responseDelayMs` omitted
// = backend default (1500).
export type AiSlotConfig = {
  investigator: string
  focus?: AiFocus
  responseDelayMs?: number
}
