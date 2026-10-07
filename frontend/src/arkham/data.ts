import type { Difficulty } from '@/arkham/types/Difficulty'

export interface Scenario {
  id: string
  name: string
  returnTo?: string
  returnToName?: string
  beta?: boolean
  alpha?: boolean
  dev?: boolean
  standaloneDifficulties?: Difficulty[]
  standalone?: boolean
  epicMultiplayer?: boolean
  allowCurvedPaths?: boolean
  miniCampaign?: boolean
  returnToVariant?: boolean
  show?: boolean
  requiredInvestigator?: string
  deckRequirements?: string[]
  campaign?: string
  scenarios?: { id: string, name: string, box?: string, notAfter?: string[] }[]
}

export interface Campaign {
  id: string
  name: string
  beta?: boolean
  alpha?: boolean
  dev?: boolean
  homebrew?: boolean
  designer?: string
  // Which chapter's rules the campaign is written against. Official campaigns
  // are derived from their id; homebrew campaigns declare it in campaign.json.
  chapter?: 1 | 2
  settings?: string[]
  /* The campaign plays with the flood rules (The Innsmouth Conspiracy, The Drowned
     City, and any homebrew box that reuses them). Declared here rather than derived
     from the id, so a homebrew campaign gets the flood UI by saying so in its own
     campaign.json. */
  floodRules?: boolean
  returnTo?: {
    id: string
    name: string
    beta?: boolean
    alpha?: boolean
  }
}

/* Whether the flood rules are in play, for UI that only makes sense in a flooded
 * scenario (the debug flood controls). Reads the campaign's declared `floodRules`
 * rather than matching its id, so a homebrew box that reuses the rules opts in from
 * its own campaign.json. Callers resolve the entry themselves -- for a standalone
 * there is no campaign in the game, so the scenario's declared `campaign` names it. */
export function campaignHasFloodRules(campaign?: Campaign | null): boolean {
  return campaign?.floodRules === true
}

/* The chapter whose rules a campaign defaults to (currently only the "as if"
 * ruling). An explicit `chapter` wins; otherwise official campaigns from `11`
 * on are Chapter 2, and everything else — including homebrew campaigns, whose
 * `:`-prefixed ids don't order against official ones — is Chapter 1. */
export function campaignChapter(campaign?: Campaign | null, id?: string | null): 1 | 2 {
  if (campaign?.chapter) return campaign.chapter
  const campaignId = campaign?.id ?? id ?? null
  if (campaignId == null || campaignId.startsWith(':')) return 1
  return campaignId >= '11' ? 2 : 1
}
