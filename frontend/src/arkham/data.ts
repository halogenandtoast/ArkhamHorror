import type { Difficulty } from '@/arkham/types/Difficulty'

export type SideStoryGroup = 'chapter1' | 'chapter2' | 'homebrew'

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
  // Which tab of the side-story chooser this belongs under. Declared rather
  // than derived: official side-story ids don't order by chapter the way
  // campaign ids do.
  group?: SideStoryGroup
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

/* Which tab of the side-story chooser a side story sits under. An explicit
 * `group` in its json wins; otherwise a homebrew side story -- `:`-prefixed,
 * contributed by a homebrew box -- is Homebrew and everything else is Chapter 1.
 * Official side-story ids are their own series (`7x`, `8x`, `90xxx`) and don't
 * order against the chapters, so a Chapter 2 side story says so in its entry. */
export function sideStoryGroup(sideStory: { id: string, group?: SideStoryGroup }): SideStoryGroup {
  if (sideStory.group) return sideStory.group
  return sideStory.id.startsWith(':') ? 'homebrew' : 'chapter1'
}

/* The tab to open the side-story chooser on: the one matching the campaign
 * being played. A homebrew campaign is Chapter 1 unless it declares otherwise,
 * so its side stories come from the Chapter 1 pool, not the Homebrew tab. */
export function defaultSideStoryGroup(campaign?: Campaign | null, id?: string | null): SideStoryGroup {
  return campaignChapter(campaign, id) === 2 ? 'chapter2' : 'chapter1'
}
