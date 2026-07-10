// Achievement catalog ("above the table" per-user accomplishments). Names and
// descriptions live in the i18n scope `achievements.entries.<tag>` (see
// locales/en/gameBoard/achievements.json); this module only carries the typed
// tag -> campaign mapping, mirroring the backend Arkham.Achievement.Types.

// Campaign ids whose official achievement list is implemented backend-side.
export const ACHIEVEMENT_CAMPAIGN_IDS: string[] = ['50', '51']

export type AchievementTag =
  | 'TheZealotsRevenge'
  | 'IDontTrustHer'
  | 'InsuranceDoesntCoverGhouls'
  | 'ButDoIHaveTo'
  | 'ConspiracyOfSilence'
  | 'TourOfArkham'
  | 'DoYouGetItNow'
  | 'PinchHitter'
  | 'EvenDeathMayDie'
  | 'BreakTheCircle'
  | 'TheyreJustMisunderstood'
  | 'UmordhothsFavor'
  | 'ZealotLineInTheSand'
  | 'ArkhamExpertise'
  | 'WhatIsThisStuffAnyway'
  | 'NoVoidForYou'
  | 'FirstRuleOfArkham'
  | 'AllAboard'
  | 'TheGangsAllHere'
  | 'NoBroodLeftBehind'
  | 'RemindMeNotToPissHerOff'
  | 'Eureka'
  | 'BeyondWhatVeil'
  | 'HereWeGoAgain'
  | 'BirdHunting'
  | 'TheyArentGettingAwayWithThis'
  | 'TabulaRasa'
  | 'DunwichLineInTheSand'
  | 'DunwichExpertise'

export type AchievementEntry = { tag: AchievementTag; campaignId: string }

export const achievementCatalog: AchievementEntry[] = [
  { tag: 'TheZealotsRevenge', campaignId: '50' },
  { tag: 'IDontTrustHer', campaignId: '50' },
  { tag: 'InsuranceDoesntCoverGhouls', campaignId: '50' },
  { tag: 'ButDoIHaveTo', campaignId: '50' },
  { tag: 'ConspiracyOfSilence', campaignId: '50' },
  { tag: 'TourOfArkham', campaignId: '50' },
  { tag: 'DoYouGetItNow', campaignId: '50' },
  { tag: 'PinchHitter', campaignId: '50' },
  { tag: 'EvenDeathMayDie', campaignId: '50' },
  { tag: 'BreakTheCircle', campaignId: '50' },
  { tag: 'TheyreJustMisunderstood', campaignId: '50' },
  { tag: 'UmordhothsFavor', campaignId: '50' },
  { tag: 'ZealotLineInTheSand', campaignId: '50' },
  { tag: 'ArkhamExpertise', campaignId: '50' },
  { tag: 'WhatIsThisStuffAnyway', campaignId: '51' },
  { tag: 'NoVoidForYou', campaignId: '51' },
  { tag: 'FirstRuleOfArkham', campaignId: '51' },
  { tag: 'AllAboard', campaignId: '51' },
  { tag: 'TheGangsAllHere', campaignId: '51' },
  { tag: 'NoBroodLeftBehind', campaignId: '51' },
  { tag: 'RemindMeNotToPissHerOff', campaignId: '51' },
  { tag: 'Eureka', campaignId: '51' },
  { tag: 'BeyondWhatVeil', campaignId: '51' },
  { tag: 'HereWeGoAgain', campaignId: '51' },
  { tag: 'BirdHunting', campaignId: '51' },
  { tag: 'TheyArentGettingAwayWithThis', campaignId: '51' },
  { tag: 'TabulaRasa', campaignId: '51' },
  { tag: 'DunwichLineInTheSand', campaignId: '51' },
  { tag: 'DunwichExpertise', campaignId: '51' },
]
