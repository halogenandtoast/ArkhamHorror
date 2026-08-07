// Achievement catalog ("above the table" per-user accomplishments). Names and
// descriptions live in the i18n scope `achievements.entries.<tag>` (see
// locales/en/gameBoard/achievements.json); this module only carries the typed
// tag -> campaign mapping, mirroring the backend Arkham.Achievement.Types.

// Campaign ids whose official achievement list is implemented backend-side.
export const ACHIEVEMENT_CAMPAIGN_IDS: string[] = ['06', '11', '50', '51', '52', '53', '54']

// Sub-grouping inside a campaign, for campaigns whose achievement list is
// printed per mini-campaign. Only The Dream-Eaters has one: its list is split
// between The Dream-Quest and The Web of Dreams, and both sections are shown
// even when the pair is played as one interconnected 8-part campaign.
// Mirrors the backend achievementCampaignPart.
export type AchievementPart = 'theDreamQuest' | 'theWebOfDreams'

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
  | 'FairWarning'
  | 'FirstSteps'
  | 'CrashingTheParty'
  | 'ForPryingEyes'
  | 'TheCuckoosNest'
  | 'TakeALookAtThis'
  | 'ThePathOfDeath'
  | 'GuessingGame'
  | 'HasturMadeMeDoIt'
  | 'SayMyName'
  | 'GetBackHere'
  | 'ThePathIsFalse'
  | 'ThePathIsReal'
  | 'ThePathIsMine'
  | 'CarcosaLineInTheSand'
  | 'CarcosaExpertise'
  | 'WhyDidItHaveToBeSnakes'
  | 'WatchThemUnravel'
  | 'HopeForHumanity'
  | 'Scenario5What'
  | 'BeyondPerfection'
  | 'IRememberEverything'
  | 'Patricide'
  | 'HesGotAPoint'
  | 'ValusiaSoundsGreat'
  | 'IveBuiltUpAnImmunity'
  | 'WeHaveAnUnderstanding'
  | 'WhoNeedsAnyOfThisJunk'
  | 'DontTreadOnMe'
  | 'BaneOfYig'
  | 'IfICouldTurnBackTime'
  | 'YothExpertise'
  | 'WhoYouGonnaCall'
  | 'SaviorOfHumanity'
  | 'TenOutOfTenWouldReadAgain'
  | 'CarlShmarl'
  | 'TheThreefoldRule'
  | 'NewWorldOrder'
  | 'ImmortalitySoundsNice'
  | 'MoreLikeExcursion'
  | 'MemberThese'
  | 'CaseClosed'
  | 'MusicOfTheOuterGods'
  | 'WeaverOfShadowAndMist'
  | 'FinePrint'
  | 'SpeakTheWordsAloud'
  | 'CircleExpertise'
  | 'OneFirstLastJob'
  | 'SeasonTwo'
  | 'CliffDiver'
  | 'ThisIsACoup'
  | 'ThoroughSearch'
  | 'TidalFlipMinigame'
  | 'NoAcolyteLeftBehind'
  | 'KillTheAdds'
  | 'InTheDeepEnd'
  | 'SorryDidntSeeYouThere'
  | 'SkyRider'
  | 'SkipToTheEnd'
  | 'AlienSchoolDropout'
  | 'AlienSchoolGraduate'
  | 'EmptyHanded'
  | 'WhyWontYouStayDead'
  | 'WithYourPowersCombined'
  | 'Obligations'
  | 'DrownedCityLineInTheSand'
  | 'RlyehExpertise'
  | 'DoYouAlwaysFollowOrders'
  | 'AwwButTheyreSoCute'
  | 'LosingMyReligion'
  | 'FantasyFlightGamesDoesNotCondoneAccomplishingThisAchievement'
  | 'TacticalEspionageAction'
  | 'MoonLizardsIDontBelieveTheyExist'
  | 'BarkhamHorrorEnthusiast'
  | 'OnlyWayToBeSure'
  | 'GiveThemSomethingToTalkAbout'
  | 'ThisIsntEvenMyFinalForm'
  | 'DontTellAnyoneBut'
  | 'DreamQuestLineInTheSand'
  | 'DreamlandsExpertise'
  | 'BewareTheBlackCat'
  | 'ReunitedAndItFeelsSoGood'
  | 'EveryonesAFeministUntilThereIsASpiderAround'
  | 'TheCarterMethod'
  | 'TheDoctorIsIn'
  | 'DejaVu'
  | 'TheCasaLomaManeuver'
  | 'IRememberThisPlace'
  | 'BadAdvice'
  | 'MarchOfTheGhouls'
  | 'TheIshimuraFlex'
  | 'YouSpinMeRightRound'
  | 'MasterOfUnlocking'
  | 'WebOfDreamsLineInTheSand'
  | 'UnderworldExpertise'

export type AchievementEntry = { tag: AchievementTag; campaignId: string; part?: AchievementPart }

export type AchievementSection = { part: AchievementPart | null; entries: AchievementEntry[] }

// Split a campaign's entries into its printed sections. Campaigns without a
// mini-campaign split come back as a single unlabelled section, so callers can
// render sections unconditionally. Relies on the catalog keeping each part's
// entries contiguous, which is also their printed order.
export function achievementSections(entries: AchievementEntry[]): AchievementSection[] {
  const sections: AchievementSection[] = []
  for (const entry of entries) {
    const part = entry.part ?? null
    const last = sections[sections.length - 1]
    if (last && last.part === part) last.entries.push(entry)
    else sections.push({ part, entries: [entry] })
  }
  return sections
}

// Cross-playthrough checklist achievements: item keys mirror the backend
// achievementChecklist (Arkham.Achievement.Types); the earned row's progress
// column holds the checked keys. Names live at
// achievements.entries.<tag>.items.<key>.
export const achievementChecklists: Partial<Record<AchievementTag, string[]>> = {
  TheGangsAllHere: [
    'DrHenryArmitage',
    'ZebulonWhateley',
    'DrFrancisMorgan',
    'EarlSawyer',
    'ProfessorWarrenRice',
  ],
  FirstSteps: [
    'ConstanceDumaine',
    'SebastienMoreau',
    'JordanPerry',
    'AshleighClarke',
    'IshimaruHaruko',
  ],
  // Printed checklist order (two-column card layout).
  MemberThese: [
    'MesmerizingFlute',
    'KeziahsFormulae',
    'RitualComponents',
    'WornCrucifix',
    'ScrapOfTornShadow',
    'WispOfSpectralMist',
    'StrangeIncantation',
    'CornHuskDoll',
    'GilmansJournal',
    'BloodyTreeCarvings',
  ],
  CaseClosed: [
    'ValentinoRivas',
    'GavriellaMizrah',
    'PennyWhite',
    'JeromeDavids',
  ],
  WithYourPowersCombined: [
    'BarrierNode',
    'GrislyMask',
    'ObsidianClaw',
    'TidalTablet',
    'ShardOfYchlecht',
    'HorrorInClay',
  ],
  Obligations: [
    'WalkInFaith',
    'DreamsOfDestruction',
    'ToeTheLine',
    'DoNoHarm',
    'GoodMoney',
    'NoPlaceLikeHome',
    'ProveYourWorth',
    'PlumbTheDepths',
  ],
}

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
  { tag: 'FairWarning', campaignId: '52' },
  { tag: 'FirstSteps', campaignId: '52' },
  { tag: 'CrashingTheParty', campaignId: '52' },
  { tag: 'ForPryingEyes', campaignId: '52' },
  { tag: 'TheCuckoosNest', campaignId: '52' },
  { tag: 'TakeALookAtThis', campaignId: '52' },
  { tag: 'ThePathOfDeath', campaignId: '52' },
  { tag: 'GuessingGame', campaignId: '52' },
  { tag: 'HasturMadeMeDoIt', campaignId: '52' },
  { tag: 'SayMyName', campaignId: '52' },
  { tag: 'GetBackHere', campaignId: '52' },
  { tag: 'ThePathIsFalse', campaignId: '52' },
  { tag: 'ThePathIsReal', campaignId: '52' },
  { tag: 'ThePathIsMine', campaignId: '52' },
  { tag: 'CarcosaLineInTheSand', campaignId: '52' },
  { tag: 'CarcosaExpertise', campaignId: '52' },
  { tag: 'WhyDidItHaveToBeSnakes', campaignId: '53' },
  { tag: 'WatchThemUnravel', campaignId: '53' },
  { tag: 'HopeForHumanity', campaignId: '53' },
  { tag: 'Scenario5What', campaignId: '53' },
  { tag: 'BeyondPerfection', campaignId: '53' },
  { tag: 'IRememberEverything', campaignId: '53' },
  { tag: 'Patricide', campaignId: '53' },
  { tag: 'HesGotAPoint', campaignId: '53' },
  { tag: 'ValusiaSoundsGreat', campaignId: '53' },
  { tag: 'IveBuiltUpAnImmunity', campaignId: '53' },
  { tag: 'WeHaveAnUnderstanding', campaignId: '53' },
  { tag: 'WhoNeedsAnyOfThisJunk', campaignId: '53' },
  { tag: 'DontTreadOnMe', campaignId: '53' },
  { tag: 'BaneOfYig', campaignId: '53' },
  { tag: 'IfICouldTurnBackTime', campaignId: '53' },
  { tag: 'YothExpertise', campaignId: '53' },
  { tag: 'WhoYouGonnaCall', campaignId: '54' },
  { tag: 'SaviorOfHumanity', campaignId: '54' },
  { tag: 'TenOutOfTenWouldReadAgain', campaignId: '54' },
  { tag: 'CarlShmarl', campaignId: '54' },
  { tag: 'TheThreefoldRule', campaignId: '54' },
  { tag: 'NewWorldOrder', campaignId: '54' },
  { tag: 'ImmortalitySoundsNice', campaignId: '54' },
  { tag: 'MoreLikeExcursion', campaignId: '54' },
  { tag: 'MemberThese', campaignId: '54' },
  { tag: 'CaseClosed', campaignId: '54' },
  { tag: 'MusicOfTheOuterGods', campaignId: '54' },
  { tag: 'WeaverOfShadowAndMist', campaignId: '54' },
  { tag: 'FinePrint', campaignId: '54' },
  { tag: 'SpeakTheWordsAloud', campaignId: '54' },
  { tag: 'CircleExpertise', campaignId: '54' },
  { tag: 'OneFirstLastJob', campaignId: '11' },
  { tag: 'SeasonTwo', campaignId: '11' },
  { tag: 'CliffDiver', campaignId: '11' },
  { tag: 'ThisIsACoup', campaignId: '11' },
  { tag: 'ThoroughSearch', campaignId: '11' },
  { tag: 'TidalFlipMinigame', campaignId: '11' },
  { tag: 'NoAcolyteLeftBehind', campaignId: '11' },
  { tag: 'KillTheAdds', campaignId: '11' },
  { tag: 'InTheDeepEnd', campaignId: '11' },
  { tag: 'SorryDidntSeeYouThere', campaignId: '11' },
  { tag: 'SkyRider', campaignId: '11' },
  { tag: 'SkipToTheEnd', campaignId: '11' },
  { tag: 'AlienSchoolDropout', campaignId: '11' },
  { tag: 'AlienSchoolGraduate', campaignId: '11' },
  { tag: 'EmptyHanded', campaignId: '11' },
  { tag: 'WhyWontYouStayDead', campaignId: '11' },
  { tag: 'WithYourPowersCombined', campaignId: '11' },
  { tag: 'Obligations', campaignId: '11' },
  { tag: 'DrownedCityLineInTheSand', campaignId: '11' },
  { tag: 'RlyehExpertise', campaignId: '11' },
  { tag: 'DoYouAlwaysFollowOrders', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'AwwButTheyreSoCute', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'LosingMyReligion', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'FantasyFlightGamesDoesNotCondoneAccomplishingThisAchievement', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'TacticalEspionageAction', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'MoonLizardsIDontBelieveTheyExist', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'BarkhamHorrorEnthusiast', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'OnlyWayToBeSure', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'GiveThemSomethingToTalkAbout', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'ThisIsntEvenMyFinalForm', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'DontTellAnyoneBut', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'DreamQuestLineInTheSand', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'DreamlandsExpertise', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'BewareTheBlackCat', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'ReunitedAndItFeelsSoGood', campaignId: '06', part: 'theDreamQuest' },
  { tag: 'EveryonesAFeministUntilThereIsASpiderAround', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'TheCarterMethod', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'TheDoctorIsIn', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'DejaVu', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'TheCasaLomaManeuver', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'IRememberThisPlace', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'BadAdvice', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'MarchOfTheGhouls', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'TheIshimuraFlex', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'YouSpinMeRightRound', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'MasterOfUnlocking', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'WebOfDreamsLineInTheSand', campaignId: '06', part: 'theWebOfDreams' },
  { tag: 'UnderworldExpertise', campaignId: '06', part: 'theWebOfDreams' },
]
