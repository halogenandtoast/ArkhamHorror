// Mirrors AgesUnwound.Key. Group only outcomes that exclude one another.
export const recordRows: { label?: string; keys: string[] }[] = [
  {
    label: 'Night of Fire',
    keys: [
      'YourHuntersFoundANewQuarry',
      'TheInvestigatorsSurvivedTheNightOfFire',
      'TheInvestigatorsSlewTheirStrangeObserver',
      'TheInvestigatorsEscapedTheNightOfFire',
    ],
  },
  {
    label: 'An Unknown Benefactor',
    keys: ['TheInvestigatorsDeclinedAnOfferOfHelp', 'TheInvestigatorsAcceptedAnOfferOfHelp'],
  },
  {
    label: 'The Myriad Gentleman',
    keys: ['TheInvestigatorsLearntOfTheMyriadsRitual', 'TheInvestigatorsFledTheGentlemansManor'],
  },
  {
    label: 'School entrance',
    keys: [
      'TheInvestigatorsUsedTheSchoolsFrontDoor',
      'TheInvestigatorsSnuckIntoTheBackOfTheSchool',
    ],
  },
  {
    label: 'Hound of Unmaking',
    keys: [
      'TheInvestigatorsRepelledTheHoundOfUnmaking',
      'TheInvestigatorsPutDownTheHoundOfUnmaking',
    ],
  },
  {
    label: 'Unstable Warding',
    keys: ['TheInvestigatorsDispelledTheWard', 'TheInvestigatorsUnleashedChaos'],
  },
  {
    label: 'A step through time',
    keys: ['TheInvestigatorsSteppedIntoThePast', 'TheInvestigatorsSteppedIntoTheFuture'],
  },
  {
    label: 'Riddle of the Sphinx',
    keys: ['YouSolvedTheRiddleOfTheSphinx', 'TheMyriadSolvedTheRiddleOfTheSphinx'],
  },
  {
    label: 'Another Realm',
    keys: ['YouTookTeaWithTheRulerOfAStrangeDimension', 'TheMyriadHarnessedThePowerOfAnotherRealm'],
  },
  {
    label: 'Campaign ending',
    keys: ['AllTimesAreOne', 'TheInvestigatorsBoundAforgomonInAPrisonOfTime'],
  },
  {
    keys: ['TheBoundaryIsBroken'],
  },
  {
    keys: ['TheInvestigatorsBrokeTheFirstCircle'],
  },
  {
    keys: ['TheInvestigatorsFellToTheMyriad'],
  },
  {
    keys: ['TheInvestigatorsEliminatedTheHighPriest'],
  },
  {
    keys: ['TheTimelineWasWeakened'],
  },
  {
    keys: ['TheRitualIsNigh'],
  },
  {
    keys: ['TheInvestigatorsViolatedCausality'],
  },
  {
    keys: ['TheMyriadTookControlOfAColourOutOfSpace'],
  },
  {
    keys: ['TheMyriadRaisedAPowerfulWarding'],
  },
  {
    keys: ['TheMyriadRecruitedACruelSorcerer'],
  },
  {
    keys: ['TheMyriadWeavedADreadCurse'],
  },
  {
    keys: ['YouHaveAdvancedTheSchemesOfTheSilverTwilightLodge'],
  },
  {
    keys: ['MustReplayAWorldTornDown'],
  },
  {
    keys: ['MustReplayTimeRunsOut'],
  },
]
export const countKeys = ['StrangeAssistance', 'DamageOnTheHoundOfUnmaking']
export const investigatorKeys = [
  'ReturnedToArkhamLate',
  'StillBearsAforgomonsMark',
  'ExistenceIsWaning',
]
export const timedKeys = [
  'TheBoundaryIsBroken',
  'TheInvestigatorsUsedTheSchoolsFrontDoor',
  'TheInvestigatorsRepelledTheHoundOfUnmaking',
  'TheInvestigatorsPutDownTheHoundOfUnmaking',
  'TheInvestigatorsDispelledTheWard',
  'TheInvestigatorsBrokeTheFirstCircle',
  'TheInvestigatorsFellToTheMyriad',
  'TheInvestigatorsUnleashedChaos',
]

export type RecordKey = { tag: string; contents: string }
export const recordKey = (key: string): RecordKey => ({
  tag: 'HomebrewCampaignLogKey',
  contents: `agesUnwound.${key}`,
})

// Delete all alternatives and their times in the same undoable operation.
export function recordChoiceMessages(
  keys: string[],
  selected: string,
  status: string,
  forget: (key: string) => unknown[],
  removeTimes: (key: string) => unknown[],
): unknown[] {
  const messages: unknown[] = keys.flatMap((key) => [
    ...forget(key),
    ...(key !== selected || status === 'none' ? removeTimes(key) : []),
  ])
  if (selected && status !== 'none') {
    messages.push({ tag: 'Record', contents: recordKey(selected) })
    if (status === 'crossedOut')
      messages.push({ tag: 'CrossOutRecord', contents: recordKey(selected) })
  }
  return messages
}

export interface RecordValidationIssue {
  id: string
  keys: string[]
  reason:
    | 'exclusive'
    | 'backPast'
    | 'frontFuture'
    | 'backHound'
    | 'futureHound'
    | 'frontWard'
    | 'pastWard'
    | 'houndWard'
}

// Scenario III's front-door act places Gateway to the Past; its back-door act
// places A Harnessed Future. Resolution 3 records the matching time destination.
// Backfire may record UnleashedChaos in Scenario VI, so that flag is deliberately
// NOT constrained to Scenario III's back-door branch.
export function validateRecordKeys(recorded: Iterable<string>): RecordValidationIssue[] {
  const active = new Set(recorded)
  const issues: RecordValidationIssue[] = []
  for (const row of recordRows) {
    const keys = row.keys.filter((key) => active.has(key))
    if (keys.length > 1) issues.push({ id: `exclusive:${row.keys[0]}`, keys, reason: 'exclusive' })
  }
  const front = 'TheInvestigatorsUsedTheSchoolsFrontDoor'
  const back = 'TheInvestigatorsSnuckIntoTheBackOfTheSchool'
  const past = 'TheInvestigatorsSteppedIntoThePast'
  const future = 'TheInvestigatorsSteppedIntoTheFuture'
  const ward = 'TheInvestigatorsDispelledTheWard'
  const pairs: [string, string, RecordValidationIssue['reason']][] = [
    [back, past, 'backPast'],
    [front, future, 'frontFuture'],
    [front, ward, 'frontWard'],
    [past, ward, 'pastWard'],
  ]
  for (const hound of [
    'TheInvestigatorsRepelledTheHoundOfUnmaking',
    'TheInvestigatorsPutDownTheHoundOfUnmaking',
  ]) {
    pairs.push(
      [back, hound, 'backHound'],
      [future, hound, 'futureHound'],
      [hound, ward, 'houndWard'],
    )
  }
  for (const [a, b, reason] of pairs) {
    if (active.has(a) && active.has(b)) issues.push({ id: `${a}:${b}`, keys: [a, b], reason })
  }
  return issues
}

// Namespaced and older unprefixed keys describe the same record. Crossed-out
// entries do not participate: the engine reads only active records here too.
export function activeRecordKeys(log: {
  recorded: { raw?: unknown; tag: string; contents?: unknown }[]
}): string[] {
  return log.recorded.flatMap((entry) => {
    const raw = (entry.raw ?? entry) as { tag?: string; contents?: unknown }
    return raw.tag === 'HomebrewCampaignLogKey' && typeof raw.contents === 'string'
      ? [raw.contents.replace(/^agesUnwound\./, '')]
      : []
  })
}
