export type SelectableDeckList = {
  investigator_code: string
  slots: Record<string, number>
  meta?: string | { alternate_front?: string }
}

type Translate = (key: string, named?: Record<string, unknown>) => string

type CampaignDeckRestrictionContext = {
  campaignId?: string | null
  campaignLog?: { recordedSets?: Record<string, unknown[]> } | null
}

type DeckRestrictionContext = CampaignDeckRestrictionContext & {
  scenarioId: string | null | undefined
  deckList: SelectableDeckList
  chosenInvestigatorCodes: string[]
  t?: Translate
}

type DeckRestriction = {
  description?: string | ((t: Translate) => string)
  validate: (context: DeckRestrictionContext) => string | null
}

type RequiredInvestigatorRestriction = DeckRestriction & {
  name: string
  investigatorCodes: Set<string>
}

const NON_WEAKNESS_TOME_ASSET_CODES = new Set([
  '01031', '01035', '01042', '01070', '01531', '01535', '01542', '01570', '01686', '02140',
  '03025', '03154', '04013', '04148', '04307', '05116', '05150', '05188', '05189', '05232',
  '05235', '05316', '06112', '06116', '06158', '06236', '06237', '06238', '06279', '07017',
  '07022', '07151', '07159', '07191', '07192', '08005', '08033', '08038', '08045', '08067',
  '08070', '08072', '08731', '08735', '09044', '09045', '09058', '10013', '10064', '10104',
  '10715', '11005', '11020', '11078', '11084', '11103', '12040', '54004', '60206', '60208',
  '60210', '60222', '60229', '60230', '60233', '60252', '60255', '60256', '60259', '60276',
  '60279', '60280', '60281', '60506', '84034', '90082',
])

const PARALLEL_CONTENT_INVESTIGATOR_CODES = new Set([
  '01001', '01501', '90024', '98004', // Roland Banks
  '01002', '01502', '90001', // Daisy Walker
  '01003', '01503', '90008', // "Skids" O'Toole
  '01004', '01504', '90017', // Agnes Baker
  '01005', '01505', '90037', // Wendy Adams
  '02001', '90059', // Zoey Samaras
  '02002', '90078', // Rex Murphy
  '02003', '90084', '98001', // Jenny Barnes
  '02004', '90049', // Jim Culver
  '02005', '90046', // "Ashcan" Pete
  '03006', '90087', // Lola Hayes
  '04004', '90081', // Father Mateo
  '08007', '90062', // Monterey Jack
])

const challengeScenarioInvestigators: Record<string, RequiredInvestigatorRestriction> = {
  '90004': requiredInvestigator('Daisy Walker', ['01002', '01502', '90001']),
  '90011': requiredInvestigator('"Skids" O\'Toole', ['01003', '01503', '90008']),
  '90020': requiredInvestigator('Agnes Baker', ['01004', '01504', '90017']),
  '90032': requiredInvestigator('Roland Banks', ['01001', '01501', '90024', '98004']),
  '90041': requiredInvestigator('Wendy Adams', ['01005', '01505', '90037']),
  '90054': requiredInvestigator('Jim Culver', ['02004', '90049']),
  '90065': requiredInvestigator('Monterey Jack', ['08007', '90062']),
}

const scenarioDeckRestrictions: Record<string, DeckRestriction[]> = {
  '90004': [minimumCardCount({
    cardCodes: NON_WEAKNESS_TOME_ASSET_CODES,
    minimum: 4,
    label: 'non-weakness Tome assets',
    description: "Daisy Walker's deck must include at least 4 non-weakness Tome assets.",
    error: (count, minimum) => `Daisy Walker's deck must include at least ${minimum} non-weakness Tome assets (${count}/${minimum})`,
  })],
  '90094': [requiresAnyParallelContentInvestigator()],
}

const campaignDeckRestrictions: Record<string, DeckRestriction[]> = {
  '09': [forbiddenCardCodes({
    cardCodes: new Set(['02310']),
    description: (t) => t('deckRestrictions.scarletKeys.redGlovedMan.description'),
    error: (_code, { t }) => t?.('deckRestrictions.scarletKeys.redGlovedMan.error')
      ?? 'The Scarlet Keys campaign cannot include The Red-Gloved Man.',
  })],
}

function requiredInvestigator(name: string, codes: string[]): RequiredInvestigatorRestriction {
  const investigatorCodes = new Set(codes)
  return {
    name,
    investigatorCodes,
    description: `This scenario requires ${name}.`,
    validate: ({ deckList }) => {
      return investigatorCodes.has(deckInvestigatorCode(deckList)) ? null : `This scenario requires ${name}`
    },
  }
}

function requiresAnyParallelContentInvestigator(): DeckRestriction {
  return {
    description: 'At least 1 investigator with parallel content must be chosen.',
    validate: ({ deckList, chosenInvestigatorCodes }) => {
      const hasChosenParallel = chosenInvestigatorCodes.some(hasParallelContent)
      if (hasChosenParallel || hasParallelContent(deckInvestigatorCode(deckList))) return null
      return 'Enthralling Encore requires at least 1 investigator with parallel content'
    },
  }
}

function minimumCardCount({
  cardCodes,
  minimum,
  label,
  description,
  error,
}: {
  cardCodes: Set<string>
  minimum: number
  label: string
  description?: string
  error?: (count: number, minimum: number) => string
}): DeckRestriction {
  return {
    description,
    validate: ({ deckList }) => {
      const count = countDeckSlots(deckList, cardCodes)
      if (count >= minimum) return null
      return error?.(count, minimum) ?? `Deck must include at least ${minimum} ${label} (${count}/${minimum})`
    },
  }
}

function forbiddenCardCodes({
  cardCodes,
  description,
  error,
}: {
  cardCodes: Set<string> | ((context: DeckRestrictionContext) => Set<string>)
  description?: DeckRestriction['description']
  error?: (code: string, context: DeckRestrictionContext) => string
}): DeckRestriction {
  return {
    description,
    validate: (context) => {
      const forbidden = typeof cardCodes === 'function' ? cardCodes(context) : cardCodes
      const invalidCode = Object.entries(context.deckList.slots).find(([code, count]) => {
        return count > 0 && forbidden.has(normalizeCardCode(code))
      })?.[0]

      if (!invalidCode) return null
      const normalizedCode = normalizeCardCode(invalidCode)
      return error?.(normalizedCode, context) ?? `This deck cannot include ${normalizedCode}`
    },
  }
}

function countDeckSlots(deckList: SelectableDeckList, cardCodes: Set<string>): number {
  return Object.entries(deckList.slots).reduce((total, [code, count]) => {
    return total + (cardCodes.has(normalizeCardCode(code)) ? count : 0)
  }, 0)
}

function normalizeCardCode(code: string): string {
  return code.replace(/^c/, '')
}

export function deckInvestigatorCode(deckList: SelectableDeckList): string {
  if (deckList.meta) {
    try {
      const result = typeof deckList.meta === 'string' ? JSON.parse(deckList.meta) : deckList.meta
      if (result?.alternate_front) return normalizeCardCode(result.alternate_front)
    } catch (e) {}
  }
  return normalizeCardCode(deckList.investigator_code)
}

export function deckRequirementDescriptions(
  scenarioId: string | null | undefined,
  campaign?: CampaignDeckRestrictionContext,
  t?: Translate,
): string[] {
  const normalizedScenarioId = scenarioId ? normalizeCardCode(scenarioId) : null
  const normalizedCampaignId = campaign?.campaignId ? normalizeCardCode(campaign.campaignId) : null

  return [
    ...(normalizedScenarioId ? [challengeScenarioInvestigators[normalizedScenarioId]] : []),
    ...(normalizedScenarioId ? (scenarioDeckRestrictions[normalizedScenarioId] ?? []) : []),
    ...(normalizedCampaignId ? (campaignDeckRestrictions[normalizedCampaignId] ?? []) : []),
  ].filter((r): r is DeckRestriction => r !== undefined).flatMap((r) => {
    if (!r.description) return []
    if (typeof r.description === 'function') return t ? [r.description(t)] : []
    return [r.description]
  })
}

export function hasParallelContent(investigatorCode: string): boolean {
  return PARALLEL_CONTENT_INVESTIGATOR_CODES.has(normalizeCardCode(investigatorCode))
}

export type DeckRestrictionOptions = {
  // Whether this is the last player still choosing a deck. Challenge scenarios
  // only need a single player to use the required deck, so the required
  // investigator is only enforced as a last resort when nobody else can
  // provide it. Defaults to true to preserve single-player behaviour.
  isLastPlayer?: boolean
}

export function deckRestrictionError(
  scenarioId: string | null | undefined,
  deckList: SelectableDeckList,
  chosenInvestigatorCodes: string[] = [],
  campaign?: CampaignDeckRestrictionContext,
  t?: Translate,
  options: DeckRestrictionOptions = {},
): string | null {
  const isLastPlayer = options.isLastPlayer ?? true
  const normalizedScenarioId = scenarioId ? normalizeCardCode(scenarioId) : null
  const normalizedCampaignId = campaign?.campaignId ? normalizeCardCode(campaign.campaignId) : null
  const context = { scenarioId, deckList, chosenInvestigatorCodes, ...campaign, t }

  const requiredInvestigatorRestriction = normalizedScenarioId
    ? challengeScenarioInvestigators[normalizedScenarioId]
    : undefined
  const scenarioRestrictions = normalizedScenarioId ? (scenarioDeckRestrictions[normalizedScenarioId] ?? []) : []
  const campaignRestrictions = normalizedCampaignId ? (campaignDeckRestrictions[normalizedCampaignId] ?? []) : []

  if (requiredInvestigatorRestriction) {
    const { investigatorCodes } = requiredInvestigatorRestriction
    const deckIsRequired = investigatorCodes.has(deckInvestigatorCode(deckList))
    const someoneElseIsRequired = chosenInvestigatorCodes.some((code) =>
      investigatorCodes.has(normalizeCardCode(code)),
    )

    if (deckIsRequired) {
      // This deck provides the required investigator, so enforce its deck
      // building restrictions (e.g. Daisy must run 4+ non-weakness Tomes).
      for (const restriction of scenarioRestrictions) {
        const error = restriction.validate(context)
        if (error) return error
      }
    } else if (!someoneElseIsRequired && isLastPlayer) {
      // Only one player needs the required deck. Block a non-required deck
      // solely when this is the last player choosing and nobody else has
      // already provided it.
      const error = requiredInvestigatorRestriction.validate(context)
      if (error) return error
    }
  } else {
    // Non-challenge scenario restrictions (e.g. parallel-content requirement)
    // are group aware and apply to every deck.
    for (const restriction of scenarioRestrictions) {
      const error = restriction.validate(context)
      if (error) return error
    }
  }

  for (const restriction of campaignRestrictions) {
    const error = restriction.validate(context)
    if (error) return error
  }

  return null
}
