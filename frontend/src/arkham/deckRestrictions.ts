export type SelectableDeckList = {
  investigator_code: string
  slots: Record<string, number>
  meta?: string | { alternate_front?: string }
}

type DeckRestrictionContext = {
  scenarioId: string | null | undefined
  deckList: SelectableDeckList
}

type DeckRestriction = {
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
    error: (count, minimum) => `Daisy Walker's deck must include at least ${minimum} non-weakness Tome assets (${count}/${minimum})`,
  })],
}

function requiredInvestigator(name: string, codes: string[]): RequiredInvestigatorRestriction {
  const investigatorCodes = new Set(codes)
  return {
    name,
    investigatorCodes,
    validate: ({ deckList }) => {
      return investigatorCodes.has(deckInvestigatorCode(deckList)) ? null : `This scenario requires ${name}`
    },
  }
}

function minimumCardCount({
  cardCodes,
  minimum,
  label,
  error,
}: {
  cardCodes: Set<string>
  minimum: number
  label: string
  error?: (count: number, minimum: number) => string
}): DeckRestriction {
  return {
    validate: ({ deckList }) => {
      const count = countDeckSlots(deckList, cardCodes)
      if (count >= minimum) return null
      return error?.(count, minimum) ?? `Deck must include at least ${minimum} ${label} (${count}/${minimum})`
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

export function deckRestrictionError(scenarioId: string | null | undefined, deckList: SelectableDeckList): string | null {
  if (!scenarioId) return null

  const normalizedScenarioId = normalizeCardCode(scenarioId)
  const restrictions = [
    challengeScenarioInvestigators[normalizedScenarioId],
    ...(scenarioDeckRestrictions[normalizedScenarioId] ?? []),
  ].filter((r): r is DeckRestriction => r !== undefined)

  for (const restriction of restrictions) {
    const error = restriction.validate({ scenarioId, deckList })
    if (error) return error
  }

  return null
}
