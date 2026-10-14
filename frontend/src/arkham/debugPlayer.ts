/* The exact JSON the backend's derived decoders take, kept in one place so the
 * shapes are unit-tested rather than scattered through templates. */

const gameSource = { tag: 'GameSource' }

export const drawCardsMessage = (investigatorId: string, amount: number) => ({
  tag: 'DrawCards',
  contents: [
    investigatorId,
    {
      cardDrawSource: gameSource,
      cardDrawDeck: { tag: 'InvestigatorDeck', contents: investigatorId },
      cardDrawAmount: Math.max(1, Math.floor(amount)),
      cardDrawState: { tag: 'UnresolvedCardDraw' },
      cardDrawTarget: null,
      cardDrawAction: false,
      cardDrawKind: 'StandardCardDraw',
      cardDrawRules: [],
      cardDrawAndThen: null,
      cardDrawAlreadyDrawn: [],
      cardDrawDiscard: null,
    },
  ],
})

export const discardRandomMessage = (investigatorId: string) => ({
  tag: 'DiscardFromHand',
  contents: {
    discardStrategy: 'DiscardRandom',
    discardInvestigator: investigatorId,
    discardSource: gameSource,
    discardTarget: null,
    discardFilter: { tag: 'AnyCard', contents: [] },
    discardAmount: 1,
    discardThen: null,
    discardDestination: 'ToDiscardPile',
    discardBatchCards: [],
  },
})

export type TraumaKind = 'physical' | 'mental'

const traumaPair = (kind: TraumaKind, amount: number) =>
  kind === 'physical' ? [amount, 0] : [0, amount]

export const sufferTraumaMessage = (investigatorId: string, kind: TraumaKind, amount = 1) => ({
  tag: 'SufferTrauma',
  contents: [investigatorId, ...traumaPair(kind, amount)],
})

export const healTraumaMessage = (investigatorId: string, kind: TraumaKind, amount = 1) => ({
  tag: 'HealTrauma',
  contents: [investigatorId, ...traumaPair(kind, amount)],
})

// Report to the XP breakdown first, as `gainXp` does, so it still adds up.
export const gainXpMessage = (investigatorId: string, amount = 1) => ({
  tag: 'Run',
  contents: [
    {
      tag: 'ReportXp',
      contents: [
        {
          tag: 'InvestigatorGainXp',
          investigator: investigatorId,
          details: { source: 'XpFromCardEffect', sourceName: 'Debug adjustment', amount },
        },
      ],
    },
    { tag: 'GainXP', contents: [investigatorId, gameSource, amount] },
  ],
})

export const spendXpMessage = (investigatorId: string, amount = 1) => ({
  tag: 'SpendXP',
  contents: [investigatorId, amount],
})

/* ---- Campaign log --------------------------------------------------------- */

/* Custom entries are free-text homebrew keys under the `custom.` scope, so they
 * live in the real campaign log and survive into later scenarios. */
export const CUSTOM_LOG_SCOPE = 'custom.'

export type CampaignLogKeyJson = { tag: string, contents?: unknown }

export const customLogKey = (text: string): CampaignLogKeyJson => ({
  tag: 'HomebrewCampaignLogKey',
  contents: `${CUSTOM_LOG_SCOPE}${text.trim()}`,
})

export function customLogText(raw: unknown): string | null {
  if (typeof raw !== 'object' || raw === null) return null
  const { tag, contents } = raw as CampaignLogKeyJson
  if (tag !== 'HomebrewCampaignLogKey' || typeof contents !== 'string') return null
  return contents.startsWith(CUSTOM_LOG_SCOPE) ? contents.slice(CUSTOM_LOG_SCOPE.length) : null
}

/* A homebrew campaign's own keys (`<scope>.Key`) are its campaign log, not a
 * player's notes, so they are not editable. */
export function isEditableLogKey(raw: unknown, campaignScope?: string): boolean {
  if (typeof raw !== 'object' || raw === null) return false
  const { tag, contents } = raw as CampaignLogKeyJson
  if (tag !== 'HomebrewCampaignLogKey' || typeof contents !== 'string') return false
  if (contents.startsWith(CUSTOM_LOG_SCOPE)) return true
  return !campaignScope || !contents.startsWith(`${campaignScope}.`)
}

export function editableLogText(raw: unknown): string {
  const custom = customLogText(raw)
  if (custom !== null) return custom
  const contents = (raw as CampaignLogKeyJson).contents
  return typeof contents === 'string' ? contents : ''
}

export const recordMessage = (key: CampaignLogKeyJson) => ({ tag: 'Record', contents: key })

export const recordCountMessage = (key: CampaignLogKeyJson, value: number) => ({
  tag: 'RecordCount',
  contents: [key, Math.floor(value)],
})

export const forgetRecordMessage = (key: CampaignLogKeyJson) => ({
  tag: 'ForgetRecord',
  contents: key,
})

// Editing is delete-then-record in one step, so a single undo puts it back.
export const renameNoteMessage = (oldKey: CampaignLogKeyJson, text: string) => ({
  tag: 'Run',
  contents: [forgetRecordMessage(oldKey), recordMessage(customLogKey(text))],
})

export const renameCountMessage = (oldKey: CampaignLogKeyJson, text: string, value: number) => ({
  tag: 'Run',
  contents: [forgetRecordMessage(oldKey), recordCountMessage(customLogKey(text), value)],
})
