/* Debug tools that act on an investigator and their campaign: drawing,
 * discarding at random, trauma, XP and free-text campaign log entries.
 *
 * Each builder is the exact JSON the backend's derived decoders take, kept in
 * one place so the shapes are unit-tested rather than scattered through
 * templates. Everything is sent through `debug.send`, so it is synced to every
 * player and undoable like any other step. */

const gameSource = { tag: 'GameSource' }

/* A real draw from the top of the investigator's own deck: draw windows fire,
 * and a weakness drawn this way resolves as it would from any other draw. */
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

/* "+" follows the rules: the engine checks trauma against health and sanity and
 * kills or drives the investigator insane at the limit. "-" heals, floored at 0. */
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

/* Gaining XP reports it to the campaign's XP breakdown first, as card effects
 * do (`gainXp` in Arkham.Message.Lifted), so the breakdown still adds up. */
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

// Spending is already reported as "Spent Xp" and floors at 0 in the engine.
export const spendXpMessage = (investigatorId: string, amount = 1) => ({
  tag: 'SpendXP',
  contents: [investigatorId, amount],
})

/* ---- Campaign log --------------------------------------------------------- */

/* Custom entries are free-text homebrew keys under the `custom.` scope -- the
 * same key type a custom card's `record` step writes -- so they live in the real
 * campaign log (or the standalone one) and survive into later scenarios. */
export const CUSTOM_LOG_SCOPE = 'custom.'

export type CampaignLogKeyJson = { tag: string, contents?: unknown }

export const customLogKey = (text: string): CampaignLogKeyJson => ({
  tag: 'HomebrewCampaignLogKey',
  contents: `${CUSTOM_LOG_SCOPE}${text.trim()}`,
})

/* The text a player typed for a custom entry, or null for any other key. */
export function customLogText(raw: unknown): string | null {
  if (typeof raw !== 'object' || raw === null) return null
  const { tag, contents } = raw as CampaignLogKeyJson
  if (tag !== 'HomebrewCampaignLogKey' || typeof contents !== 'string') return null
  return contents.startsWith(CUSTOM_LOG_SCOPE) ? contents.slice(CUSTOM_LOG_SCOPE.length) : null
}

/* A free-text homebrew key that does not belong to the running homebrew
 * campaign: a custom entry, or one a custom card recorded. These are the
 * entries the debug panel may edit and delete; a homebrew campaign's own keys
 * (`<scope>.Key`) are its campaign log, not a player's notes. */
export function isEditableLogKey(raw: unknown, campaignScope?: string): boolean {
  if (typeof raw !== 'object' || raw === null) return false
  const { tag, contents } = raw as CampaignLogKeyJson
  if (tag !== 'HomebrewCampaignLogKey' || typeof contents !== 'string') return false
  if (contents.startsWith(CUSTOM_LOG_SCOPE)) return true
  return !campaignScope || !contents.startsWith(`${campaignScope}.`)
}

/* How an editable entry reads: the typed text for a custom entry, otherwise the
 * homebrew key's own text. */
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

export const removeLogKeyMessage = (key: CampaignLogKeyJson) => ({
  tag: 'DebugRemoveCampaignLogKey',
  contents: key,
})

// Editing is delete-then-record in one step, so a single undo puts it back.
export const renameNoteMessage = (oldKey: CampaignLogKeyJson, text: string) => ({
  tag: 'Run',
  contents: [removeLogKeyMessage(oldKey), recordMessage(customLogKey(text))],
})

export const renameCountMessage = (oldKey: CampaignLogKeyJson, text: string, value: number) => ({
  tag: 'Run',
  contents: [removeLogKeyMessage(oldKey), recordCountMessage(customLogKey(text), value)],
})
