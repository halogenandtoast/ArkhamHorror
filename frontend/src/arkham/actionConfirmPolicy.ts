/* Which of the player's own choices deserve an "are you sure?".
 *
 * Two things the engine does not stop to ask about:
 * - playing a card from hand: one click and the card is spent;
 * - an attack of opportunity: when an action provokes one, the engine queues
 *   the attack right after the cost is paid (ActiveCost.hs). There is no
 *   question in between, so the only place to warn is here, before the answer
 *   is sent.
 *
 * Only player-window choices are considered: that is where actions and plays
 * are offered. A "choose a card to discard" prompt also shows hand cards as
 * targets and must not ask.
 *
 * The client cannot see every server-side modifier (a card that says "this
 * action does not provoke"), so a provoke is a warning, never a certainty.
 *
 * Import-free so the node tests can load it without the Vue toolchain; tags are
 * matched as the plain strings the backend sends.
 */

type ActionsLike = { tag: string; contents: unknown }

export type ConfirmChoiceLike = {
  tag: string
  investigatorId?: string
  target?: { tag: string; contents?: unknown }
  ability?: { type: { tag: string; actions?: ActionsLike } }
  component?: { tag: string; investigatorId?: string; tokenType?: string }
}

type QuestionLike = { tag: string; question?: QuestionLike; isPlayerWindow?: boolean }

type CardContentsLike = { id: string; cardCode: string }
type CardLike = { tag: string; contents: CardContentsLike | CardLike }

export type ConfirmInvestigatorLike = {
  id: string
  playerId?: string
  hand: readonly CardLike[]
  engagedEnemies: readonly string[]
}

export type ConfirmEnemyLike = { id: string; exhausted: boolean }

export type ActionConfirmInput = {
  question: QuestionLike | null | undefined
  choice: ConfirmChoiceLike | undefined
  playerId: string | null | undefined
  investigators: Readonly<Record<string, ConfirmInvestigatorLike>>
  enemies: Readonly<Record<string, ConfirmEnemyLike>>
  /* English rules text of a card code, or null when unknown. */
  cardText: (cardCode: string) => string | null
  confirmPlays: boolean
  warnAttacksOfOpportunity: boolean
}

export type ActionConfirmRequest = {
  kind: 'play' | 'action'
  investigatorId: string
  /* The card being played, for kind 'play'. */
  cardCode: string | null
  /* Ready enemies engaged with the investigator, when the choice provokes. */
  provokingEnemyIds: string[]
}

// Actions the rules exempt from attacks of opportunity (ActiveCost.hs).
const NON_PROVOKING_ACTIONS = new Set(['Fight', 'Evade', 'Resign', 'Parley'])

function flattenActions(actions: ActionsLike | undefined): string[] {
  if (!actions) return []
  if (actions.tag === 'SingleAction') return [String(actions.contents)]
  if (Array.isArray(actions.contents)) {
    return (actions.contents as ActionsLike[]).flatMap(flattenActions)
  }
  return []
}

function cardContents(card: CardLike): CardContentsLike {
  const inner = card.contents
  return 'id' in inner ? inner : cardContents(inner)
}

/* A Fast card takes no action, and an event whose text opens with Fight. /
 * Evade. / Parley. is played as that action -- neither provokes. Unknown text
 * counts as provoking: a needless warning beats a missed one. */
export function playProvokes(text: string | null): boolean {
  if (!text) return true
  const plain = text.replace(/<\/?b>/g, '').trimStart()
  if (/^Fast\b/i.test(plain)) return false
  if (/^(Fight|Evade|Parley)\./.test(plain)) return false
  return true
}

function readyEngagedEnemies(
  investigator: ConfirmInvestigatorLike,
  enemies: Readonly<Record<string, ConfirmEnemyLike>>,
): string[] {
  return investigator.engagedEnemies.filter((id) => enemies[id] && !enemies[id].exhausted)
}

function ownInvestigatorHolding(
  cardId: string,
  playerId: string,
  investigators: Readonly<Record<string, ConfirmInvestigatorLike>>,
): { investigator: ConfirmInvestigatorLike; cardCode: string } | null {
  for (const investigator of Object.values(investigators)) {
    if (investigator.playerId !== playerId) continue
    const card = investigator.hand.map(cardContents).find((c) => c.id === cardId)
    if (card) return { investigator, cardCode: card.cardCode }
  }
  return null
}

/* An action the investigator takes that is not a play: an action ability
 * (including the basic move / investigate / engage), drawing a card, or taking
 * a resource. Returns the investigator and whether it provokes. */
function actionOf(choice: ConfirmChoiceLike): { investigatorId: string; provokes: boolean } | null {
  if (choice.tag === 'AbilityLabel' && choice.ability?.type.tag === 'ActionAbility' && choice.investigatorId) {
    const actions = flattenActions(choice.ability.type.actions)
    return {
      investigatorId: choice.investigatorId,
      provokes: !actions.some((action) => NON_PROVOKING_ACTIONS.has(action)),
    }
  }
  if (choice.tag === 'ComponentLabel' && choice.component?.investigatorId) {
    const { tag, tokenType, investigatorId } = choice.component
    const isDraw = tag === 'InvestigatorDeckComponent'
    const isResource = tag === 'InvestigatorComponent' && tokenType === 'ResourceToken'
    if (isDraw || isResource) return { investigatorId, provokes: true }
  }
  return null
}

export function decideActionConfirm(input: ActionConfirmInput): ActionConfirmRequest | null {
  const { question, choice, playerId, investigators, enemies } = input
  if (!question || !choice || !playerId) return null
  if (!input.confirmPlays && !input.warnAttacksOfOpportunity) return null
  const inner = question.tag === 'QuestionLabel' ? question.question : question
  if (!inner?.isPlayerWindow) return null

  // Playing a card from hand: offered as a target naming the card's id.
  if (choice.tag === 'TargetLabel' && typeof choice.target?.contents === 'string') {
    const held = ownInvestigatorHolding(choice.target.contents, playerId, investigators)
    if (held) {
      const provoking =
        input.warnAttacksOfOpportunity && playProvokes(input.cardText(held.cardCode))
          ? readyEngagedEnemies(held.investigator, enemies)
          : []
      if (!input.confirmPlays && provoking.length === 0) return null
      return {
        kind: 'play',
        investigatorId: held.investigator.id,
        cardCode: held.cardCode,
        provokingEnemyIds: provoking,
      }
    }
  }

  if (!input.warnAttacksOfOpportunity) return null
  const action = actionOf(choice)
  if (!action?.provokes) return null
  const investigator = investigators[action.investigatorId]
  if (!investigator || investigator.playerId !== playerId) return null
  const provoking = readyEngagedEnemies(investigator, enemies)
  if (provoking.length === 0) return null
  return { kind: 'action', investigatorId: investigator.id, cardCode: null, provokingEnemyIds: provoking }
}
