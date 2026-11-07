/* Which engine questions are not really questions.
 *
 * The engine stops and asks even when there is nothing to decide: one
 * encounter deck to draw from, a lone "apply results" button after a skill
 * test, a forced effect offered as the only choice. The autopilot answers
 * those so a round flows on its own, and leaves every real decision to the
 * player.
 *
 * Allow-list on purpose: an unknown question shape returns null and stops as
 * it always has, so a new card or scenario can never be auto-played by
 * accident. Import-free so the node tests can load it without the Vue
 * toolchain; tags are matched as the plain strings the backend sends.
 */

export type AutoAnswerReason =
  | 'encounterDraw'
  | 'applyResults'
  | 'startSkillTest'
  | 'singleChoice'
  | 'closeWindow'
  | 'assignDamage'

// The only thing left in a player window that does nothing but close it.
const PLAYER_WINDOW_CLOSERS = new Set(['EndTurnButton', 'Done'])

export type AutoAnswer = { index: number; reason: AutoAnswerReason }

type ChoiceLike = {
  tag: string
  target?: { tag: string }
  component?: { tag: string; investigatorId?: string; tokenType?: string }
}
type QuestionLike = {
  tag: string
  question?: QuestionLike
  isPlayerWindow?: boolean
  label?: string
}

export type AutoAnswerContext = {
  /* Cards are face up for the player to look at (a search, a reveal). The lone
   * "done" under them is how the player says they have seen them. */
  hasRevealedCards?: boolean
}

// Lone choices that resolve something the rules already decided.
const SINGLE_CHOICE_TAGS = new Set([
  'Label',
  'TooltipLabel',
  'CostLabel',
  'TargetLabel',
  'PortraitLabel',
  'CardLabel',
  'ComponentLabel',
  'AuxiliaryComponentLabel',
  'AbilityLabel',
  'SkillLabel',
  'SkillLabelWithLabel',
  'FightLabel',
  'FightLabelWithSkill',
  'EvadeLabel',
  'EvadeLabelWithSkill',
  'EngageLabel',
  'GridLabel',
  'ConnectionLabel',
  'KeyLabel',
  'Done',
  'SkipTriggersButton',
])

// Question shapes that are a plain "pick one of these". Everything else
// (amounts, decks, story passages, campaign pickers, multi-select) is left alone.
const CHOOSE_ONE_TAGS = new Set(['ChooseOne', 'WindowChooseOne', 'ChooseOneAtATime'])

// Wrappers the engine puts around a plain choice: totals label, damage source.
const WRAPPER_TAGS = new Set(['QuestionLabel', 'QuestionWithSource'])

function unwrap(question: QuestionLike): { inner: QuestionLike | null; label: string | null } {
  let current: QuestionLike | undefined = question
  let label: string | null = null
  while (current && WRAPPER_TAGS.has(current.tag)) {
    if (current.tag === 'QuestionLabel' && typeof current.label === 'string') label = label ?? current.label
    current = current.question
  }
  return { inner: current ?? null, label }
}

// "Assign 2 damage and 1 horror": the engine's own prompt for dealing damage.
// Healing offers the same token labels, so the prompt text is what tells them apart.
const ASSIGN_LABEL = /^Assign \d+ (damage|horror)/

/* When the only targets are the one investigator's own damage and horror, the
 * order is irrelevant: it all lands on the same card. Any asset, other
 * investigator or auxiliary label means a real soak decision, so it stops. */
function decideAssignDamage(label: string | null, valid: { choice: ChoiceLike; index: number }[]): AutoAnswer | null {
  if (!label || !ASSIGN_LABEL.test(label) || valid.length === 0) return null
  const owner = valid[0].choice.component?.investigatorId
  const ownOnly = valid.every(
    ({ choice }) =>
      choice.tag === 'ComponentLabel' &&
      choice.component?.tag === 'InvestigatorComponent' &&
      choice.component.investigatorId === owner &&
      (choice.component.tokenType === 'DamageToken' || choice.component.tokenType === 'HorrorToken'),
  )
  if (!ownOnly) return null
  const pick = valid.find(({ choice }) => choice.component?.tokenType === 'DamageToken') ?? valid[0]
  return { index: pick.index, reason: 'assignDamage' }
}

export function decideAutoAnswer(
  question: QuestionLike | null | undefined,
  choices: readonly ChoiceLike[],
  context: AutoAnswerContext = {},
): AutoAnswer | null {
  if (!question) return null
  const { inner, label } = unwrap(question)
  if (!inner || !CHOOSE_ONE_TAGS.has(inner.tag)) return null
  if (context.hasRevealedCards) return null
  // Flavor text sits beside the choice to be read before it is taken.
  if (choices.some((c) => c.tag === 'Info')) return null

  const valid = choices
    .map((choice, index) => ({ choice, index }))
    .filter(({ choice }) => choice.tag !== 'InvalidLabel')
  const assign = decideAssignDamage(label, valid)
  if (assign) return assign
  if (valid.length !== 1) return null

  const { choice, index } = valid[0]
  // A player window is the player's turn or a chance to play something fast.
  // Once nothing but "end turn" / "done" is left, there is nothing to play or
  // do, so the window closes itself; any other lone choice stays the player's.
  if (inner.isPlayerWindow) {
    return PLAYER_WINDOW_CLOSERS.has(choice.tag) ? { index, reason: 'closeWindow' } : null
  }
  if (choice.tag === 'TargetLabel' && choice.target?.tag === 'EncounterDeckTarget') {
    return { index, reason: 'encounterDraw' }
  }
  if (choice.tag === 'SkillTestApplyResultsButton') return { index, reason: 'applyResults' }
  if (choice.tag === 'StartSkillTestButton') return { index, reason: 'startSkillTest' }
  if (SINGLE_CHOICE_TAGS.has(choice.tag)) return { index, reason: 'singleChoice' }
  return null
}

/* How long the board holds still before an automatic answer, so the player can
 * see what is about to happen. Test results get the longest beat: the revealed
 * token and the pass/fail are the thing worth reading. */
export const AUTO_ANSWER_DELAY_MS: Record<AutoAnswerReason, number> = {
  encounterDraw: 450,
  applyResults: 1300,
  startSkillTest: 450,
  singleChoice: 450,
  closeWindow: 600,
  assignDamage: 300,
}
