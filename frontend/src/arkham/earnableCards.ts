/* Which cards an investigator can *earn*, and where from.
 *
 * An earnable card is a player-back card that is not a normal player card: a
 * player card type that belongs to an encounter set. That is also the engine's
 * one and only test for "is this a campaign card" -- see `isCampaignCardDef` in
 * `Api/Handler/Arkham/Cards.hs` -- so the rule lives here once and the card
 * browser, the encounter-deck debug and the earned-cards debug all read it from
 * the same place.
 *
 * Nothing maps a campaign or scenario to the cards it hands out: that relation
 * exists only as `addCampaignCardToDeck` calls scattered through the scenario
 * modules. So the grouping below is derived from each card's encounter set plus
 * its card code, both of which every CardDef already carries. */
import type { CardDef } from '@/arkham/types/CardDef'
import type { Game } from '@/arkham/types/Game'
import type { Scenario } from '@/arkham/types/Scenario'
import type { CampaignStep } from '@/arkham/types/CampaignStep'
import { PLAYER_CARD_TYPES } from '@/arkham/customCards'
import * as CardT from '@/arkham/types/Card'
import sideStories from '@/arkham/data/side-stories.json'
import campaigns from '@/arkham/data/campaigns.json'
import { homebrewCampaigns, homebrewSideStories } from '@/arkham/homebrewData'

/* Blood Token is an encounter card that belongs to no encounter set, so the
 * usual test would file it with the player cards. */
export const setlessEncounterCards = new Set(['13119'])

export const isCampaignCard = (card: CardDef) =>
  card.encounterSet != null || setlessEncounterCards.has(card.art)

/* A story asset/event/skill/weakness/enemy -- the things a scenario or interlude
 * can make an investigator the bearer of. */
export const isEarnable = (card: CardDef) =>
  PLAYER_CARD_TYPES.includes(card.cardType) && isCampaignCard(card)

export const bareCardCode = (code: string) => code.replace(/^c/, '')

/* Every encounter card code the scenario has put anywhere on the table, used to
 * work out which encounter sets are in play. */
export function scenarioCardCodes(scenario: Scenario | null): Set<string> {
  if (!scenario) return new Set<string>()

  const decks = Object.values(scenario.encounterDecks).flatMap(([deck, discard]) => [
    ...deck,
    ...discard,
  ])

  return new Set([
    ...scenario.encounterDeck.map((c) => c.cardCode),
    ...scenario.discard.map((c) => c.cardCode),
    ...decks.map((c) => c.cardCode),
    ...scenario.victoryDisplay.map(CardT.asCardCode),
    ...scenario.setAsideCards.map(CardT.asCardCode),
  ])
}

/* The encounter sets the loaded scenario is actually built from, read off the
 * cards it has in play rather than from any table. */
export function scenarioEncounterSets(scenario: Scenario | null, allCards: CardDef[]): Set<string> {
  const codes = scenarioCardCodes(scenario)
  return new Set(
    allCards
      .filter((card) => codes.has(card.cardCode))
      .map((card) => card.encounterSet)
      .filter((set): set is string => set != null),
  )
}

/* The scenario a step runs, for the steps that name one. ScenarioStep carries a
 * bare card code; the other three carry it as the head of a tuple. */
function stepScenarioId(step: CampaignStep | null): string | null {
  if (!step) return null
  switch (step.tag) {
    case 'ScenarioStep':
      return bareCardCode(step.contents)
    case 'ScenarioStepWithOptions':
    case 'StandaloneScenarioStep':
    case 'StandaloneScenarioStepWithOptions':
      return bareCardCode(step.contents[0])
    default:
      return null
  }
}

/* A scenario card carries its own encounter set (`scenarioEncounterSets` in
 * `Arkham/Scenario.hs` puts it there), so a scenario id resolves to a set
 * without any extra data. */
function encounterSetForScenario(scenarioId: string, allCards: CardDef[]): string | null {
  const card = allCards.find((c) => bareCardCode(c.cardCode) === scenarioId)
  return card?.encounterSet ?? null
}

type NamedEntry = { id: string; name: string }

const allSideStories = [...sideStories, ...homebrewSideStories] as NamedEntry[]

const sideStoryNames: Record<string, string> = Object.fromEntries(
  allSideStories.map((s) => [bareCardCode(s.id), s.name]),
)

/* The box a card code belongs to: ":ages-unwound:" for homebrew, the leading
 * two digits otherwise ("13067" -> "13"). */
function codeGroup(code: string): string {
  const homebrew = code.match(/^(:[^:]+:)/)
  return homebrew ? homebrew[1] : code.slice(0, 2)
}

const campaignNames: NamedEntry[] = [
  ...(campaigns as NamedEntry[]),
  ...(homebrewCampaigns as NamedEntry[]),
]

/* Which campaign or side story a card was printed for.
 *
 * Checked campaign-first, so a campaign scenario's cards file under the
 * campaign rather than under that one scenario; a side story has no campaign to
 * belong to, so it is reached by its own scenario card or by sharing a box with
 * one. Nothing records this relation, so it is read off the card code and the
 * encounter set. */
function ownerOf(card: CardDef, setToScenario: Map<string, CardDef>): string | null {
  const code = bareCardCode(card.cardCode)

  const campaign = campaignNames
    .filter((c) => code.startsWith(c.id))
    .sort((a, b) => b.id.length - a.id.length)[0]
  if (campaign) return campaign.name

  const scenario = card.encounterSet ? setToScenario.get(card.encounterSet) : undefined
  if (scenario) {
    const sid = bareCardCode(scenario.cardCode)
    return sideStoryNames[sid] ?? fullTitle(scenario)
  }

  /* A side story's other encounter sets have no scenario card of their own
   * (Curse of the Rougarou's The Bayou, Guardians of the Abyss's Abyssal
   * Gifts), so fall back to the one side story sharing this box. */
  const group = codeGroup(code)
  const sharing = allSideStories.filter((s) => codeGroup(bareCardCode(s.id)) === group)
  if (sharing.length === 1) return sharing[0].name

  return null
}

function fullTitle(card: CardDef): string {
  return card.name.subtitle ? `${card.name.title}: ${card.name.subtitle}` : card.name.title
}

export type EarnableGroup = {
  key: string
  /* i18n key for a fixed heading, or null when `label` is already a name. */
  titleKey: string | null
  label: string | null
  cards: CardDef[]
}

/* Ordered sections for the picker. These are suggestions, not a filter -- a
 * Return-To campaign replays another cycle's scenarios, so its own code prefix
 * under-reports, and "all" is the escape hatch rather than a special case. */
export function earnableGroups(game: Game, allCards: CardDef[]): EarnableGroup[] {
  const earnable = allCards.filter(isEarnable)
  const campaign = game.campaign
  const groups: EarnableGroup[] = []

  const inSets = (sets: Set<string>) =>
    earnable.filter((c) => c.encounterSet != null && sets.has(c.encounterSet))

  const scenarioSets = scenarioEncounterSets(game.scenario, allCards)
  const thisScenario = inSets(scenarioSets)
  if (thisScenario.length > 0) {
    groups.push({ key: 'scenario', titleKey: 'thisScenario', label: null, cards: thisScenario })
  }

  if (campaign) {
    /* The campaign id prefixes its own card codes for official ("13" ->
     * "13067") and homebrew (":ages-unwound" -> ":ages-unwound:040") alike, and
     * it is what picks up the campaign-wide sets -- Infected, Friends in Low
     * Places -- that no scenario card points at. */
    const prefix = campaign.id
    /* Side stories get their own groups below, so they are left out here --
     * otherwise a campaign that spliced two of them in lists their cards
     * twice, under a heading that does not describe them. */
    const playedSets = new Set<string>()
    for (const step of [...(campaign.completedSteps ?? []), campaign.step]) {
      if (step?.tag === 'StandaloneScenarioStep' || step?.tag === 'StandaloneScenarioStepWithOptions') {
        continue
      }
      const sid = stepScenarioId(step)
      if (!sid) continue
      const set = encounterSetForScenario(sid, allCards)
      if (set) playedSets.add(set)
    }

    const thisCampaign = earnable.filter(
      (c) =>
        bareCardCode(c.cardCode).startsWith(prefix) ||
        (c.encounterSet != null && playedSets.has(c.encounterSet)),
    )
    if (thisCampaign.length > 0) {
      groups.push({ key: 'campaign', titleKey: 'thisCampaign', label: null, cards: thisCampaign })
    }

    /* One group per side story already played, named from the side-story data. */
    const sideStorySteps = (campaign.completedSteps ?? []).filter(
      (s) => s.tag === 'StandaloneScenarioStep' || s.tag === 'StandaloneScenarioStepWithOptions',
    )
    for (const step of sideStorySteps) {
      const sid = stepScenarioId(step)
      if (!sid) continue
      const set = encounterSetForScenario(sid, allCards)
      if (!set) continue
      const cards = inSets(new Set([set]))
      if (cards.length === 0) continue
      if (groups.some((g) => g.key === `side:${sid}`)) continue
      groups.push({
        key: `side:${sid}`,
        titleKey: null,
        label: sideStoryNames[sid] ?? encounterSetLabel(set),
        cards,
      })
    }
  }

  return groups
}

/* Encounter sets are raw identifiers -- "TheGathering", or
 * ":against-the-wendigo:hanninah_valley" for homebrew. Turn one into something
 * readable for a heading. */
export function encounterSetLabel(set: string): string {
  const homebrew = set.match(/^:[^:]+:(.*)$/)
  if (homebrew) {
    return homebrew[1]
      .split(/[_-]/)
      .map((w) => w.charAt(0).toUpperCase() + w.slice(1))
      .join(' ')
  }
  /* Two splits, so "APhantomOfTruth" becomes "A Phantom Of Truth" rather than
   * "APhantom Of Truth". */
  return set.replace(/([A-Z])([A-Z][a-z])/g, '$1 $2').replace(/([a-z0-9])([A-Z])/g, '$1 $2')
}

/* Everything earnable, grouped by the campaign or side story it belongs to, for
 * the "view all" list. A card whose box cannot be worked out falls back to its
 * encounter set, which is always something rather than a silent "Other". */
export function allEarnableByOwner(allCards: CardDef[]): EarnableGroup[] {
  const setToScenario = new Map<string, CardDef>()
  for (const card of allCards) {
    if (card.cardType === 'ScenarioType' && card.encounterSet && !setToScenario.has(card.encounterSet)) {
      setToScenario.set(card.encounterSet, card)
    }
  }

  const byOwner = new Map<string, CardDef[]>()
  for (const card of allCards.filter(isEarnable)) {
    const owner = ownerOf(card, setToScenario) ?? encounterSetLabel(card.encounterSet ?? '')
    const existing = byOwner.get(owner)
    if (existing) existing.push(card)
    else byOwner.set(owner, [card])
  }

  return [...byOwner.entries()]
    .map(([owner, cards]) => ({ key: `owner:${owner}`, titleKey: null, label: owner, cards }))
    .sort((a, b) => a.label.localeCompare(b.label))
}
