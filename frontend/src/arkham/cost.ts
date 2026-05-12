// Renders a backend `Cost` value into a localized human-readable string.
// Mirrors what the (now-removed) Haskell `displayCostType` produced. Each
// branch maps onto an i18n key in `label.cost.*` (see locale files); unknown tags
// fall back to a literal "X" so a missing case never breaks the UI.
import type { Cost } from '@/arkham/types/Cost'

type Translate = (key: string, params?: Record<string, unknown>) => string

const get = <T = unknown>(c: Cost, key: string): T | undefined =>
  (c as unknown as Record<string, unknown>)[key] as T | undefined

const num = (c: Cost, key: string): number => Number(get(c, key) ?? 0)

const join = (parts: string[], separator: string): string =>
  parts.filter((p) => p.length > 0).join(separator)

// Multi-arg costs serialize `contents` as a positional tuple. Pull out element N as an int.
const intAt = (c: Cost, idx: number): number => {
  const contents = get<unknown[]>(c, 'contents')
  return Array.isArray(contents) ? Number(contents[idx] ?? 0) : 0
}

// Extract a numeric count from a GameValue (Static n / PerPlayer n / ...).
// Non-Static variants are shown with their literal count; the per-player nuance is
// dropped here, but the magnitude is correct for most card text.
const gameValueCount = (gv: unknown): number => {
  if (!gv || typeof gv !== 'object') return 0
  const v = gv as { tag?: string; contents?: unknown }
  if (v.tag === 'Static' || v.tag === 'PerPlayer') return Number(v.contents) || 0
  return 0
}

const useTypeLabel = (useType: unknown): string => {
  if (typeof useType !== 'string') return ''
  return useType
}

export function formatCost(cost: Cost, t: Translate): string {
  const tag = cost.tag

  switch (tag) {
    case 'Free':
      return t('label.cost.free')
    case 'ActionCost': {
      const n = num(cost, 'contents')
      return t('label.cost.action', { count: n })
    }
    case 'AdditionalActionCost':
    case 'AdditionalActionsCost':
    case 'AdditionalActionsCostThatReducesResourceCostBy':
      return t('label.cost.additionalAction')
    case 'ResourceCost':
      return t('label.cost.resource', { count: num(cost, 'contents') })
    case 'ScenarioResourceCost':
      return t('label.cost.scenarioResource', { count: num(cost, 'contents') })
    case 'ClueCostX':
      return t('label.cost.clueX')
    case 'ConcealedXCost':
      return t('label.cost.concealedX')
    case 'UnpayableCost':
      return t('label.cost.unpayable')
    case 'ExhaustCost':
      return t('label.cost.exhaust')
    case 'ExhaustAssetCost':
      return t('label.cost.exhaustAsset')
    case 'RemoveCost':
      return t('label.cost.remove')
    case 'RevealCost':
      return t('label.cost.reveal')
    case 'DiscardCardCost':
      return t('label.cost.discardCard')
    case 'DiscardRandomCardCost':
      return t('label.cost.discardRandomCard')
    case 'DiscardDrawnCardCost':
      return t('label.cost.discardDrawnCard')
    case 'DiscardHandCost':
      return t('label.cost.discardHand')
    case 'DiscardAssetCost':
      return t('label.cost.discardAsset')
    case 'ReturnAssetToHandCost':
    case 'ReturnMatchingAssetToHandCost':
      return t('label.cost.returnAssetToHand')
    case 'ReturnEventToHandCost':
      return t('label.cost.returnEventToHand')
    case 'ExileCost':
      return t('label.cost.exile')
    case 'SpendTokenCost':
      return t('label.cost.spendToken')
    case 'SealChaosTokenCost':
      return t('label.cost.sealToken')
    case 'XCost': {
      const inner = get<Cost>(cost, 'contents')
      return inner ? t('label.cost.x', { inner: formatCost(inner, t) }) : t('label.cost.x', { inner: '' })
    }
    case 'OneOfDistanceCost': {
      const contents = get<unknown[]>(cost, 'contents')
      const inner = Array.isArray(contents) ? (contents[1] as Cost | undefined) : undefined
      return inner ? t('label.cost.x', { inner: formatCost(inner, t) }) : t('label.cost.x', { inner: '' })
    }
    case 'OptionalCost': {
      const inner = get<Cost>(cost, 'contents')
      return inner ? t('label.cost.optional', { inner: formatCost(inner, t) }) : ''
    }
    case 'NonBlankedCost': {
      const inner = get<Cost>(cost, 'contents')
      return inner ? formatCost(inner, t) : ''
    }
    case 'CostWhenEnemy':
    case 'CostWhenTreachery':
    case 'CostOnlyWhen':
    case 'AsIfAtLocationCost': {
      const contents = get<unknown[]>(cost, 'contents')
      const inner = Array.isArray(contents) ? (contents[1] as Cost | undefined) : undefined
      return inner ? formatCost(inner, t) : ''
    }
    case 'CostWhenTreacheryElse':
    case 'CostIfEnemy':
    case 'CostIfCustomization':
    case 'CostIfRemembered': {
      const contents = get<unknown[]>(cost, 'contents')
      const inner = contents && contents.length > 0 ? (contents[contents.length - 1] as Cost) : undefined
      return inner ? formatCost(inner, t) : ''
    }
    case 'Costs': {
      const inner = get<Cost[]>(cost, 'contents') ?? []
      return join(inner.map((c) => formatCost(c, t)), t('label.cost.separator'))
    }
    case 'OrCost': {
      const inner = get<Cost[]>(cost, 'contents') ?? []
      return join(inner.map((c) => formatCost(c, t)), t('label.cost.or'))
    }
    case 'DamageCost':
      return t('label.cost.damage', { count: intAt(cost, 2) })
    case 'DirectDamageCost':
      return t('label.cost.directDamage', { count: intAt(cost, 2) })
    case 'InvestigatorDamageCost':
    case 'EachInvestigatorDamageCost': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      const strategy = (contents[2] as { tag?: string } | undefined)?.tag
      const count = Number(contents[3] ?? 0)
      if (strategy === 'DamageDirect') {
        return t('label.cost.directDamage', { count })
      }
      return t('label.cost.damage', { count })
    }
    case 'DirectHorrorCost':
      return t('label.cost.directHorror', { count: intAt(cost, 2) })
    case 'HorrorCost':
      return t('label.cost.horror', { count: intAt(cost, 2) })
    case 'HorrorCostX':
      return t('label.cost.horrorX')
    case 'DirectDamageAndHorrorCost':
      return t('label.cost.directDamageAndHorror', {
        damage: intAt(cost, 2),
        horror: intAt(cost, 3),
      })
    case 'DoomCost':
      return t('label.cost.doom', { count: intAt(cost, 2) })
    case 'LabeledCost': {
      const contents = get<unknown[]>(cost, 'contents')
      return typeof contents?.[0] === 'string' ? (contents[0] as string) : ''
    }
    case 'SkillTestCost': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      const skillType = contents[1] as string | undefined
      const gameValue = contents[2] as { tag: string; contents?: unknown } | undefined
      const skill = skillType?.replace(/^Skill/, '').toLowerCase() ?? ''
      const count = gameValue?.tag === 'Fixed' ? Number(gameValue.contents) : 0
      return t('label.test', { skill: `{${skill}}`, count })
    }
    case 'UnlessFastActionCost':
      return t('label.cost.action', { count: num(cost, 'contents') })
    case 'IncreaseCostOfThis':
      return t('label.cost.increaseCostOfThis', { count: intAt(cost, 1) })
    case 'CostToEnterUnrevealed': {
      const inner = get<Cost>(cost, 'contents')
      return inner ? t('label.cost.costToEnterUnrevealed', { inner: formatCost(inner, t) }) : ''
    }
    case 'ClueCost': {
      const gv = get(cost, 'contents')
      return t('label.cost.clue', { count: gameValueCount(gv) })
    }
    case 'AssetClueCost': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      return t('label.cost.clue', { count: gameValueCount(contents[2]) })
    }
    case 'GroupClueCostX':
      return t('label.cost.groupClueX')
    case 'GroupClueCost':
    case 'SameLocationGroupClueCost': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      const count = gameValueCount(contents[0])
      return t(
        tag === 'SameLocationGroupClueCost' ? 'label.cost.sameLocationGroupClue' : 'label.cost.groupClue',
        { count }
      )
    }
    case 'GroupClueCostRange': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      const range = (contents[0] as [number, number] | undefined) ?? [0, 0]
      return t('label.cost.groupClueRange', { min: range[0], max: range[1] })
    }
    case 'GroupResourceCost': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      return t('label.cost.groupResource', { count: gameValueCount(contents[0]) })
    }
    case 'GroupDiscardCost': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      return t('label.cost.groupDiscard', { count: gameValueCount(contents[0]) })
    }
    case 'DiscoveredCluesCost':
      return t('label.cost.discoveredClues')
    case 'PlaceClueOnLocationCost':
      return t('label.cost.placeClueOnLocation', { count: num(cost, 'contents') })
    case 'ShuffleTopOfScenarioDeckIntoYourDeck':
      return t('label.cost.shuffleTopOfScenarioDeck', { count: intAt(cost, 0) })
    case 'ChooseEnemyCost':
    case 'ChosenEnemyCost':
    case 'ChooseEnemyCostAndMaybeFieldClueCost':
    case 'ChooseEnemyCostAndMaybeGroupFieldClueCost':
      return t('label.cost.chooseEnemy')
    case 'ChooseExtendedCardCost':
    case 'ChosenCardCost':
      return t('label.cost.chooseCard')
    case 'ExhaustXAssetCost':
      return t('label.cost.exhaustXAsset')
    case 'PlaceKeyCost':
      return t('label.cost.placeKey')
    case 'SpendKeyCost':
      return t('label.cost.spendKey')
    case 'SpendTokenKeyCost':
      return t('label.cost.spendTokenKey', { count: intAt(cost, 0) })
    case 'GroupSpendKeyCost':
      return t('label.cost.groupSpendKey')
    case 'DiscardCost':
      return t('label.cost.discard')
    case 'DiscardTopOfDeckCost':
      return t('label.cost.discardTopOfDeck', { count: num(cost, 'contents') })
    case 'DiscardTopOfDeckWithTargetCost':
      return t('label.cost.discardTopOfDeck', { count: intAt(cost, 1) })
    case 'DiscardUnderneathCardCost':
      return t('label.cost.discardUnderneath')
    case 'DiscardFromCost':
      return t('label.cost.discardFrom', { count: intAt(cost, 0) })
    case 'DiscardCombinedCost':
      return t('label.cost.discardCombined', { count: num(cost, 'contents') })
    case 'ShuffleDiscardCost':
      return t('label.cost.shuffleDiscard', { count: intAt(cost, 0) })
    case 'EnemyDoomCost':
      return t('label.cost.enemyDoom', { count: intAt(cost, 0) })
    case 'EnemyAttackCost':
      return t('label.cost.enemyAttack')
    case 'RemoveEnemyDamageCost':
      return t('label.cost.removeEnemyDamage')
    case 'HandDiscardCost':
      return t('label.cost.discardHandCount', { count: intAt(cost, 0) })
    case 'HandDiscardAnyNumberCost':
      return t('label.cost.discardHandAny')
    case 'SkillIconCost':
      return t('label.cost.skillIcon', { count: intAt(cost, 0) })
    case 'SkillIconCostMatching':
      return t('label.cost.skillIcon', { count: intAt(cost, 0) })
    case 'SameSkillIconCost':
      return t('label.cost.sameSkillIcon', { count: num(cost, 'contents') })
    case 'SameSkillIconCostMatching':
      return t('label.cost.sameSkillIcon', { count: intAt(cost, 0) })
    case 'CalculatedResourceCost':
      return t('label.cost.calculatedResource')
    case 'CalculatedHandDiscardCost':
      return t('label.cost.calculatedHandDiscard')
    case 'FieldResourceCost':
    case 'MaybeFieldResourceCost':
      return t('label.cost.fieldResource')
    case 'UseCost':
    case 'EventUseCost': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      return t('label.cost.use', { count: Number(contents[2] ?? 0), useType: useTypeLabel(contents[1]) })
    }
    case 'AllUsesCost': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      return t('label.cost.useAll', { useType: useTypeLabel(contents[1]) })
    }
    case 'DynamicUseCost': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      return t('label.cost.dynamicUses', { useType: useTypeLabel(contents[1]) })
    }
    case 'UseCostUpTo': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      return t('label.cost.useUpTo', {
        min: Number(contents[2] ?? 0),
        max: Number(contents[3] ?? 0),
        useType: useTypeLabel(contents[1]),
      })
    }
    case 'UpTo': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      const inner = contents[1] as Cost | undefined
      return inner ? t('label.cost.upTo', { inner: formatCost(inner, t) }) : ''
    }
    case 'AtLeastOne': {
      const contents = get<unknown[]>(cost, 'contents') ?? []
      const inner = contents[1] as Cost | undefined
      return inner ? t('label.cost.atLeastOne', { inner: formatCost(inner, t) }) : ''
    }
    case 'SealCost':
      return t('label.cost.seal')
    case 'SealMultiCost':
      return t('label.cost.sealMulti', { count: intAt(cost, 0) })
    case 'AddFrostTokenCost':
      return t('label.cost.addFrostTokens', { count: num(cost, 'contents') })
    case 'AddCurseTokenCost':
      return t('label.cost.addCurseTokens', { count: num(cost, 'contents') })
    case 'AddCurseTokensCost':
      return t('label.cost.addCurseTokens', { count: intAt(cost, 0) })
    case 'AddCurseTokensEqualToShroudCost':
      return t('label.cost.addCurseTokensEqualToShroud')
    case 'AddCurseTokensEqualToSkillTestDifficulty':
      return t('label.cost.addCurseTokensEqualToTestDifficulty')
    case 'ReleaseChaosTokenCost':
      return t('label.cost.releaseChaosToken')
    case 'ReleaseChaosTokensCost':
      return t('label.cost.releaseChaosTokens', { count: intAt(cost, 0) })
    case 'ReturnChaosTokensToPoolCost':
      return t('label.cost.returnChaosTokensToPool', { count: intAt(cost, 0) })
    case 'ReturnChaosTokenToPoolCost':
      return t('label.cost.returnChaosTokenToPool')
    case 'SupplyCost':
      return t('label.cost.supply')
    case 'ResolveEachHauntedAbility':
      return t('label.cost.resolveHaunted')
    case 'ShuffleBondedCost':
      return t('label.cost.shuffleBonded', { count: intAt(cost, 0) })
    case 'ShuffleIntoDeckCost':
      return t('label.cost.shuffleIntoDeck')
    case 'ShuffleAttachedCardIntoDeckCost':
      return t('label.cost.shuffleAttachedIntoDeck')
    case 'DrawEncounterCardsCost':
      return t('label.cost.drawEncounterCards', { count: num(cost, 'contents') })
    case 'GloriaCost':
    case 'ArchiveOfConduitsUnidentifiedCost':
      return t('label.cost.calculated')
    case 'FlipScarletKeyCost':
      return t('label.cost.flipScarletKey')
    default:
      // Unknown variant — show tag name as a last resort. Better than blank.
      return typeof tag === 'string' ? tag : ''
  }
}
