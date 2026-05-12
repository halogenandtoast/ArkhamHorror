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
    case 'XCost':
    case 'OneOfDistanceCost': {
      const inner = get<Cost>(cost, 'contents')
      return inner ? t('label.cost.x', { inner: formatCost(inner, t) }) : t('label.cost.x', { inner: '' })
    }
    case 'OptionalCost': {
      const inner = get<Cost>(cost, 'contents')
      return inner ? t('label.cost.optional', { inner: formatCost(inner, t) }) : ''
    }
    case 'NonBlankedCost':
    case 'CostWhenEnemy':
    case 'CostWhenTreachery':
    case 'CostOnlyWhen':
    case 'AsIfAtLocationCost': {
      const inner = get<Cost>(cost, 'contents')
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
      return t('label.cost.damage', { count: num(cost, 'contents') })
    case 'DirectDamageCost':
      return t('label.cost.directDamage', { count: num(cost, 'contents') })
    case 'DirectHorrorCost':
      return t('label.cost.directHorror', { count: num(cost, 'contents') })
    case 'HorrorCost':
      return t('label.cost.horror', { count: num(cost, 'contents') })
    case 'HorrorCostX':
      return t('label.cost.horrorX')
    case 'DoomCost':
      return t('label.cost.doom', { count: num(cost, 'contents') })
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
    default:
      // Unknown variant — show tag name as a last resort. Better than blank.
      return tag
  }
}
