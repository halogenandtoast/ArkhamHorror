// Renders a backend `Cost` value into a localized human-readable string.
// Mirrors what the (now-removed) Haskell `displayCostType` produced. Each
// branch maps onto an i18n key in `cost.*` (see locale files); unknown tags
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
      return t('cost.free')
    case 'ActionCost': {
      const n = num(cost, 'contents')
      return t('cost.action', { count: n })
    }
    case 'AdditionalActionCost':
    case 'AdditionalActionsCost':
    case 'AdditionalActionsCostThatReducesResourceCostBy':
      return t('cost.additionalAction')
    case 'ResourceCost':
      return t('cost.resource', { count: num(cost, 'contents') })
    case 'ScenarioResourceCost':
      return t('cost.scenarioResource', { count: num(cost, 'contents') })
    case 'ClueCostX':
      return t('cost.clueX')
    case 'ConcealedXCost':
      return t('cost.concealedX')
    case 'UnpayableCost':
      return t('cost.unpayable')
    case 'ExhaustCost':
      return t('cost.exhaust')
    case 'ExhaustAssetCost':
      return t('cost.exhaustAsset')
    case 'RemoveCost':
      return t('cost.remove')
    case 'RevealCost':
      return t('cost.reveal')
    case 'DiscardCardCost':
      return t('cost.discardCard')
    case 'DiscardRandomCardCost':
      return t('cost.discardRandomCard')
    case 'DiscardDrawnCardCost':
      return t('cost.discardDrawnCard')
    case 'DiscardHandCost':
      return t('cost.discardHand')
    case 'DiscardAssetCost':
      return t('cost.discardAsset')
    case 'ReturnAssetToHandCost':
    case 'ReturnMatchingAssetToHandCost':
      return t('cost.returnAssetToHand')
    case 'ReturnEventToHandCost':
      return t('cost.returnEventToHand')
    case 'ExileCost':
      return t('cost.exile')
    case 'SpendTokenCost':
      return t('cost.spendToken')
    case 'SealChaosTokenCost':
      return t('cost.sealToken')
    case 'XCost':
    case 'OneOfDistanceCost': {
      const inner = get<Cost>(cost, 'contents')
      return inner ? t('cost.x', { inner: formatCost(inner, t) }) : t('cost.x', { inner: '' })
    }
    case 'OptionalCost': {
      const inner = get<Cost>(cost, 'contents')
      return inner ? t('cost.optional', { inner: formatCost(inner, t) }) : ''
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
      return join(inner.map((c) => formatCost(c, t)), t('cost.separator'))
    }
    case 'OrCost': {
      const inner = get<Cost[]>(cost, 'contents') ?? []
      return join(inner.map((c) => formatCost(c, t)), t('cost.or'))
    }
    case 'DamageCost':
      return t('cost.damage', { count: num(cost, 'contents') })
    case 'DirectDamageCost':
      return t('cost.directDamage', { count: num(cost, 'contents') })
    case 'DirectHorrorCost':
      return t('cost.directHorror', { count: num(cost, 'contents') })
    case 'HorrorCost':
      return t('cost.horror', { count: num(cost, 'contents') })
    case 'HorrorCostX':
      return t('cost.horrorX')
    case 'DoomCost':
      return t('cost.doom', { count: num(cost, 'contents') })
    case 'LabeledCost': {
      const contents = get<unknown[]>(cost, 'contents')
      return typeof contents?.[0] === 'string' ? (contents[0] as string) : ''
    }
    default:
      // Unknown variant — show tag name as a last resort. Better than blank.
      return tag
  }
}
