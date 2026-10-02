// Campaign/scenario data contributed by homebrew campaigns, discovered from
// frontend/homebrew/<campaign>/ like every other homebrew asset:
//
// - `campaign.json` — the campaign entry (id, name, designer, difficulty
//   chaos bags) appended to the official campaigns list.
// - `scenarios.json` — the campaign's scenario list (with an `i18n` key per
//   scenario naming its locale scope within the campaign).
import type { Campaign, Scenario } from '@/arkham/data'

const campaignModules = import.meta.glob('@homebrew/*/campaign.json', { eager: true }) as Record<
  string,
  { default: Campaign }
>

const scenarioModules = import.meta.glob('@homebrew/*/scenarios.json', { eager: true }) as Record<
  string,
  { default: (Scenario & { i18n: string })[] }
>

/* `tokens.json` — custom chaos tokens a campaign wants surfaced in the scenario
totals bar (counted across the chaos bag and sealed tokens) and in the chaos bag
debug panel. Each entry names the token `face` (its slug) and an optional
`tooltip`.

`icon` names a CSS class the campaign's own style.css defines (the same masked
glyph classes icons.json hooks into card text); without one the debug panel
falls back to the token art. `background` and `iconColor` are the campaign's
control over how its token reads as a button — `iconColor` paints the glyph and
the +/- labels, so the pair has to contrast. */
export interface HomebrewTotalsToken {
  face: string
  tooltip?: string
  icon?: string
  background?: string
  iconColor?: string
}

// ":circus-ex-mortis:moon" -> ":circus-ex-mortis". A custom token's campaign is
// the namespace in its slug, which is how a game only ever offers its own.
export function homebrewTokenCampaign(face: string): string | null {
  const parts = face.split(':')
  return parts.length === 3 && parts[1] ? `:${parts[1]}` : null
}

const tokenModules = import.meta.glob('@homebrew/*/tokens.json', { eager: true }) as Record<
  string,
  { default: HomebrewTotalsToken[] }
>

export const homebrewTotalsTokens: HomebrewTotalsToken[] = Object.values(tokenModules).flatMap(
  (m) => m.default,
)

// `achievements.json` — the campaign's achievement list, in printed order.
// Each entry's `key` matches the backend's `AchievementDefs.hs` enum
// constructor; `items` makes it a cross-playthrough checklist. Names and
// descriptions live in the campaign's own locale scope, under
// `<scope>.achievements.<key>`.
export interface HomebrewAchievementList {
  campaign: string
  entries: { key: string; items?: string[] }[]
}

const achievementModules = import.meta.glob('@homebrew/*/achievements.json', { eager: true }) as Record<
  string,
  { default: HomebrewAchievementList }
>

export const homebrewAchievementLists: HomebrewAchievementList[] = Object.values(
  achievementModules,
).map((m) => m.default)

export const homebrewCampaigns: Campaign[] = Object.values(campaignModules).map((m) => m.default)

export const homebrewScenarios: (Scenario & { i18n: string })[] = Object.values(
  scenarioModules,
).flatMap((m) => m.default)

// ":circus-ex-mortis" -> "circusExMortis" (the campaign i18n scope; the
// homebrew directory is the kebab-case id without the leading colon)
export function homebrewCampaignScope(campaignId: string): string {
  const parts = campaignId.replace(/^:/, '').split('-')
  return (parts[0] ?? '') + parts.slice(1).map((p) => p.charAt(0).toUpperCase() + p.slice(1)).join('')
}

// "c:circus-ex-mortis:001" -> "circusExMortis.oneNightOnly"
export function homebrewScenarioI18n(scenarioId: string): string | null {
  const bare = scenarioId.replace(/^c/, '')
  const scenario = homebrewScenarios.find((s) => s.id === bare)
  if (!scenario) return null
  return `${homebrewCampaignScope(scenario.campaign ?? '')}.${scenario.i18n}`
}
