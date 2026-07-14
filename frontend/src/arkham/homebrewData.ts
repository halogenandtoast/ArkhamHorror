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
