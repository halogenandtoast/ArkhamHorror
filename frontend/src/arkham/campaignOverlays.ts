import { shallowRef } from 'vue'
import * as JsonDecoder from 'ts.data.json'

export interface CampaignOverlay {
  id: string
  name: string
  scenario: string
  available: boolean
  active: boolean
  xpCost: number
  cardReplacements: Record<string, string>
}

export const campaignOverlayDecoder = JsonDecoder.object<CampaignOverlay>({
  id: JsonDecoder.string(),
  name: JsonDecoder.string(),
  scenario: JsonDecoder.string(),
  available: JsonDecoder.boolean(),
  active: JsonDecoder.boolean(),
  xpCost: JsonDecoder.number(),
  cardReplacements: JsonDecoder.record(JsonDecoder.string(), 'Card replacements'),
}, 'CampaignOverlay')

// Scoped by the mounted game view; cleared on leaving it. The client displays
// replacement cards; their rule changes are supplied by the campaign backend.
const activeCardReplacements = shallowRef<Record<string, string>>({})

export function setCampaignOverlays(overlays: CampaignOverlay[]) {
  activeCardReplacements.value = Object.fromEntries(overlays.filter(o => o.active).flatMap(o =>
    Object.entries(o.cardReplacements).map(([code, replacement]) => [code.replace(/^c/, ''), replacement.replace(/^c/, '')])
  ))
}

export function campaignCardReplacement(cardCode: string): string | undefined {
  return activeCardReplacements.value[cardCode]
}
