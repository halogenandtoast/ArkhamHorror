import * as JsonDecoder from 'ts.data.json'

export interface CampaignOverlay {
  id: string
  name: string
  scenario: string
  available: boolean
  xpCost: number
}

export const campaignOverlayDecoder = JsonDecoder.object<CampaignOverlay>({
  id: JsonDecoder.string(),
  name: JsonDecoder.string(),
  scenario: JsonDecoder.string(),
  available: JsonDecoder.boolean(),
  xpCost: JsonDecoder.number(),
}, 'CampaignOverlay')
