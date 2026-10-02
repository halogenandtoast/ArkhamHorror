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

/* An extra action the campaign offers on the continuation screen, beside
 * Continue and Upgrade Decks. `label` is a full i18n key: a homebrew campaign
 * owns its own locale namespace. */
export interface ContinueOption {
  key: string
  label: string
  available: boolean
}

export const continueOptionDecoder = JsonDecoder.object<ContinueOption>({
  key: JsonDecoder.string(),
  label: JsonDecoder.string(),
  available: JsonDecoder.boolean(),
}, 'ContinueOption')
