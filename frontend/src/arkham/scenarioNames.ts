// Display names belong to the viewer's locale; scenario metadata and saved
// games keep the engine's English names and stable ids.
export function localizedScenarioName(
  scenarioId: string,
  fallback: string,
  t: (key: string) => string,
  te: (key: string) => boolean,
): string {
  const id = scenarioId.replace(/^c(?=[0-9:])/, '')
  const key = `scenarioNames.${id}`
  return te(key) ? t(key) : fallback
}

type CampaignNameEntry = {
  id: string
  name: string
  returnTo?: { id: string; name?: string }
}

type NamedGame = {
  name: string
  campaign: { id: string } | null
  scenario: { id: string; name: { title: string } } | null
}

// Old saves retain the English default game name. Translate an exact match to
// that game's campaign/scenario, while leaving player-authored names intact.
export function localizedGameName(
  game: NamedGame,
  campaigns: readonly CampaignNameEntry[],
  t: (key: string) => string,
  te: (key: string) => boolean,
): string {
  const campaignId = game.campaign?.id.replace(/^0+/, '')
  if (campaignId) {
    for (const campaign of campaigns) {
      let id: string | undefined
      if (campaign.id.replace(/^0+/, '') === campaignId && game.name === campaign.name) {
        id = campaign.id
      } else if (campaign.returnTo?.id.replace(/^0+/, '') === campaignId
        && (game.name === `Return to ${campaign.name}` || game.name === campaign.returnTo.name)) {
        id = campaign.returnTo.id
      }
      if (id && te(`campaignNames.${id}`)) return t(`campaignNames.${id}`)
    }
  }
  if (game.scenario && game.name === game.scenario.name.title) {
    return localizedScenarioName(game.scenario.id, game.name, t, te)
  }
  return game.name
}
