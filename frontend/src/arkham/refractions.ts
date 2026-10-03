/* Refractions: the Ultimatums and Boons written for one campaign or scenario
rather than for the game at large. The FAQ lists them apart from the general
two, and they are only offered while the campaign they belong to is the one
being played — picking one anywhere else could never do anything.

Official entries are listed here as their behavior lands (an entry with no
behavior would be selectable and inert, which is worse than absent). A homebrew
campaign's ultimatums are Refractions by construction: their wire name carries
the campaign id, so they need no entry here. */
import { homebrewUltimatumTags } from '@/arkham/homebrewData'

// tag -> the campaign ids it is scoped to (a campaign and its Return to count
// as the same campaign).
export const REFRACTION_CAMPAIGNS: Record<string, string[]> = {
  UltimatumOfVenom: ['04', '53'],
  UltimatumOfAmbuscade: ['04', '53'],
  UltimatumOfAnnoyance: ['08'],
  UltimatumOfTheUnspeakableName: ['03', '52'],
  UltimatumOfTheBrassCrown: ['03', '52'],
  UltimatumOfTheFaultyCarburetor: ['07'],
  UltimatumOfTheDrowned: ['07'],
  UltimatumOfTheSleeper: ['11'],
  UltimatumOfInvisibility: ['02', '51'],
  UltimatumOfMultiplication: ['02', '51'],
  UltimatumOfTheMan: ['03', '52'],
  UltimatumOfSpoilage: ['11'],
  UltimatumOfDeath: ['03', '52'],
  BoonOfTheDreamer: ['06'],
  BoonOfAtonement: ['09'],
  BoonOfBliss: ['10'],
  BoonOfTheMiners: ['10'],
  BoonOfTheDance: ['10'],
}

export function isRefraction(tag: string): boolean {
  return tag in REFRACTION_CAMPAIGNS || tag.startsWith(':')
}

// Every Refraction a given campaign can use, official and homebrew.
export function refractionTagsFor(campaignId: string | null): string[] {
  if (!campaignId) return []
  const official = Object.entries(REFRACTION_CAMPAIGNS)
    .filter(([, campaigns]) => campaigns.includes(campaignId))
    .map(([tag]) => tag)
  return [...official, ...homebrewUltimatumTags(campaignId)]
}
