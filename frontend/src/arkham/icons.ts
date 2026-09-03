// The icon placeholder vocabulary, in one place.
//
// `{skull}` and friends are written straight into locale JSON and card text and
// swapped for an icon span by `formatContent`. vue-i18n sees the same braces as
// a named interpolation, though, so a message has to escape them —
// `{'{'}skull{'}'}` — or the placeholder resolves to nothing and vanishes before
// `formatContent` runs. `loadLocaleMessages` escapes the raw form on the way in
// so both spellings render the same; it needs exactly the names `replaceIcons`
// knows about, hence this table.
import { homebrewIcons } from '@/arkham/homebrewAssets'

export const iconClasses: Record<string, string> = {
  action: 'action-icon',
  fast: 'fast-icon',
  reaction: 'reaction-icon',
  willpower: 'willpower-icon',
  intellect: 'intellect-icon',
  combat: 'combat-icon',
  agility: 'agility-icon',
  wild: 'wild-icon',
  guardian: 'guardian-icon',
  seeker: 'seeker-icon',
  rogue: 'rogue-icon',
  mystic: 'mystic-icon',
  survivor: 'survivor-icon',
  elderSign: 'elder-sign',
  autoFail: 'auto-fail',
  skull: 'skull-icon',
  cultist: 'cultist-icon',
  tablet: 'tablet-icon',
  elderThing: 'elder-thing-icon',
  bless: 'bless-icon',
  curse: 'curse-icon',
  frost: 'frost-icon',
  blood: 'blood-icon',
  sealA: 'seal-a-icon',
  sealB: 'seal-b-icon',
  sealC: 'seal-c-icon',
  sealD: 'seal-d-icon',
  sealE: 'seal-e-icon',
  codex: 'codex-icon',
  day: 'day-icon',
  night: 'night-icon',
  perPlayer: 'per-player',
}

// Not icons, but `formatContent` owns these braces too, so i18n must not eat them.
export const literalPlaceholders = ['asterisk', 'underscore']

export const runePlaceholder = /{rune([A-Z])}/g

export function iconPlaceholderNames(): string[] {
  return [...Object.keys(iconClasses), ...literalPlaceholders, ...Object.keys(homebrewIcons)]
}

// Matches every placeholder `formatContent` resolves, so i18n can be told to
// leave them alone.
export function iconPlaceholderPattern(): RegExp {
  return new RegExp(`{(${iconPlaceholderNames().join('|')}|rune[A-Z])}`, 'g')
}
