import { homebrewTokenMap } from '@/arkham/homebrewAssets'

/* ArkhamDB card text ([action], [[Trait]], <i>…) turned into the HTML the card overlay and the
 * location info card show. Only the db's own markup and icon tokens are rewritten. */
const TOKEN_MAP: Record<string, string> = {
  '[action]': '<span class="action-icon"></span>',
  '[fast]': '<span class="fast-icon"></span>',
  '[free]': '<span class="free-icon"></span>',
  '[reaction]': '<span class="reaction-icon"></span>',
  '[willpower]': '<span class="willpower-icon"></span>',
  '[intellect]': '<span class="intellect-icon"></span>',
  '[combat]': '<span class="combat-icon"></span>',
  '[agility]': '<span class="agility-icon"></span>',
  '[wild]': '<span class="wild-icon"></span>',
  '[guardian]': '<span class="guardian-icon"></span>',
  '[seeker]': '<span class="seeker-icon"></span>',
  '[rogue]': '<span class="rogue-icon"></span>',
  '[mystic]': '<span class="mystic-icon"></span>',
  '[survivor]': '<span class="survivor-icon"></span>',
  '[elder_sign]': '<span class="elder-sign"></span>',
  '[auto_fail]': '<span class="auto-fail"></span>',
  '[skull]': '<span class="skull-icon"></span>',
  '[cultist]': '<span class="cultist-icon"></span>',
  '[tablet]': '<span class="tablet-icon"></span>',
  '[elder_thing]': '<span class="elder-thing-icon"></span>',
  '[bless]': '<span class="bless-icon"></span>',
  '[curse]': '<span class="curse-icon"></span>',
  '[frost]': '<span class="frost-icon"></span>',
  '[blood]': '<span class="blood-icon"></span>',
  '[per_investigator]': '<span class="per-player"></span>',
  '[seal_a]': '<span class="seal-a-icon"></span>',
  '[seal_b]': '<span class="seal-b-icon"></span>',
  '[seal_c]': '<span class="seal-c-icon"></span>',
  '[seal_d]': '<span class="seal-d-icon"></span>',
  '[seal_e]': '<span class="seal-e-icon"></span>',
  '[day]': '<span class="day-icon"></span>',
  '[night]': '<span class="night-icon"></span>',
  '[codex]': '<span class="codex-icon"></span>',
  ...homebrewTokenMap,
}

const escapeRegExp = (s: string) => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
const tokenRE = new RegExp(Object.keys(TOKEN_MAP).map(escapeRegExp).join('|'), 'g')
export const replaceText = (text: string): string => !text ? '' :
  text
    .replaceAll('[[', '<span style="font-style: italic; font-weight: bold">')
    .replaceAll(']]', '</span>')
    .replaceAll('<i>', '<span style="font-style: italic;">')
    .replaceAll('</i>', '</span>')
    .replace(tokenRE, (m) => TOKEN_MAP[m] ?? m)
