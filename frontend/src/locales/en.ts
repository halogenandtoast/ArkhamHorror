/* The core English messages: everything the app needs before it knows what the
 * player is going to do.
 *
 * The per-campaign narrative scopes that used to be listed here --
 * nightOfTheZealot through theDrownedCity, plus standalone and the homebrew
 * campaigns -- are loaded by locales/campaigns.ts when a game of that campaign
 * is opened. They were ~3.2 MB of this file's ~3.9 MB and `main.ts` blocks
 * `app.mount()` on it, so they were holding the whole app behind prose for
 * fifteen campaigns nobody had asked for yet.
 *
 * Anything added here is paid for by every cold load. If it belongs to one
 * campaign, it belongs in that campaign's scope instead.
 */
import base from '@/locales/en/base.json'
import cardOption from '@/locales/en/cardOption.json'
import modifiers from '@/locales/en/modifiers.json'
import traits from '@/locales/en/traits.json'
import { homebrewTraits } from '@/locales/homebrew'
import event from '@/locales/en/event.json'
import log from '@/locales/en/log.json'
import cards from '@/locales/en/cards.json'
import label from '@/locales/en/label.json'
import investigators from '@/locales/en/investigators.json'
import campaignLog from '@/locales/en/campaignLog.json'
import gameBoard from '@/locales/en/gameBoard/gameBoard'
import xp from '@/locales/en/xp.json'
/* The campaign-log scope, not the standalone scenario of the same name --
 * that one lives in `standalone` and loads with it. A few hundred bytes. */
import theLabyrinthsOfLunacyLog from '@/locales/en/theLabyrinthsOfLunacy.json'

export default
  { ...base
  , ...cardOption
  , ...modifiers
  , ...traits
  , ...event
  , ...campaignLog
  , ...gameBoard
  , cards
  , investigators
  , label: { ...label, cards: cards["label"] }
  , log
  , xp
  , ...homebrewTraits('en', traits.trait)
  , theLabyrinthsOfLunacy: theLabyrinthsOfLunacyLog
  }
