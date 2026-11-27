/* Vietnamese is a partial translation. vue-i18n's `fallbackLocale: 'en'` (set in
 * main.ts) resolves any key this tree leaves out against English, so a scope
 * only appears here once something under it is actually translated -- and a
 * file only carries the keys that are. `cards.json` is deliberately absent:
 * card names and their choice labels fall back to English wholesale rather than
 * half-translated. */
import base from '@/locales/vi/base.json'
import { homebrewTraits } from '@/locales/homebrew'
import event from '@/locales/vi/event.json'
import log from '@/locales/vi/log.json'
import label from '@/locales/vi/label.json'
import campaignLog from '@/locales/vi/campaignLog.json'
import theLabyrinthsOfLunacyLog from '@/locales/vi/theLabyrinthsOfLunacy.json'
import gameBoard from '@/locales/vi/gameBoard/gameBoard'

export default
  { ...base
  , ...event
  , ...campaignLog
  , ...gameBoard
  , label
  , log
  , ...homebrewTraits('vi')
  , theLabyrinthsOfLunacy: theLabyrinthsOfLunacyLog
  }
