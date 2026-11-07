/* Vietnamese is a partial translation. vue-i18n's `fallbackLocale: 'en'` (set in
 * main.ts) resolves any key this tree leaves out against English, so a scope
 * only appears here once something under it is actually translated -- and a
 * file only carries the keys that are. `cards.json` is deliberately absent:
 * card names and their choice labels fall back to English wholesale rather than
 * half-translated. */
import base from '@/locales/vi/base.json'
import { homebrewMessages } from '@/locales/homebrew'
import event from '@/locales/vi/event.json'
import log from '@/locales/vi/log.json'
import label from '@/locales/vi/label.json'
import campaignLog from '@/locales/vi/campaignLog.json'
import nightOfTheZealot from '@/locales/vi/nightOfTheZealot'
import theDunwichLegacy from '@/locales/vi/theDunwichLegacy'
import thePathToCarcosa from '@/locales/vi/thePathToCarcosa'
import theForgottenAge from '@/locales/vi/theForgottenAge'
import theCircleUndone from '@/locales/vi/theCircleUndone'
import theDreamEaters from '@/locales/vi/theDreamEaters'
import theInnsmouthConspiracy from '@/locales/vi/theInnsmouthConspiracy'
import edgeOfTheEarth from '@/locales/vi/edgeOfTheEarth'
import theScarletKeys from '@/locales/vi/theScarletKeys'
import theFeastOfHemlockVale from '@/locales/vi/theFeastOfHemlockVale'
import theDrownedCity from '@/locales/vi/theDrownedCity'
import brethrenOfAsh from '@/locales/vi/brethrenOfAsh'
import childrenOfBlood from '@/locales/vi/childrenOfBlood'
import standalone from '@/locales/vi/standalone'
import theLabyrinthsOfLunacyLog from '@/locales/vi/theLabyrinthsOfLunacy.json'
import gameBoard from '@/locales/vi/gameBoard/gameBoard'

export default
  { ...base
  , ...event
  , ...campaignLog
  , ...gameBoard
  , label
  , log
  , nightOfTheZealot
  , theDunwichLegacy
  , thePathToCarcosa
  , theForgottenAge
  , returnToTheForgottenAge: theForgottenAge
  , theCircleUndone
  , theDreamEaters
  , theInnsmouthConspiracy
  , edgeOfTheEarth
  , theScarletKeys
  , theFeastOfHemlockVale
  , theDrownedCity
  , brethrenOfAsh
  , childrenOfBlood
  , ...homebrewMessages('vi')
  , standalone
  , theLabyrinthsOfLunacy: theLabyrinthsOfLunacyLog
  }
