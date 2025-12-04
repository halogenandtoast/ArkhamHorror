import log from '@/locales/fr/log.json'
import cards from '@/locales/fr/cards.json'
import label from '@/locales/fr/label.json'
import investigators from '@/locales/fr/investigators.json'
import campaignLog from '@/locales/fr/campaignLog.json'
import nightOfTheZealot from '@/locales/fr/nightOfTheZealot'
import theDunwichLegacy from '@/locales/fr/theDunwichLegacy'
import thePathToCarcosa from '@/locales/fr/thePathToCarcosa'
import theForgottenAge from '@/locales/fr/theForgottenAge'
import theCircleUndone from '@/locales/fr/theCircleUndone'
import theDreamEaters from '@/locales/fr/theDreamEaters'
import theInnsmouthConspiracy from '@/locales/fr/theInnsmouthConspiracy'
import edgeOfTheEarth from '@/locales/fr/edgeOfTheEarth'
import theScarletKeys from '@/locales/fr/theScarletKeys'
import standalone from '@/locales/fr/standalone'
import gameBoard from '@/locales/fr/gameBoard/gameBoard'
import xp from '@/locales/fr/xp.json'

export default
  { ...base
  , ...campaignLog
  , ...gameBoard
  , cards
  , investigators
  , label: { ...label, cards: cards["label"] }
  , log
  , xp
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
  , standalone
  }
