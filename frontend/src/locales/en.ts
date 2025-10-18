import base from '@/locales/en/base.json'
import cards from '@/locales/en/cards.json'
import label from '@/locales/en/label.json'
import campaignLog from '@/locales/en/campaignLog.json'
import nightOfTheZealot from '@/locales/en/nightOfTheZealot'
import theDunwichLegacy from '@/locales/en/theDunwichLegacy'
import thePathToCarcosa from '@/locales/en/thePathToCarcosa'
import theForgottenAge from '@/locales/en/theForgottenAge'
import theCircleUndone from '@/locales/en/theCircleUndone'
import theDreamEaters from '@/locales/en/theDreamEaters'
import theInnsmouthConspiracy from '@/locales/en/theInnsmouthConspiracy'
import edgeOfTheEarth from '@/locales/en/edgeOfTheEarth'
import theScarletKeys from '@/locales/en/theScarletKeys'
import standalone from '@/locales/en/standalone'
import gameBoard from '@/locales/en/gameBoard/gameBoard'
import xp from '@/locales/en/xp.json'

export default
  { ...base
  , ...campaignLog
  , ...gameBoard
  , cards
  , label: { ...label, cards: cards["label"] }
  , xp
  , nightOfTheZealot
  , theDunwichLegacy
  , thePathToCarcosa
  , theForgottenAge
  , theCircleUndone
  , theDreamEaters
  , theInnsmouthConspiracy
  , edgeOfTheEarth
  , theScarletKeys
  , standalone
  }
