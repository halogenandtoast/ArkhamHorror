import base from '@/locales/en/base.json'
import event from '@/locales/en/event.json'
import log from '@/locales/en/log.json'
import cards from '@/locales/en/cards.json'
import label from '@/locales/en/label.json'
import investigators from '@/locales/en/investigators.json'
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
import theFeastOfHemlockVale from '@/locales/en/theFeastOfHemlockVale'
import brethrenOfAsh from '@/locales/en/brethrenOfAsh'
import theDrownedCity from '@/locales/en/theDrownedCity'
import standalone from '@/locales/en/standalone'
import theLabyrinthsOfLunacyLog from '@/locales/en/theLabyrinthsOfLunacy.json'
import gameBoard from '@/locales/en/gameBoard/gameBoard'
import xp from '@/locales/en/xp.json'

export default
  { ...base
  , ...event
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
  , theFeastOfHemlockVale
  , brethrenOfAsh
  , theDrownedCity
  , standalone
  , theLabyrinthsOfLunacy: theLabyrinthsOfLunacyLog
  }
