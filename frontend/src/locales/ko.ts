import base from '@/locales/ko/base.json'
import cardOption from '@/locales/ko/cardOption.json'
import { homebrewMessages } from '@/locales/homebrew'
import event from '@/locales/ko/event.json'
import log from '@/locales/ko/log.json'
import cards from '@/locales/ko/cards.json'
import label from '@/locales/ko/label.json'
import investigators from '@/locales/ko/investigators.json'
import campaignLog from '@/locales/ko/campaignLog.json'
import nightOfTheZealot from '@/locales/ko/nightOfTheZealot'
import theDunwichLegacy from '@/locales/ko/theDunwichLegacy'
import thePathToCarcosa from '@/locales/ko/thePathToCarcosa'
import theForgottenAge from '@/locales/ko/theForgottenAge'
import theCircleUndone from '@/locales/ko/theCircleUndone'
import theDreamEaters from '@/locales/ko/theDreamEaters'
import theInnsmouthConspiracy from '@/locales/ko/theInnsmouthConspiracy'
import edgeOfTheEarth from '@/locales/ko/edgeOfTheEarth'
import theScarletKeys from '@/locales/ko/theScarletKeys'
import theFeastOfHemlockVale from '@/locales/ko/theFeastOfHemlockVale'
import theDrownedCity from '@/locales/ko/theDrownedCity'
import brethrenOfAsh from '@/locales/ko/brethrenOfAsh'
import childrenOfBlood from '@/locales/ko/childrenOfBlood'
import standalone from '@/locales/ko/standalone'
import theLabyrinthsOfLunacyLog from '@/locales/ko/theLabyrinthsOfLunacy.json'
import gameBoard from '@/locales/ko/gameBoard/gameBoard'
import xp from '@/locales/ko/xp.json'
// Display-only names, looked up with te() and falling back to the engine's
// English (see arkham/scenarioNames.ts and views/Cards.vue).
import scenarioNames from '@/locales/ko/scenarioNames.json'
import campaignNames from '@/locales/ko/campaignNames.json'
import cardSetNames from '@/locales/ko/cardSetNames.json'

export default
  { ...base
  , ...cardOption
  , ...event
  , ...campaignLog
  , ...gameBoard
  , cards
  , investigators
  , label: { ...label, cards: cards["label"] }
  , log
  , xp
  , scenarioNames
  , campaignNames
  , cardSetNames
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
  , ...homebrewMessages('ko')
  , standalone
  , theLabyrinthsOfLunacy: theLabyrinthsOfLunacyLog
  }
