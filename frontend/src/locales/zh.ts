import base from '@/locales/zh/base.json'
import log from '@/locales/zh/log.json'
import cards from '@/locales/zh/cards.json'
import label from '@/locales/zh/label.json'
import investigators from '@/locales/zh/investigators.json'
import campaignLog from '@/locales/zh/campaignLog.json'
import nightOfTheZealot from '@/locales/zh/nightOfTheZealot'
import theDunwichLegacy from '@/locales/zh/theDunwichLegacy'
import thePathToCarcosa from '@/locales/zh/thePathToCarcosa'
import theForgottenAge from '@/locales/zh/theForgottenAge'
import theCircleUndone from '@/locales/zh/theCircleUndone'
import theDreamEaters from '@/locales/zh/theDreamEaters'
import theInnsmouthConspiracy from '@/locales/zh/theInnsmouthConspiracy'
import edgeOfTheEarth from '@/locales/zh/edgeOfTheEarth'
import theScarletKeys from '@/locales/zh/theScarletKeys'
import theFeastOfHemlockVale from '@/locales/en/theFeastOfHemlockVale'
import brethrenOfAsh from '@/locales/en/brethrenOfAsh'
import standalone from '@/locales/zh/standalone'
import gameBoard from '@/locales/zh/gameBoard/gameBoard'
import xp from '@/locales/zh/xp.json'

export default 
{ ...base,
  ...campaignLog, 
  ...gameBoard, 
  cards,
  investigators,
  label: { ...label, cards: cards["label"] },
  log,
  xp,
  nightOfTheZealot, 
  theDunwichLegacy, 
  thePathToCarcosa, 
  theForgottenAge, 
  returnToTheForgottenAge: theForgottenAge,
  theCircleUndone, 
  theDreamEaters, 
  theInnsmouthConspiracy, 
  edgeOfTheEarth, 
  theScarletKeys, 
  theFeastOfHemlockVale,
  brethrenOfAsh, 
  standalone
}
