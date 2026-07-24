import base from '@/locales/zh-cn/base.json'
import { homebrewMessages } from '@/locales/homebrew'
import log from '@/locales/zh-cn/log.json'
import cards from '@/locales/zh-cn/cards.json'
import label from '@/locales/zh-cn/label.json'
import investigators from '@/locales/zh-cn/investigators.json'
import campaignLog from '@/locales/zh-cn/campaignLog.json'
import nightOfTheZealot from '@/locales/zh-cn/nightOfTheZealot'
import theDunwichLegacy from '@/locales/zh-cn/theDunwichLegacy'
import thePathToCarcosa from '@/locales/zh-cn/thePathToCarcosa'
import theForgottenAge from '@/locales/zh-cn/theForgottenAge'
import theCircleUndone from '@/locales/zh-cn/theCircleUndone'
import theDreamEaters from '@/locales/zh-cn/theDreamEaters'
import theInnsmouthConspiracy from '@/locales/zh-cn/theInnsmouthConspiracy'
import edgeOfTheEarth from '@/locales/zh-cn/edgeOfTheEarth'
import theScarletKeys from '@/locales/zh-cn/theScarletKeys'
import theFeastOfHemlockVale from '@/locales/zh-cn/theFeastOfHemlockVale'
import brethrenOfAsh from '@/locales/zh-cn/brethrenOfAsh'
import theDrownedCity from '@/locales/zh-cn/theDrownedCity'
import standalone from '@/locales/zh-cn/standalone'
import gameBoard from '@/locales/zh-cn/gameBoard/gameBoard'
import xp from '@/locales/zh-cn/xp.json'

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
  ...homebrewMessages('zh-cn'),
  theDrownedCity,
  standalone
}
