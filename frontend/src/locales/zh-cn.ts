import base from '@/locales/zh-cn/base.json'
import { homebrewTraits } from '@/locales/homebrew'
import log from '@/locales/zh-cn/log.json'
import event from '@/locales/zh-cn/event.json'
import cardOption from '@/locales/zh-cn/cardOption.json'
import cards from '@/locales/zh-cn/cards.json'
import label from '@/locales/zh-cn/label.json'
import investigators from '@/locales/zh-cn/investigators.json'
import campaignLog from '@/locales/zh-cn/campaignLog.json'
import gameBoard from '@/locales/zh-cn/gameBoard/gameBoard'
import xp from '@/locales/zh-cn/xp.json'
import theLabyrinthsOfLunacy from '@/locales/zh-cn/theLabyrinthsOfLunacy.json'
import scenarioNames from '@/locales/zh-cn/scenarioNames.json'
import campaignNames from '@/locales/zh-cn/campaignNames.json'

export default 
{ ...base,
  ...campaignLog, 
  ...gameBoard, 
  ...cardOption,
  ...event,
  cards,
  investigators,
  label: { ...label, cards: cards["label"] },
  log,
  xp,
  scenarioNames,
  campaignNames,
  ...homebrewTraits('zh-cn'),
  theLabyrinthsOfLunacy
}
