import base from '@/locales/zh/base.json'
import log from '@/locales/zh/log.json'
import cards from '@/locales/zh/cards.json'
import label from '@/locales/zh/label.json'
import investigators from '@/locales/zh/investigators.json'
import campaignLog from '@/locales/zh/campaignLog.json'
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
}
