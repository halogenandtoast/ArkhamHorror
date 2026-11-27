import base from '@/locales/fr/base.json'
import log from '@/locales/fr/log.json'
import cards from '@/locales/fr/cards.json'
import label from '@/locales/fr/label.json'
import investigators from '@/locales/fr/investigators.json'
import campaignLog from '@/locales/fr/campaignLog.json'
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
  }
