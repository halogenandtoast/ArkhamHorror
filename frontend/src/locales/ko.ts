import base from '@/locales/ko/base.json'
import cardOption from '@/locales/ko/cardOption.json'
import { homebrewTraits } from '@/locales/homebrew'
import event from '@/locales/ko/event.json'
import log from '@/locales/ko/log.json'
import cards from '@/locales/ko/cards.json'
import label from '@/locales/ko/label.json'
import investigators from '@/locales/ko/investigators.json'
import campaignLog from '@/locales/ko/campaignLog.json'
import theLabyrinthsOfLunacyLog from '@/locales/ko/theLabyrinthsOfLunacy.json'
import gameBoard from '@/locales/ko/gameBoard/gameBoard'
import xp from '@/locales/ko/xp.json'
// Display-only names, looked up with te() and falling back to the engine's
// English (see arkham/scenarioNames.ts and views/Cards.vue).
import scenarioNames from '@/locales/ko/scenarioNames.json'
import campaignNames from '@/locales/ko/campaignNames.json'
import cardSetNames from '@/locales/ko/cardSetNames.json'
// English the engine sends as plain text, not as a key (see arkham/engineText.ts).
export { default as engineText } from '@/locales/ko/engineText.json'

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
  , ...homebrewTraits('ko')
  , theLabyrinthsOfLunacy: theLabyrinthsOfLunacyLog
  }
