import base from '@/locales/zh/base.json'
import label from '@/locales/zh/label.json'
import campaignLog from '@/locales/zh/campaignLog.json'
import investigators from '@/locales/zh/investigators.json'
import nightOfTheZealot from '@/locales/zh/nightOfTheZealot'
import theDunwichLegacy from '@/locales/zh/theDunwichLegacy'
import thePathToCarcosa from '@/locales/zh/thePathToCarcosa'
import theForgottenAge from '@/locales/zh/theForgottenAge'
import theCircleUndone from '@/locales/zh/theCircleUndone'
import theDreamEaters from '@/locales/zh/theDreamEaters'
import theInnsmouthConspiracy from '@/locales/zh/theInnsmouthConspiracy'
import edgeOfTheEarth from '@/locales/zh/edgeOfTheEarth'
import standalone from '@/locales/zh/standalone'
import gameBoard from '@/locales/zh/gameBoard/gameBoard'
import cardsZh from '../../public/cards_zh.json'

const autoCardsDict: Record<string, string> = {};
Object.values(cardsZh).forEach((card: any) => {
  if (card.real_name && card.name) {
    autoCardsDict[card.real_name] = card.name;
  }
});

export default {...base, label, ...campaignLog, ...investigators, ...gameBoard, ...autoCardsDict, nightOfTheZealot, theDunwichLegacy, thePathToCarcosa, theForgottenAge, theCircleUndone, theDreamEaters, theInnsmouthConspiracy, edgeOfTheEarth, standalone}
