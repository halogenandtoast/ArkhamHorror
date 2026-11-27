/* Spanish is a partial translation. vue-i18n's `fallbackLocale: 'en'` resolves
 * anything missing against English; the campaign scopes it does translate
 * (es/nightOfTheZealot, theDunwichLegacy, thePathToCarcosa) are discovered and
 * loaded on demand by locales/campaigns.ts. */
import base from '@/locales/es/base.json'
import label from '@/locales/es/label.json'
import gameBoard from '@/locales/es/gameBoard/gameBoard'

export default {...base, ...label, ...gameBoard}
