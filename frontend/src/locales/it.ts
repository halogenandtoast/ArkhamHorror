/* Italian is a partial translation; everything it leaves out falls back to
 * English through vue-i18n's `fallbackLocale`. It carries no campaign scopes --
 * it/nightOfTheZealot.json is an empty object -- so campaign prose comes from
 * the English scopes locales/campaigns.ts loads on demand. */
import base from '@/locales/it/base.json'

export default {...base}
