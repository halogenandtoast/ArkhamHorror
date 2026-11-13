/* The running vue-i18n instance, for the few places that have to add messages
 * from outside a component.
 *
 * A custom card carries its own text, so the messages a game needs are not
 * known when the app starts and do not live in `@/locales` at all. They arrive
 * with the cards, which is nowhere near a component with `useI18n` in it.
 */
import type { I18n } from 'vue-i18n'

let instance: I18n | null = null

export const setI18n = (i18n: I18n) => {
  instance = i18n
}

/** The messages a locale holds, for the few places that have to read them. */
export function messagesFor(locale: string): Record<string, unknown> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const global = instance?.global as any
  const messages = global?.getLocaleMessage?.(locale)
  return messages && typeof messages === 'object' ? messages : {}
}

/* Route titles are written in English in the route tables ("Arkham Horror: New
 * Game"); the page part is looked up under `pageTitles` by its camel-cased name
 * ("newGame"), falling back to the English text for a page with no entry. */
export function localizeTitle(title: string): string {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const global = instance?.global as any
  if (!global?.te) return title
  const m = title.match(/^Arkham ?Horror(?::\s*(.+))?$/)
  if (!m) return title
  const base = global.te('pageTitles.base') ? global.t('pageTitles.base') : 'Arkham Horror'
  if (!m[1]) return base
  const key = `pageTitles.${m[1].replace(/\s+(\w)/g, (_: string, c: string) => c.toUpperCase()).replace(/^\w/, (c) => c.toLowerCase())}`
  return global.te(key) ? `${base}: ${global.t(key)}` : `${base}: ${m[1]}`
}

/** Messages for one locale, merged over whatever is already there. */
export function mergeMessages(locale: string, messages: Record<string, unknown>) {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const global = instance?.global as any
  if (!global?.mergeLocaleMessage) return
  global.mergeLocaleMessage(locale, messages)
}
