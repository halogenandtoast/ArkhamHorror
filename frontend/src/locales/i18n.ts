/* The running vue-i18n instance, for the few places that need UI messages
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

/** UI text needed by renderers outside a component, including SVG placeholders. */
export function translateUiMessage(key: string, fallback: string): string {
  const composer = instance?.global as { te: (key: string) => boolean; t: (key: string) => unknown } | undefined
  if (!composer?.te(key)) return fallback
  const value = composer.t(key)
  return typeof value === 'string' ? value : fallback
}

/** The messages a locale holds, for the few places that have to read them. */
export function messagesFor(locale: string): Record<string, unknown> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const global = instance?.global as any
  const messages = global?.getLocaleMessage?.(locale)
  return messages && typeof messages === 'object' ? messages : {}
}

/** Messages for one locale, merged over whatever is already there. */
export function mergeMessages(locale: string, messages: Record<string, unknown>) {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const global = instance?.global as any
  if (!global?.mergeLocaleMessage) return
  global.mergeLocaleMessage(locale, messages)
}
