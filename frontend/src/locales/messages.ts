import { uiLocaleFor, type UiLocale } from '@/locales/language'
import { iconPlaceholderPattern } from '@/arkham/icons'
import { registerEngineText, type EngineTextTable } from '@/arkham/engineText'

/* English is imported statically, every other locale on demand.
 *
 * `main.ts` loads English unconditionally -- it is the `fallbackLocale`, so it
 * is needed whoever is playing -- and as a dynamic import it could not even
 * begin downloading until the entry chunk had arrived and run. That cost a
 * whole round trip of nothing happening: measured against production, the
 * locale chunk did not start until 1.55s into a load whose document had
 * finished at 0.26s. Static means it is part of the entry chunk and arrives
 * with it.
 *
 * This is only affordable because the campaign prose moved out of `en.ts`
 * (locales/campaigns.ts). Do not put a campaign back into it. */
import * as en from '@/locales/en'

const localeLoaders = {
  en: async () => en,
  fr: () => import('@/locales/fr'),
  it: () => import('@/locales/it'),
  ko: () => import('@/locales/ko'),
  vi: () => import('@/locales/vi'),
  es: () => import('@/locales/es'),
  zh: () => import('@/locales/zh'),
  'zh-cn': () => import('@/locales/zh-cn'),
  de: () => import('@/locales/de'),
} satisfies Record<UiLocale, () => Promise<unknown>>

export type SupportedLocale = keyof typeof localeLoaders

export const supportedLocales = Object.keys(localeLoaders) as SupportedLocale[]

export function normalizeLocale(locale: string): SupportedLocale {
  return uiLocaleFor(locale)
}

const iconPlaceholder = iconPlaceholderPattern()

// A brace group vue-i18n's compiler would reject: anything that is not a named
// or indexed placeholder, or its literal form.
const uncompilableBrace = /\{(?!\s*(?:[A-Za-z_$][\w$]*|\d+|'(?:[^'\\]|\\.)*')\s*\})/

// vue-i18n reads `{skull}` as a named interpolation and, finding no such
// parameter, renders it as nothing — so an icon written the plain way disappears
// instead of reaching `formatContent`. Rewriting it to vue-i18n's literal form
// on the way in means messages can spell icons either way and get the same
// output. Already-escaped text can't match: it reads `}skull{`, not `{skull}`.
export function escapeIconPlaceholders(text: string): string {
  if (!text.includes('{')) return text
  // Not every message compiles: a `<style>` block's CSS braces are not valid
  // placeholders, and vue-i18n gives up and hands back the raw source — where a
  // plain `{skull}` already reaches `formatContent` intact. Escaping those would
  // only put `{'{'}skull{'}'}` on screen, so leave them exactly as they are.
  if (uncompilableBrace.test(text)) return text
  return text.replace(iconPlaceholder, "{'{'}$1{'}'}")
}

function normalizeIconPlaceholders<T>(value: T): T {
  if (typeof value === 'string') return escapeIconPlaceholders(value) as T
  if (Array.isArray(value)) return value.map(normalizeIconPlaceholders) as T
  if (value && typeof value === 'object') {
    const normalized: Record<string, unknown> = {}
    for (const [key, entry] of Object.entries(value)) normalized[key] = normalizeIconPlaceholders(entry)
    return normalized as T
  }
  return value
}

export async function loadLocaleMessages(locale: string) {
  const normalizedLocale = normalizeLocale(locale)
  const messages = await localeLoaders[normalizedLocale]()
  // Literal engine text (see arkham/engineText.ts) lives beside the messages, not in them.
  registerEngineText(normalizedLocale, (messages as { engineText?: EngineTextTable }).engineText)
  return { locale: normalizedLocale, messages: normalizeIconPlaceholders(messages.default) }
}
