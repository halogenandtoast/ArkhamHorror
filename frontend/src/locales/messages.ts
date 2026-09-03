import { uiLocaleFor, type UiLocale } from '@/locales/language'
import { iconPlaceholderPattern } from '@/arkham/icons'

const localeLoaders = {
  en: () => import('@/locales/en'),
  fr: () => import('@/locales/fr'),
  it: () => import('@/locales/it'),
  ko: () => import('@/locales/ko'),
  es: () => import('@/locales/es'),
  zh: () => import('@/locales/zh'),
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
  return { locale: normalizedLocale, messages: normalizeIconPlaceholders(messages.default) }
}
