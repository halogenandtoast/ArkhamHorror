export const uiLocales = ['en', 'fr', 'it', 'ko', 'es', 'zh', 'zh-cn'] as const

export type UiLocale = (typeof uiLocales)[number]

export function uiLocaleFor(language: string): UiLocale {
  const normalized = language.toLowerCase()
  return uiLocales.includes(normalized as UiLocale) ? (normalized as UiLocale) : 'en'
}

export function preferredLanguage(browserLocale: string): string {
  const normalized = browserLocale.trim().toLowerCase()
  if (!normalized) return 'en'

  const [language, ...subtags] = normalized.split('-')
  if (language !== 'zh') return language

  if (subtags.includes('hant')) return 'zh'

  if (subtags.includes('hans') || subtags.includes('cn') || subtags.includes('sg')) {
    return 'zh-cn'
  }

  return 'zh'
}
