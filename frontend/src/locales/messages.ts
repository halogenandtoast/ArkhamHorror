const localeLoaders = {
  en: () => import('@/locales/en'),
  fr: () => import('@/locales/fr'),
  it: () => import('@/locales/it'),
  ko: () => import('@/locales/ko'),
  es: () => import('@/locales/es'),
  zh: () => import('@/locales/zh'),
}

export type SupportedLocale = keyof typeof localeLoaders

export const supportedLocales = Object.keys(localeLoaders) as SupportedLocale[]

export function normalizeLocale(locale: string): SupportedLocale {
  return supportedLocales.includes(locale as SupportedLocale) ? locale as SupportedLocale : 'en'
}

export async function loadLocaleMessages(locale: string) {
  const normalizedLocale = normalizeLocale(locale)
  const messages = await localeLoaders[normalizedLocale]()
  return { locale: normalizedLocale, messages: messages.default }
}
