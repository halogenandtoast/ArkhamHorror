// Homebrew campaign locales are discovered from frontend/homebrew/<campaign>/locales/<locale>/,
// mirroring the backend's instance discovery: dropping a campaign directory in
// requires no i18n registration here. The kebab-case directory name (matching
// the campaign id, e.g. circus-ex-mortis for :circus-ex-mortis) camelCases to
// the message scope (circusExMortis, matching the backend campaignI18n scope);
// every JSON file in its locale folder is shallow-merged into that scope.
const modules = import.meta.glob('@homebrew/*/locales/*/*.json', { eager: true }) as Record<
  string,
  { default: Record<string, unknown> }
>

export function homebrewMessages(locale = 'en'): Record<string, Record<string, unknown>> {
  const out: Record<string, Record<string, unknown>> = {}
  for (const [path, mod] of Object.entries(modules)) {
    const match = path.match(/\/([^/]+)\/locales\/([^/]+)\/[^/]+\.json$/)
    if (!match || !match[1] || match[2] !== locale) continue
    const parts = match[1].split('-')
    const scope = (parts[0] ?? '') + parts.slice(1).map((p) => p.charAt(0).toUpperCase() + p.slice(1)).join('')
    out[scope] = { ...(out[scope] ?? {}), ...mod.default }
  }
  return out
}
