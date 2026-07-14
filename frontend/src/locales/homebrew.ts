// Homebrew campaign locales are discovered from the campaign directories
// themselves (backend/arkham-api/library/Arkham/Homebrew/<Name>/locales/en/),
// mirroring the backend's instance discovery: dropping a campaign directory in
// requires no i18n registration here. The directory name (PascalCase) becomes
// the message scope (camelCase), e.g. CircusExMortis -> circusExMortis, and
// every JSON file in its locales/en/ folder is shallow-merged into that scope.
const modules = import.meta.glob('@homebrew/*/locales/en/*.json', { eager: true }) as Record<
  string,
  { default: Record<string, unknown> }
>

export function homebrewMessages(): Record<string, Record<string, unknown>> {
  const out: Record<string, Record<string, unknown>> = {}
  for (const [path, mod] of Object.entries(modules)) {
    const match = path.match(/\/([^/]+)\/locales\/en\/[^/]+\.json$/)
    if (!match || !match[1]) continue
    const scope = match[1].charAt(0).toLowerCase() + match[1].slice(1)
    out[scope] = { ...(out[scope] ?? {}), ...mod.default }
  }
  return out
}
