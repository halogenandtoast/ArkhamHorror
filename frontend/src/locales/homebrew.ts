// Homebrew campaign locales are discovered from frontend/homebrew/<campaign>/locales/<locale>/,
// mirroring the backend's instance discovery: dropping a campaign directory in
// requires no i18n registration here. The kebab-case directory name (matching
// the campaign id, e.g. circus-ex-mortis for :circus-ex-mortis) camelCases to
// the message scope (circusExMortis, matching the backend campaignI18n scope);
// every JSON file in its locale folder is shallow-merged into that scope.
//
// `trait` is the ONE exception, and it is hoisted out to the root instead.
// Homebrew traits are declared with `declareHomebrewTraits` into the engine's
// single flat trait universe -- they are not scoped to a campaign, and the UI
// looks them up as `trait.<Name>` with no campaign in hand. Scoping their
// labels would put them somewhere nothing can find.
const modules = import.meta.glob('@homebrew/*/locales/*/*.json', { eager: true }) as Record<
  string,
  { default: Record<string, unknown> }
>

const isRecord = (v: unknown): v is Record<string, unknown> =>
  typeof v === 'object' && v !== null && !Array.isArray(v)

/**
 * Homebrew messages for a locale, by scope.
 *
 * `baseTraits` is the core `trait` map this locale already has. Homebrew trait
 * labels are merged ON TOP of it and returned under the root `trait` key, so a
 * locale file can spread the result last without the shallow spread wiping the
 * core traits out -- which is exactly how two homebrew files sharing a scope
 * key already delete each other's subkeys.
 *
 * A campaign supplies labels by putting a top-level `trait` object in any of
 * its locale JSON files; the convention is its own `traits.json`. English
 * rarely needs one: an unlabelled trait falls back to the camel split of its
 * name, which matches the engine's `displayTrait`. Other locales always do.
 */
export function homebrewMessages(
  locale = 'en',
  baseTraits: Record<string, unknown> = {},
): Record<string, Record<string, unknown>> {
  const out: Record<string, Record<string, unknown>> = {}
  const traits: Record<string, unknown> = { ...baseTraits }

  for (const [path, mod] of Object.entries(modules)) {
    const match = path.match(/\/([^/]+)\/locales\/([^/]+)\/[^/]+\.json$/)
    if (!match || !match[1] || match[2] !== locale) continue
    const parts = match[1].split('-')
    const scope = (parts[0] ?? '') + parts.slice(1).map((p) => p.charAt(0).toUpperCase() + p.slice(1)).join('')

    const { trait, ...rest } = mod.default
    if (isRecord(trait)) Object.assign(traits, trait)
    out[scope] = { ...(out[scope] ?? {}), ...rest }
  }

  if (Object.keys(traits).length) out.trait = traits
  return out
}
