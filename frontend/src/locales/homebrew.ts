// Homebrew campaign locales are discovered from frontend/homebrew/<campaign>/locales/<locale>/,
// mirroring the backend's instance discovery: dropping a campaign directory in
// requires no i18n registration here. The kebab-case directory name (matching
// the campaign id, e.g. circus-ex-mortis for :circus-ex-mortis) camelCases to
// the message scope (circusExMortis, matching the backend campaignI18n scope);
// every JSON file in its locale folder is shallow-merged into that scope.
//
// Those per-campaign scopes are NOT loaded here any more -- locales/campaigns.ts
// fetches one when a game of that campaign is opened. Eagerly globbing them was
// pulling every homebrew campaign's text, in every locale, into the startup
// bundle: 328 KB brotli and 1.38 MB parsed, before the app could mount, so an
// English player downloaded the Korean and Chinese prose for sixteen campaigns
// they were not playing.
//
// `trait` is the ONE thing that still has to be here, and it is hoisted to the
// root rather than scoped. Homebrew traits are declared with
// `declareHomebrewTraits` into the engine's single flat trait universe -- they
// are not scoped to a campaign, and the UI looks them up as `trait.<Name>` with
// no campaign in hand. Scoping their labels would put them somewhere nothing
// can find, and deferring them would leave a trait blank until some unrelated
// campaign happened to load.
//
// Hence the narrower glob: a campaign declares its trait labels in its own
// `traits.json`, which is the convention every existing campaign already
// follows. A `trait` key in some other file of the campaign is no longer picked
// up globally -- it arrives scoped with the rest of that campaign, where
// nothing reads it -- so put trait labels in traits.json.
const traitModules = import.meta.glob('@homebrew/*/locales/*/traits.json', { eager: true }) as Record<
  string,
  { default: Record<string, unknown> }
>

const isRecord = (v: unknown): v is Record<string, unknown> =>
  typeof v === 'object' && v !== null && !Array.isArray(v)

/**
 * The root `trait` map for a locale: the core labels with every homebrew
 * campaign's labels merged on top.
 *
 * `baseTraits` is the core `trait` map this locale already has. Homebrew trait
 * labels are merged ON TOP of it and returned under the root `trait` key, so a
 * locale file can spread the result last without the shallow spread wiping the
 * core traits out -- which is exactly how two homebrew files sharing a scope
 * key already delete each other's subkeys.
 *
 * A campaign supplies labels by putting a top-level `trait` object in its
 * `traits.json`. English rarely needs one: an unlabelled trait falls back to
 * the camel split of its name, which matches the engine's `displayTrait`.
 * Other locales always do.
 */
export function homebrewTraits(
  locale = 'en',
  baseTraits: Record<string, unknown> = {},
): Record<string, Record<string, unknown>> {
  const traits: Record<string, unknown> = { ...baseTraits }

  for (const [path, mod] of Object.entries(traitModules)) {
    const match = path.match(/\/locales\/([^/]+)\/traits\.json$/)
    if (!match || match[1] !== locale) continue
    const { trait } = mod.default
    if (isRecord(trait)) Object.assign(traits, trait)
  }

  return Object.keys(traits).length ? { trait: traits } : {}
}
