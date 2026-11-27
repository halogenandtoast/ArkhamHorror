/* Campaign narrative text, fetched when a game asks for it instead of at boot.
 *
 * This is the bulk of the locale data and almost none of it is ever needed:
 * `src/locales/en/` is ~3.9 MB of source and ~3.2 MB of that is per-campaign
 * prose. It all used to be statically imported by `en.ts`, which `main.ts`
 * awaits *before* `app.mount()` -- so every cold load, including one that only
 * ever showed the games list, blocked on 2.74 MB of parsed JSON. Measured
 * against production it was 8.6s of a 13s load.
 *
 * A scope is discovered, not registered, the same way homebrew campaigns and
 * narrative variants are: a campaign is `<locale>/<scope>.ts`, or
 * `<locale>/<scope>/index.ts` once its text has outgrown one file. Adding a
 * campaign or translating an existing one needs no edit here.
 *
 * Both halves of the lookup already existed. `campaignIdToI18n` (types/Scenario)
 * maps a campaign to its scope name, and `mergeMessages` (locales/i18n) was
 * written for custom cards, whose text is likewise unknown at startup.
 *
 * Fallback is left to vue-i18n rather than reimplemented. A scope is merged
 * into the active locale AND into `en`, so a half-translated French campaign
 * resolves key by key exactly as it did when both were bundled -- translated
 * keys from `fr`, the rest through `fallbackLocale: 'en'`. A locale with no
 * file for a scope merges nothing of its own and falls back wholesale, which is
 * what `it.ts` importing `@/locales/en/theFeastOfHemlockVale` used to do by
 * hand.
 */
import { currentLocale, mergeMessages } from '@/locales/i18n'
import { normalizeLocale } from '@/locales/messages'

type Loaded = { default?: unknown }
type Loader = () => Promise<unknown>

/* `<locale>/<scope>.ts`. Every file at this depth is a campaign barrel -- the
 * core tables are JSON, and a campaign that keeps its own helper modules puts
 * them a level down. A stray module here would only ever add a loader nothing
 * asks for. */
const barrels = import.meta.glob('@/locales/*/*.ts') as Record<string, Loader>

/* `<locale>/<scope>/index.ts`, for the campaigns split across several files. */
const indexes = import.meta.glob('@/locales/*/*/index.ts') as Record<string, Loader>

/* Homebrew campaigns, whose locale folders are discovered rather than listed.
 * `traits.json` is deliberately still in here: homebrew.ts also loads it
 * eagerly, because a trait label is looked up as `trait.<Name>` with no
 * campaign in hand, and loading those few bytes twice is cheaper than teaching
 * this module which keys are global. The `trait` key is stripped below so a
 * campaign's scope does not get a copy nested inside it. */
const homebrew = import.meta.glob('@homebrew/*/locales/*/*.json') as Record<string, Loader>

const kebabToCamel = (name: string) => {
  const parts = name.split('-')
  return (parts[0] ?? '') + parts.slice(1).map((p) => p.charAt(0).toUpperCase() + p.slice(1)).join('')
}

/** locale -> scope -> the modules that make up that scope. */
const registry = new Map<string, Map<string, Loader[]>>()

const register = (locale: string, scope: string, load: Loader) => {
  let scopes = registry.get(locale)
  if (!scopes) registry.set(locale, (scopes = new Map()))
  const existing = scopes.get(scope)
  if (existing) existing.push(load)
  else scopes.set(scope, [load])
}

for (const [path, load] of Object.entries(barrels)) {
  const match = path.match(/\/locales\/([^/]+)\/([^/]+)\.ts$/)
  if (match?.[1] && match[2]) register(match[1], match[2], load)
}

for (const [path, load] of Object.entries(indexes)) {
  const match = path.match(/\/locales\/([^/]+)\/([^/]+)\/index\.ts$/)
  if (match?.[1] && match[2]) register(match[1], match[2], load)
}

for (const [path, load] of Object.entries(homebrew)) {
  const match = path.match(/\/([^/]+)\/locales\/([^/]+)\/[^/]+\.json$/)
  if (match?.[1] && match[2]) register(match[2], kebabToCamel(match[1]), load)
}

/* A Return-to box reuses its parent campaign's prose. `campaignIdToI18n` maps
 * both ids to the parent scope, so nothing should ask for this name -- but the
 * bundled locales all published it as an alias, and a key that silently stopped
 * resolving is a worse way to find that out than carrying one extra reference. */
const aliases: Record<string, string> = { theForgottenAge: 'returnToTheForgottenAge' }

const isRecord = (v: unknown): v is Record<string, unknown> =>
  typeof v === 'object' && v !== null && !Array.isArray(v)

/** In flight or finished, keyed `<locale>:<scope>`; a scope is fetched once. */
const inFlight = new Map<string, Promise<void>>()

async function loadInto(locale: string, scope: string): Promise<void> {
  const loaders = registry.get(locale)?.get(scope)
  if (!loaders?.length) return

  const key = `${locale}:${scope}`
  const started = inFlight.get(key)
  if (started) return started

  const run = (async () => {
    const modules = await Promise.all(loaders.map((load) => load() as Promise<Loaded>))

    let merged: Record<string, unknown> = {}
    for (const mod of modules) {
      const value = mod?.default ?? mod
      if (!isRecord(value)) continue
      // See the glob comment: `trait` is global and homebrew.ts owns it.
      const { trait: _trait, ...rest } = value
      merged = { ...merged, ...rest }
    }
    if (!Object.keys(merged).length) return

    const alias = aliases[scope]
    mergeMessages(locale, alias ? { [scope]: merged, [alias]: merged } : { [scope]: merged })
  })()

  inFlight.set(key, run)
  try {
    await run
  } catch (error) {
    // A failed chunk must not poison the scope forever: the app survives
    // missing prose (vue-i18n renders the key), and a retry should be able to
    // fix it once the network comes back.
    inFlight.delete(key)
    throw error
  }
}

/**
 * Make `scope`'s messages available, in the active locale and in `en`.
 *
 * Safe to call with a scope that has no files, with `null`, and repeatedly --
 * which is what lets callers put it on a watcher without tracking what they
 * have already asked for.
 */
export async function loadCampaignMessages(scope: string | null | undefined): Promise<void> {
  if (!scope) return
  // `campaignIdToI18n` answers "standalone.guardiansOfTheAbyss" for a
  // standalone; the file that holds it is the root scope's.
  const root = scope.split('.')[0]
  if (!root) return

  const locale = normalizeLocale(currentLocale())
  const targets = locale === 'en' ? ['en'] : ['en', locale]
  await Promise.all(targets.map((target) => loadInto(target, root)))
}
