import { computed, watch, type Ref } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import { narrativeMessages } from '@/locales/narratives'
import narrativesJSON from '@/arkham/data/narratives.json'
import campaignJSON from '@/arkham/data/campaigns.json'
import { escapeIconPlaceholders } from '@/locales/messages'

export type NarrativeEntry = {
  key: string
  /* Who actually wrote it. The variant signs itself with an in-fiction
   * pseudonym, which is the title's business and not the credit's. */
  designer?: string
  /* Where the original document lives, so a player can go read it. */
  url?: string
  /* Gated and labelled like any other beta content: the prose and the beats a
   * variant adds have had far less play than the Campaign Guide's. */
  beta?: boolean
  campaigns: Record<string, { name: string }>
}

export const narratives = narrativesJSON as NarrativeEntry[]

/* A "Return to" box tells the same story out of the same locale scope, so a
variant written for a campaign covers its Return too. The boxes are keyed by
their own campaign id, which this maps back to the campaign a variant names. */
const returnToBase = new Map<string, string>(
  (campaignJSON as { id: string; returnTo?: { id: string } }[])
    .filter((c) => c.returnTo?.id)
    .map((c) => [c.returnTo!.id, c.id]),
)

export function baseCampaignId(campaignId: string | null | undefined): string | null {
  if (!campaignId) return null
  return returnToBase.get(campaignId) ?? campaignId
}

export function narrativeEntry(key: string | null | undefined): NarrativeEntry | null {
  if (!key) return null
  return narratives.find((n) => n.key === key) ?? null
}

/* The host a variant's link points at, shown as the link's own text so there is
no label to translate and no doubt about where it leads. */
export function narrativeHost(url: string | undefined): string | null {
  if (!url) return null
  try {
    return new URL(url).host.replace(/^www\./, '')
  } catch {
    return null
  }
}

/* The variant's name for one campaign, as its own document titles itself. */
export function narrativeTitle(
  entry: NarrativeEntry,
  campaignId: string | null | undefined,
): string | null {
  const id = baseCampaignId(campaignId)
  if (!id) return null
  return entry.campaigns[id]?.name ?? null
}

/* A variant's own one-line description of itself, out of its locale file.

Read straight from the variant's messages rather than through `t`, because the
chooser runs before any game exists and so before `useNarrative` has merged
anything. Keyed by locale with an English fallback, so it translates with the
rest of the variant. */
export function narrativeBlurb(
  variant: string,
  campaignScope: string | null,
  locale: string,
): string | null {
  if (!campaignScope) return null
  const at = (l: string) => {
    const scoped = narrativeMessages(l)[variant]?.[campaignScope] as
      | { about?: { blurb?: unknown } }
      | undefined
    const blurb = scoped?.about?.blurb
    return typeof blurb === 'string' ? blurb : null
  }
  return at(locale) ?? at('en')
}

/* The narrative variants offered for a campaign, by its id. */
export function narrativesFor(campaignId: string | null | undefined): NarrativeEntry[] {
  const id = baseCampaignId(campaignId)
  if (!id) return []
  return narratives.filter((n) => id in n.campaigns)
}

/* The variant a game was created with. It is a campaign option, so it is in the
 * log rather than on the campaign record, and it carries its name in `contents`. */
export function narrativeOf(game: Game | null | undefined): string | null {
  const options = game?.campaign?.log?.options ?? game?.scenario?.standaloneCampaignLog?.options
  const option = (options ?? []).find((o) => o.tag === 'NarrativeVariant')
  return option?.contents ?? null
}

/* The untouched messages for each scope a variant can rewrite, captured before
 * anything is merged over it so the campaign's own wording can always be put
 * back. Keyed by locale and scope; module-level, because the pristine copy must
 * outlive any one component. */
const pristine = new Map<string, unknown>()

const isObject = (v: unknown): v is Record<string, unknown> =>
  !!v && typeof v === 'object' && !Array.isArray(v)

function deepMerge(base: unknown, over: unknown): unknown {
  if (!isObject(base) || !isObject(over)) return over
  const out: Record<string, unknown> = { ...base }
  for (const [k, v] of Object.entries(over)) out[k] = k in base ? deepMerge(base[k], v) : v
  return out
}

function normalize(value: unknown): unknown {
  if (typeof value === 'string') return escapeIconPlaceholders(value)
  if (Array.isArray(value)) return value.map(normalize)
  if (isObject(value)) {
    const out: Record<string, unknown> = {}
    for (const [k, v] of Object.entries(value)) out[k] = normalize(v)
    return out
  }
  return value
}

/* Apply a variant's messages to the campaign scopes it rewrites, or restore the
 * Campaign Guide's for every scope any variant could have touched.
 *
 * Restoring is unconditional rather than tracked, so leaving a variant game for
 * one without a variant in the same tab can never leave a rewrite behind. */
export function useNarrative(game: Ref<Game | null | undefined>) {
  const i18n = useI18n({ useScope: 'global' })
  const variant = computed(() => narrativeOf(game.value))

  const apply = (key: string | null) => {
    const locale = i18n.locale.value
    const all = narrativeMessages(locale)
    const messages = { ...(i18n.getLocaleMessage(locale) as Record<string, unknown>) }
    let changed = false

    for (const [variantKey, scopes] of Object.entries(all)) {
      for (const [scope, overrides] of Object.entries(scopes)) {
        const cacheKey = `${locale}:${scope}`
        if (!pristine.has(cacheKey)) pristine.set(cacheKey, messages[scope])
        const base = pristine.get(cacheKey)
        messages[scope] = variantKey === key ? deepMerge(base, normalize(overrides)) : base
        changed = true
      }
    }

    if (changed) i18n.setLocaleMessage(locale, messages as never)
  }

  watch([variant, i18n.locale], ([key]) => apply(key ?? null), { immediate: true })

  return { variant, entry: computed(() => narrativeEntry(variant.value)) }
}
