/* Community narrative variants: rewrites of a campaign's prose that leave its
 * mechanics alone.
 *
 * A variant's messages are discovered from
 * frontend/src/locales/<locale>/narratives/<variant>/<campaignScope>.json,
 * mirroring how homebrew locales are discovered: dropping a file in requires no
 * registration here. The file's top-level keys are the campaign's own, because
 * they are merged over that campaign's scope -- so a block the variant only
 * rewords resolves to its prose at the key the engine already emits, and every
 * key it leaves out keeps the Campaign Guide's wording.
 *
 * A variant's `about` block rides along in the same file and lands at
 * <campaignScope>.about, which is what the Campaign Log's About tab reads. */
const modules = import.meta.glob('@/locales/*/narratives/*/*.json', { eager: true }) as Record<
  string,
  { default: Record<string, unknown> }
>

export type NarrativeMessages = Record<string, Record<string, unknown>>

/* variant key -> campaign scope -> the messages to merge over that scope. */
export function narrativeMessages(locale = 'en'): Record<string, NarrativeMessages> {
  const out: Record<string, NarrativeMessages> = {}
  for (const [path, mod] of Object.entries(modules)) {
    const match = path.match(/\/locales\/([^/]+)\/narratives\/([^/]+)\/([^/]+)\.json$/)
    if (!match) continue
    const [, fileLocale, variant, scope] = match
    if (fileLocale !== locale || !variant || !scope) continue
    out[variant] = { ...(out[variant] ?? {}), [scope]: mod.default }
  }
  return out
}

export function narrativeScopes(variant: string, locale = 'en'): string[] {
  return Object.keys(narrativeMessages(locale)[variant] ?? {})
}
