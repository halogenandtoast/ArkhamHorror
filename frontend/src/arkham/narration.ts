/* Which pre-rendered story narration to play for the current game state.
 *
 * Every client receives every player's pending question, so when the lead
 * investigator is shown an intro or resolution, each table member's browser
 * sees the same `Read` question at the same moment and plays the same file:
 * the table hears the story together, each in their own narration language.
 *
 * Import-free (types only) so tests can load it without the Vue toolchain. */
import type { FlavorTextEntry } from '@/arkham/types/FlavorText'
import type { Question } from '@/arkham/types/Question'

export type NarrationLanguage = 'en' | 'vi'
export const NARRATION_LANGUAGES: NarrationLanguage[] = ['en', 'vi']

/* public/audio/narration/manifest.json, written by scripts/narration/generate_narration.py:
 * language -> i18n key -> content hash (the hash busts the browser cache when a file is re-rendered). */
export type NarrationManifest = {
  version: number
  languages: Partial<Record<NarrationLanguage, Record<string, string>>>
}

export type NarrationTrack = { key: string; url: string }

/* The i18n keys a flavor text is built from, in reading order. */
export function entryKeys(entries: FlavorTextEntry[]): string[] {
  const out: string[] = []
  const walk = (e: FlavorTextEntry) => {
    switch (e.tag) {
      case 'I18nEntry':
        out.push(e.key)
        break
      case 'ModifyEntry':
      case 'FootnoteEntry':
        walk(e.entry)
        break
      case 'CompositeEntry':
      case 'ColumnEntry':
        e.entries.forEach(walk)
        break
      case 'ListEntry': {
        const items = (list: { entry: FlavorTextEntry; nested: unknown[] }[]): void =>
          list.forEach((i) => {
            walk(i.entry)
            items(i.nested as { entry: FlavorTextEntry; nested: unknown[] }[])
          })
        items(e.list)
        break
      }
      default:
        break
    }
  }
  entries.forEach(walk)
  return out
}

function readEntries(question: Question | undefined): FlavorTextEntry[] {
  if (!question) return []
  if (question.tag === 'Read') return question.flavorText.body
  if (question.tag === 'QuestionWithSource' || question.tag === 'QuestionLabel') {
    return readEntries((question as { question: Question }).question)
  }
  return []
}

/* The story keys any player is being shown right now, de-duplicated and in
 * player order (several players can be shown the same text at once). */
export function storyKeysInPlay(questions: Record<string, Question> | null | undefined): string[] {
  const seen = new Set<string>()
  for (const question of Object.values(questions ?? {})) {
    for (const key of entryKeys(readEntries(question))) seen.add(key)
  }
  return [...seen]
}

/* The tracks to play for those keys: only keys with a rendered file, in the
 * chosen language, falling back to English for a key not translated yet. */
export function narrationTracks(
  keys: string[],
  manifest: NarrationManifest | null,
  language: NarrationLanguage,
  root = '/audio/narration',
): NarrationTrack[] {
  if (!manifest) return []
  const tracks: NarrationTrack[] = []
  for (const key of keys) {
    const lang = manifest.languages[language]?.[key] ? language : manifest.languages.en?.[key] ? 'en' : null
    if (!lang) continue
    const hash = manifest.languages[lang]![key]
    tracks.push({ key, url: `${root}/${lang}/${encodeURIComponent(key)}.mp3?v=${hash}` })
  }
  return tracks
}

/* The narration language to start with: Vietnamese for a Vietnamese browser, else English. */
export function defaultNarrationLanguage(browserLanguages: readonly string[]): NarrationLanguage {
  return browserLanguages.some((l) => l.toLowerCase().startsWith('vi')) ? 'vi' : 'en'
}

export function readNarrationLanguage(value: string | null, fallback: NarrationLanguage): NarrationLanguage {
  return NARRATION_LANGUAGES.find((l) => l === value) ?? fallback
}
