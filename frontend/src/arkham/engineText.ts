/* Plain English the engine still sends instead of an i18n key.
 *
 * Most prompts arrive as `$key` tokens, but some choice labels, amount prompts
 * and their row titles are written as literal text in the engine ("Amount of
 * Charges to Spend", "Charges", "Use " <> skill). A locale can translate those
 * by their exact English, or by a pattern for the ones built around a value,
 * in `src/locales/<locale>/engineText.json`:
 *
 *   { "exact": { "Charges": "충전" },
 *     "patterns": [["^Use (.+)$", "{1} 사용"]] }
 *
 * A pattern's captures are looked up in `exact` too, so "Give Red key" can
 * become "빨간색 열쇠 주기" without a pattern per key colour. Text with no entry
 * is returned unchanged, so English (and any locale without the file) is
 * unaffected.
 */
import { currentLocale } from '@/locales/i18n'

export interface EngineTextTable {
  exact?: Record<string, string>
  patterns?: [string, string][]
}

interface CompiledTable {
  exact: Map<string, string>
  patterns: [RegExp, string][]
}

const tables = new Map<string, CompiledTable>()

export function registerEngineText(locale: string, table: EngineTextTable | undefined) {
  if (!table) return
  tables.set(locale, {
    exact: new Map(Object.entries(table.exact ?? {})),
    patterns: (table.patterns ?? []).map(([source, template]) => [new RegExp(source), template]),
  })
}

export function localizeEngineText(text: string, locale = currentLocale()): string {
  const table = tables.get(locale)
  if (!table || !text) return text
  const trimmed = text.trim()
  const exact = table.exact.get(trimmed)
  if (exact !== undefined) return exact
  for (const [pattern, template] of table.patterns) {
    const match = trimmed.match(pattern)
    if (!match) continue
    return template.replace(/\{(\d+)\}/g, (_, n: string) => {
      const part = match[Number(n)] ?? ''
      return table.exact.get(part.trim()) ?? part
    })
  }
  return text
}
