// A homebrew campaign draws one of its own questions by dropping a component at
// `frontend/homebrew/<campaign>/question-panels/<label path>.vue`, where the file
// is named after the question's label key with its campaign scope and `label.`
// prefix stripped (`$darkMatter.label.scienceExpansion.purchase` ->
// `scienceExpansion.purchase.vue`). Discovered exactly like the campaign's
// locales, `campaign.json` and its log panels: nothing is registered centrally.
//
// The component is handed `{ game, playerId, viewOnly }` and emits
// `choose(index)` against the question's own choices -- the same contract
// `UltimatumsAndBoonsQuestion` uses -- and owns the whole panel, so a campaign
// can lay its cards out and style them however its own content needs.
import type { Component } from 'vue'

const modules = import.meta.glob('@homebrew/*/question-panels/*.vue', { eager: true }) as Record<
  string,
  { default: Component }
>

// campaign scope -> label path -> component
const panels: Record<string, Record<string, Component>> = {}
for (const [path, mod] of Object.entries(modules)) {
  const match = path.match(/\/([^/]+)\/question-panels\/(.+)\.vue$/)
  if (!match || !match[1] || !match[2]) continue
  const parts = match[1].split('-')
  const scope = (parts[0] ?? '') + parts.slice(1).map((p) => p.charAt(0).toUpperCase() + p.slice(1)).join('')
  panels[scope] = { ...(panels[scope] ?? {}), [match[2]]: mod.default }
}

/* The panel for a question, if the campaign that asked it supplied one.
 *
 * Keyed off the label rather than the active campaign, because the label already
 * names the campaign scope that produced it (`campaignI18n` scopes every label a
 * campaign builds), so a side story asking under another campaign's log cannot
 * pick up the wrong panel. */
export function homebrewQuestionPanel(label: string | null | undefined): Component | undefined {
  if (!label) return undefined
  const match = label.replace(/^\$/, '').match(/^([^.]+)\.label\.(.+)$/)
  if (!match || !match[1] || !match[2]) return undefined
  return panels[match[1]]?.[match[2]]
}
