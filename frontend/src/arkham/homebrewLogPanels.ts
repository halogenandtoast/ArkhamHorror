// A homebrew campaign renders one of its own recorded sets by dropping a
// component at `frontend/homebrew/<campaign>/log-panels/<setKey>.vue`, where
// `<setKey>` is the recorded set's key with a lower-case first letter
// (`Destinies` -> `destinies.vue`). Discovered exactly like the campaign's
// locales and `campaign.json`: nothing is registered anywhere central.
//
// The component is handed `{ entries, game }` -- the recorded set's values as
// the log decoded them, and the game -- and draws the inside of the log box;
// the title and the box itself stay with `CampaignLogRecordedSets`, so a panel
// cannot drift out of the campaign log's visual language.
import type { Component } from 'vue'
import { homebrewCampaignScope } from '@/arkham/homebrewData'

const modules = import.meta.glob('@homebrew/*/log-panels/*.vue', { eager: true }) as Record<
  string,
  { default: Component }
>

// campaign scope -> recorded set key -> component
const panels: Record<string, Record<string, Component>> = {}
for (const [path, mod] of Object.entries(modules)) {
  const match = path.match(/\/([^/]+)\/log-panels\/([^/]+)\.vue$/)
  if (!match || !match[1] || !match[2]) continue
  const scope = homebrewCampaignScope(match[1])
  const setKey = match[2].charAt(0).toLowerCase() + match[2].slice(1)
  panels[scope] = { ...(panels[scope] ?? {}), [setKey]: mod.default }
}

/* The panel for a recorded set, if its campaign supplied one.
 *
 * Matched on the campaign whose log is being rendered plus the key's last
 * segment, because the path a key formats to is not reliably scoped: a key
 * recorded before its campaign namespaced them (Circus Ex Mortis' own keys, and
 * every key already sitting in a save) formats to `homebrewCampaignLog.key.x`
 * rather than `<campaign>.key.x`. Keying off the campaign instead also keeps two
 * campaigns' same-named sets apart.
 */
export function homebrewLogPanel(scope: string | undefined, keyPath: string): Component | undefined {
  if (!scope) return undefined
  return panels[scope]?.[keyPath.split('.').pop() ?? keyPath]
}
