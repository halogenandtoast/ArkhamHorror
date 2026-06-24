import { reactive } from 'vue'
import { updateGameRaw } from '@/arkham/api'
import type { Scenario } from '@/arkham/types/Scenario'

// Scenario count keys that can be edited from the Scenario Debug modal. `key` is
// the ScenarioCountKey tag (see Arkham.ScenarioLogKey); the current value is read
// from `scenario.counts[key]` and updated via a `ScenarioCountSet` message.
export interface ScenarioDebugCount {
  key: string
  label: string
}

const scenarioDebugCounts: Record<string, ScenarioDebugCount[]> = {
  c83001: [{ key: 'StrengthOfTheAbyss', label: 'Strength of the Abyss' }], // The Eternal Slumber
  c83016: [{ key: 'StrengthOfTheAbyss', label: 'Strength of the Abyss' }], // The Night's Usurper
  c04277: [{ key: 'CurrentDepth', label: 'Depth' }], // The Depths of Yoth
}

export function scenarioDebugCountsFor(scenario: Scenario): ScenarioDebugCount[] {
  return scenarioDebugCounts[scenario.id] ?? []
}

// Scenarios with a bespoke debug section (beyond the generic count editors above).
const scenariosWithCustomDebugOptions = ['c85001'] // The Blob That Ate Everything

export function scenarioHasDebugOptions(scenario: Scenario): boolean {
  return scenariosWithCustomDebugOptions.includes(scenario.id)
    || scenarioDebugCountsFor(scenario).length > 0
}

const debug = reactive({
  active: false,
  toggle: () => {
    debug.active = !debug.active
  },
  send: async (gameId: string, message: any) => updateGameRaw(gameId, message)
})

export function useDebug() {
  return debug
}
