// Campaigns opt in by providing homebrew/<campaign>/log-editor.vue.
// Editors receive { game, log } and can emit refresh after saving.
import type { Component } from 'vue'
import { homebrewCampaignScope } from '@/arkham/homebrewData'

const modules = import.meta.glob('@homebrew/*/log-editor.vue', { eager: true }) as Record<
  string,
  { default: Component }
>
const editors: Record<string, Component> = {}
for (const [path, module] of Object.entries(modules)) {
  const campaign = path.match(/\/([^/]+)\/log-editor\.vue$/)?.[1]
  if (campaign) editors[homebrewCampaignScope(campaign)] = module.default
}

export function campaignLogEditor(scope: string | undefined): Component | undefined {
  return scope ? editors[scope] : undefined
}
