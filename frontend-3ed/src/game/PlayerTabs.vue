<script setup lang="ts">
import { computed, watch } from 'vue'
import { useGame } from '@/game/context'
import PlayerArea from '@/game/PlayerArea.vue'
import { ROLE_CLASS } from '@/game/util'

const ctx = useGame()
const g = computed(() => ctx.game.value!)

// defeated investigators go back in the box; only currently controlled ones get a tab
const sorted = computed(() => {
  const order = g.value.players.map((p) => p.investigator).filter((x): x is string => !!x)
  return Object.values(g.value.investigators)
    .filter((i) => order.includes(i.id))
    .sort((a, b) => order.indexOf(a.id) - order.indexOf(b.id))
})

// open on one of my own investigators the first time they are known, unless the
// remembered tab already is one; afterwards any tab stays selectable
let defaulted = false
watch(
  [sorted, ctx.myInvestigators],
  ([list, mine]) => {
    if (!list.length) return
    const ids = list.map((i) => i.id)
    const myIds = mine.filter((m) => ids.includes(m))
    if (!defaulted && myIds.length) {
      defaulted = true
      if (!myIds.includes(ctx.selectedTab.value ?? '')) {
        ctx.selectedTab.value = myIds.find((m) => m === g.value.turn) ?? myIds[0]
        return
      }
    }
    if (!ids.includes(ctx.selectedTab.value ?? '')) {
      ctx.selectedTab.value = myIds[0] ?? list.find((i) => i.id === g.value.turn)?.id ?? list[0].id
    }
  },
  { immediate: true },
)

const current = computed(() => sorted.value.find((i) => i.id === ctx.selectedTab.value) ?? null)
const hasChoices = (iid: string) => {
  const pid = ctx.playerOfInv(iid)
  return pid !== undefined && g.value.questions[pid] !== undefined
}
const tabClasses = (iid: string, status: string) => {
  const role = ctx.catalog.investigatorDefs?.[iid]?.roles?.[0]
  const pid = ctx.playerOfInv(iid)
  return [
    `tab--${ROLE_CLASS[role ?? ''] ?? 'Neutral'}`,
    iid === ctx.selectedTab.value ? 'tab--selected' : '',
    iid === g.value.turn ? 'tab--active-player' : '',
    pid !== undefined && pid === g.value.leader ? 'tab--lead-player' : '',
    status !== 'Playing' ? 'inactive' : '',
    iid !== ctx.selectedTab.value && hasChoices(iid) ? 'tab--has-actions' : '',
  ]
}
const tabTitle = (iid: string) => {
  const pid = ctx.playerOfInv(iid)
  const u = pid === undefined ? null : ctx.usernameOf(pid)
  return u ? `Played by ${u}` : undefined
}
</script>

<template>
  <em v-if="!sorted.length">None yet.</em>
  <template v-else>
    <div class="tabs-row">
      <ul class="tabs__header">
        <li
          v-for="i in sorted"
          :key="i.id"
          :class="tabClasses(i.id, i.status)"
          :title="tabTitle(i.id)"
          @click="ctx.selectTab(i.id)"
        >
          <i v-if="i.id !== ctx.selectedTab.value && hasChoices(i.id)" class="tab-pulse" aria-hidden="true"></i>
          <span>{{ ctx.invName(i.id) }}</span>
          <span v-if="sorted.length > 1 && hasChoices(i.id)" class="waiting-indicator" title="Waiting on this player"
            ><span class="waiting-spinner">⟳</span></span
          >
        </li>
      </ul>
    </div>
    <div class="tab"><PlayerArea v-if="current" :inv="current" /></div>
  </template>
</template>
