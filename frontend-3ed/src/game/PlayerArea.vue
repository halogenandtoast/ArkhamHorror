<script setup lang="ts">
import { computed } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import AssetCard from '@/game/AssetCard.vue'
import { useGame } from '@/game/context'
import DbgNum from '@/game/DbgNum.vue'
import MonsterCard from '@/game/MonsterCard.vue'
import { zoomFlip } from '@/game/overlays'
import Tok from '@/game/Tok.vue'
import { DBG_SKILLS, FOCUS, SKILL_ROWS } from '@/game/util'
import type { Investigator } from '@/types'

const props = defineProps<{ inv: Investigator }>()
const ctx = useGame()
const g = computed(() => ctx.game.value!)
const i = computed(() => props.inv)

const def = computed(() => ctx.catalog.investigatorDefs?.[i.value.id])
const front = computed(() => img(`investigators/${i.value.id}/front.webp`))
const back = computed(() => img(`investigators/${i.value.id}/back.webp`))
const standee = computed(() => img(`investigators/${i.value.id}/standee.webp`))
// a Map Skill Int arrives as [[skill, n], ...], not as an object keyed by skill
const focusPairs = computed<[string, number][]>(() =>
  Array.isArray(i.value.focus) ? i.value.focus : Object.entries(i.value.focus ?? {}),
)
const focus = computed(() => focusPairs.value.filter(([, v]) => v > 0))
const focusOf = (sk: string) => (focusPairs.value.find(([k]) => k === sk) ?? [sk, 0])[1]
const out = computed(() => i.value.status !== 'Playing')
const turn = computed(() => g.value.turn === i.value.id)
const pid = computed(() => ctx.playerOfInv(i.value.id))
const leader = computed(() => pid.value !== undefined && pid.value === g.value.leader)
const username = computed(() => (pid.value === undefined ? null : ctx.usernameOf(pid.value)))

// used and allowed actions this turn; the allowance comes from the server, which counts
// bonus actions and cards like Pocket Watch
const actions = computed(() => {
  if (g.value.phase !== 'ActionPhase') return null
  const total = ctx.view.value?.actionAllowance?.[i.value.id]
  if (total == null) return { total: null, used: i.value.actionsTaken ?? 0 }
  const used = Math.min(i.value.actionsTaken ?? 0, total)
  return { total, used }
})
const toks = computed(() => [
  ['damage', i.value.damage, `${i.value.damage} damage`, 'DebugSetDamage'],
  ['horror', i.value.horror, `${i.value.horror} horror`, 'DebugSetHorror'],
  ['money', i.value.money, `$${i.value.money}`, 'DebugSetMoney'],
  ['clue', i.value.clues, `${i.value.clues} clues`, 'DebugSetClues'],
  ['remnant', i.value.remnants, `${i.value.remnants} remnants`, 'DebugSetRemnants'],
] as [string, number, string, string][])
const engaged = computed(() =>
  Object.values(g.value.monsters).filter(
    (m) => ctx.inPlayerArea(m) && ((m.state.contents ?? []) as string[]).includes(i.value.id),
  ),
)
const skillEntries = computed(() => Object.entries(def.value?.skills ?? {}))

function setFocus(sk: string, e: Event) {
  void ctx.debugAction('DebugSetFocus', [i.value.id, sk, +(e.target as HTMLInputElement).value])
}
function setDelayed(e: Event) {
  void ctx.debugAction('DebugSetDelayed', [i.value.id, (e.target as HTMLInputElement).checked])
}
function gain() {
  void ctx.debugAction('DebugGainCard', [i.value.id, ctx.dbgCodeValue.value.trim()])
}
// conditions come by name rather than by card code, since one card carries two
function gainCondition(name: string) {
  void ctx.debugAction('DebugGainCondition', [i.value.id, name])
}
const standeeGone = computed(() => isBroken(standee.value))
</script>

<template>
  <div class="player-area" :class="{ out, turn }">
    <div
      class="sheet zoomable"
      :class="{ 'no-art': isBroken(front) }"
      title="Click to enlarge"
      @click="zoomFlip(front, back, 1134 / 900)"
    >
      <img v-if="!isBroken(front)" :src="front" :alt="ctx.invName(i.id)" @error="markBroken(front)" />
      <div class="sheet-text">
        <b>{{ ctx.invName(i.id) }}</b>
        <template v-if="def">
          <i>{{ def.occupation }}</i>
          <p>{{ def.abilityText }}</p>
          <p>Health {{ def.health }} · Sanity {{ def.sanity }} · Focus limit {{ def.focusLimit ?? '—' }}</p>
          <p>{{ skillEntries.map(([k, v]) => `${k} ${v}`).join(' · ') }}</p>
        </template>
      </div>
      <span v-for="[k, v] in focus" :key="k" class="sheet-focus" :style="{ top: `${(SKILL_ROWS[k] ?? 0) * 100}%` }"
        ><Tok :name="FOCUS[k] ?? 'focus-lore'" :count="v" :title="`${k} focus`" :size="30"
      /></span>
    </div>
    <div class="pa-side">
      <div class="pa-head">
        <Tok
          :name="`${leader ? 'first-player' : 'activation'}-${i.active ? 'active' : 'inactive'}`"
          :title="`${leader ? 'Leader, ' : ''}${i.active ? 'active' : 'inactive'}`"
          :size="40"
        />
        <img v-if="!standeeGone" class="standee" :class="{ delayed: i.delayed }" :src="standee" alt="" @error="markBroken(standee)" />
        <div>
          <div class="pa-name">
            {{ ctx.invName(i.id) }}<template v-if="turn">{{ " " }}<span class="pa-turn">taking a turn</span></template>
          </div>
          <div class="pa-where">{{ i.space ? ctx.spaceName(i.space) : '—' }}{{ i.delayed ? ' · delayed' : '' }}</div>
          <div v-if="username" class="pa-user">Played by {{ username }}</div>
          <template v-if="actions">
            <div v-if="actions.total === null" class="pa-actions">Actions used: {{ actions.used }}</div>
            <div v-else class="pa-actions" :title="`${actions.used} of ${actions.total} actions used this turn`">
              Actions
              <span class="pips"><span v-for="k in actions.total" :key="k" class="pip" :class="{ used: k - 1 < actions.used }"></span></span>
              {{ actions.used }} / {{ actions.total }}
            </div>
          </template>
          <span v-if="out" class="pa-status">{{ i.status }}</span>
        </div>
      </div>
      <div class="toks">
        <span v-for="[n, v, title, tag] in toks" :key="n" class="dbg-tokwrap"
          ><Tok :name="n" :count="v" :title="title" :size="34" always /><DbgNum
            v-if="ctx.dbgOn.value"
            :tag="tag"
            :iid="i.id"
            :value="v"
            :title="title"
        /></span>
      </div>
      <div v-if="ctx.dbgOn.value" class="dbg-inline">
        <span v-for="sk in DBG_SKILLS" :key="sk" class="dbg-tokwrap"
          ><Tok :name="FOCUS[sk] ?? 'focus-lore'" :count="focusOf(sk)" :title="`${sk} focus`" :size="28" always /><input
            class="dbg-num"
            type="number"
            min="0"
            :value="focusOf(sk)"
            :title="`${sk} focus`"
            @change="setFocus(sk, $event)"
        /></span>
        <label><input type="checkbox" :checked="i.delayed" @change="setDelayed" />delayed</label>
        <span class="dbg-gain"
          ><input v-model="ctx.dbgCodeValue.value" placeholder="card code" /><button @click="gain">Gain</button></span
        >
        <span class="dbg-gain"
          ><button title="Become BLESSED" @click="gainCondition('BLESSED')">Bless</button
          ><button title="Become CURSED" @click="gainCondition('CURSED')">Curse</button></span
        >
      </div>
      <div v-if="engaged.length" class="pa-engaged">
        <div class="pa-engaged-label">Engaged</div>
        <div class="pa-engaged-cards"><MonsterCard v-for="m in engaged" :key="m.card" :monster="m" /></div>
      </div>
      <div class="pa-cards">
        <AssetCard v-for="cid in i.assets" :key="cid" :cid="cid" />
        <em v-if="!i.assets.length" class="waiting">No possessions</em>
      </div>
    </div>
  </div>
</template>
