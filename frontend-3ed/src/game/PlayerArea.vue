<script setup lang="ts">
import { computed, onUnmounted, ref, watch } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import AssetCard from '@/game/AssetCard.vue'
import CardText from '@/game/CardText.vue'
import Icon from '@/game/Icon.vue'
import { useGame } from '@/game/context'
import DbgNum from '@/game/DbgNum.vue'
import MonsterCard from '@/game/MonsterCard.vue'
import { zoomFlip } from '@/game/overlays'
import Tok from '@/game/Tok.vue'
import { cssName, DBG_SKILLS, FOCUS, SKILL_ICON, SKILL_ROWS } from '@/game/util'
import type { Investigator } from '@/types'

const props = defineProps<{ inv: Investigator }>()
const ctx = useGame()
const g = computed(() => ctx.game.value!)
const i = computed(() => props.inv)

const def = computed(() => ctx.catalog.investigatorDefs?.[i.value.id])
const front = computed(() => img(`investigators/${i.value.id}/front.webp`))
const back = computed(() => img(`investigators/${i.value.id}/back.webp`))
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

/* Money gathered, clues found, damage taken: the count changing is the whole
story of an action, so the token it lands on jumps and says how much. */
const bumps = ref<Record<string, number>>({})
const gains = ref<Record<string, number>>({})
const timers: Record<string, ReturnType<typeof setTimeout>> = {}
watch(toks, (now, before) => {
  if (!before) return
  const was = Object.fromEntries(before.map(([n, v]) => [n, v]))
  for (const [n, v] of now) {
    const prev = was[n]
    if (prev === undefined || v <= prev) continue
    bumps.value[n] = (bumps.value[n] ?? 0) + 1
    gains.value[n] = v - prev
    clearTimeout(timers[n])
    timers[n] = setTimeout(() => delete gains.value[n], 900)
  }
})
onUnmounted(() => Object.values(timers).forEach(clearTimeout))
const engaged = computed(() =>
  Object.values(g.value.monsters).filter(
    (m) => ctx.inPlayerArea(m) && ((m.state.contents ?? []) as string[]).includes(i.value.id),
  ),
)
// skills arrive as [[skill, n], ...] the way focus does, not as an object
const skillEntries = computed<[string, number][]>(() => {
  const sk = def.value?.skills
  return Array.isArray(sk) ? (sk as [string, number][]) : Object.entries(sk ?? {})
})

function setFocus(sk: string, e: Event) {
  void ctx.debugAction('DebugSetFocus', [i.value.id, sk, +(e.target as HTMLInputElement).value])
}
function setDelayed(e: Event) {
  void ctx.debugAction('DebugSetDelayed', [i.value.id, (e.target as HTMLInputElement).checked])
}
// conditions come by name rather than by card code, since one card carries two
function gainCondition(name: string) {
  void ctx.debugAction('DebugGainCondition', [i.value.id, name])
}
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
          <p><CardText :text="def.abilityText" /></p>
          <p>Health {{ def.health }} · Sanity {{ def.sanity }} · Focus limit {{ def.focusLimit ?? '—' }}</p>
          <p class="sheet-skills">
            <span v-for="e in skillEntries" :key="e[0]"><Icon :name="SKILL_ICON[e[0]] ?? 'lore'" :title="e[0]" /> {{ e[1] }}</span>
          </p>
        </template>
      </div>
      <span v-for="[k, v] in focus" :key="k" class="sheet-focus" :style="{ top: `${(SKILL_ROWS[k] ?? 0) * 100}%` }"
        ><Tok :name="FOCUS[k] ?? 'focus-lore'" :count="v" :title="`${k} focus`" :size="30"
      /></span>
      <!-- the leader's token, or the activation token, sits on the card's top corner -->
      <span class="sheet-lead"
        ><Tok
          :name="`${leader ? 'first-player' : 'activation'}-${i.active ? 'active' : 'inactive'}`"
          :title="`${leader ? 'Leader, ' : ''}${i.active ? 'active' : 'inactive'}`"
          :size="34"
      /></span>
      <!-- what they are carrying rides on the card itself, clear of the skill column -->
      <div class="toks" @click.stop>
        <span v-for="[n, v, title, tag] in toks" :key="n" class="dbg-tokwrap"
          ><Tok
            :key="`${n}${bumps[n] ?? 0}`"
            :class="{ gained: !!gains[n] }"
            :name="n"
            :count="v"
            :title="title"
            :size="30"
            always
          /><span v-if="gains[n]" class="tok-gain">+{{ gains[n] }}</span><DbgNum
            v-if="ctx.dbgOn.value"
            :tag="tag"
            :iid="i.id"
            :value="v"
            :title="title"
        /></span>
      </div>
    </div>
    <div class="pa-side">
      <!-- one line of status: where they are, who plays them, and what is true of them now -->
      <div class="pa-bar">
        <span
          class="pa-where"
          :class="{ 'pa-where-known': !!i.space }"
          :data-hl="i.space ? `space:${cssName(i.space)}` : undefined"
          :title="i.space ? 'Where they stand — hover to find it on the map' : undefined"
          >{{ i.space ? ctx.spaceName(i.space) : '—' }}</span
        >
        <span v-if="username" class="pa-user">{{ username }}</span>
        <span v-if="turn" class="pa-turn">taking a turn</span>
        <span v-if="i.delayed" class="pa-delayed" title="Delayed: they skip their next turn">delayed</span>
        <span v-if="out" class="pa-status">{{ i.status }}</span>
        <span v-if="actions" class="pa-actions" :title="`${actions.used} of ${actions.total ?? '—'} actions used this turn`">
          <template v-if="actions.total === null">Actions used: {{ actions.used }}</template>
          <template v-else>
            <span class="pips"><span v-for="k in actions.total" :key="k" class="pip" :class="{ used: k - 1 < actions.used }"></span></span>
            {{ actions.used }} / {{ actions.total }}
          </template>
        </span>
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
