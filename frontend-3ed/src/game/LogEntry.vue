<script setup lang="ts">
import { computed } from 'vue'
import { useGame } from '@/game/context'
import MythosTok from '@/game/MythosTok.vue'
import Tok from '@/game/Tok.vue'
import { LOG_TOKEN } from '@/game/util'

const props = defineProps<{ line: string }>()
const ctx = useGame()

type Entry =
  | { kind: 'round'; n: string }
  | { kind: 'marker'; color: string }
  | { kind: 'mythos'; token: string }
  | { kind: 'roll'; dice: { v: string; hit: boolean; need: number | null }[]; sum?: string }
  | { kind: 'result'; n: number }
  | { kind: 'end'; win: boolean }
  | { kind: 'clue'; where: string }
  | { kind: 'doom'; text: string }
  | { kind: 'plain'; cls: string }

const neighborhoodName = (id: string) => ctx.game.value?.board.neighborhoods[id]?.name ?? id

const entry = computed((): Entry => {
  const line = props.line
  let m: RegExpExecArray | null
  if ((m = /^Round (\d+)$/.exec(line))) return { kind: 'round', n: m[1] }
  // a marker turned face up (Profane Ritual): show the token that came up
  if ((m = /^Revealed an? (\w+) marker$/.exec(line))) return { kind: 'marker', color: m[1] }
  if ((m = /^Mythos: (\w+)$/.exec(line))) return { kind: 'mythos', token: m[1] }
  // the roll records what a die had to beat; older lines without it read as 5+
  if ((m = /^Rolled \[(.*)\](?: need (\d+))?$/.exec(line))) {
    const need = m[2] ? +m[2] : 5
    return {
      kind: 'roll',
      dice: m[1]
        .split(',')
        .filter(Boolean)
        .map((v) => ({ v: v.trim(), hit: +v >= need, need })),
    }
  }
  // a single die rolled outside a test (rule 474), e.g. Why Even Go On?
  if ((m = /^Rolled (\d+)$/.exec(line))) return { kind: 'roll', dice: [{ v: m[1], hit: false, need: null }] }
  if ((m = /^Rolled (\d+) and (\d+) \((\d+)\)$/.exec(line)))
    return {
      kind: 'roll',
      dice: [
        { v: m[1], hit: false, need: null },
        { v: m[2], hit: false, need: null },
      ],
      sum: m[3],
    }
  if ((m = /^Test result: (\d+)$/.exec(line))) return { kind: 'result', n: +m[1] }
  if (/^The investigators (win|lose)/.test(line) || /win the game|lose the game/.test(line))
    return { kind: 'end', win: /win/.test(line) }
  if ((m = /^A clue spawns in (.+)$/.exec(line))) return { kind: 'clue', where: neighborhoodName(m[1]) }
  if (/^(Gate burst|Outbreak|An anomaly)/.test(line))
    return { kind: 'doom', text: line.replace(/in ([a-z-]+)$/, (_, id: string) => `in ${neighborhoodName(id)}`) }
  if (/^Card \d+ (added|flips)|^Scenario: /.test(line)) return { kind: 'plain', cls: 'log-codex' }
  if (/joins the investigation| is (Defeated|Devoured|Retired)$/.test(line)) return { kind: 'plain', cls: 'log-inv' }
  if (/data missing|unavailable|Missing|hasn't been entered/.test(line)) return { kind: 'plain', cls: 'log-warn' }
  if (line.length > 110) return { kind: 'plain', cls: 'log-story' }
  return { kind: 'plain', cls: '' }
})
</script>

<template>
  <li v-if="entry.kind === 'round'" class="log-round"><span>Round {{ entry.n }}</span></li>
  <li v-else-if="entry.kind === 'marker'" class="log-entry log-mythos">
    <Tok :name="`${entry.color.toLowerCase()}-marker`" :title="`${entry.color} marker`" :size="30" /><span
      >Revealed a <b>{{ entry.color }}</b> marker</span
    >
  </li>
  <li v-else-if="entry.kind === 'mythos'" class="log-entry log-mythos">
    <MythosTok :token="entry.token" :size="30" /><span
      >Mythos: <b>{{ LOG_TOKEN[entry.token] ?? entry.token }}</b></span
    >
  </li>
  <li v-else-if="entry.kind === 'roll'" class="log-entry log-roll">
    <span class="log-label">Rolled</span
    ><span class="log-dice"
      ><span
        v-for="(d, k) in entry.dice"
        :key="k"
        class="log-die"
        :class="{ hit: d.hit }"
        :title="d.need === null ? undefined : `success on ${d.need}+`"
        >{{ d.v }}</span
      ></span
    ><b v-if="entry.sum">= {{ entry.sum }}</b>
  </li>
  <li v-else-if="entry.kind === 'result'" class="log-entry log-result" :class="entry.n > 0 ? 'pass' : 'fail'">
    <span class="log-badge">{{ entry.n > 0 ? 'Pass' : 'Fail' }}</span>{{ entry.n }} success{{ entry.n === 1 ? '' : 'es' }}
  </li>
  <li v-else-if="entry.kind === 'end'" class="log-entry log-end" :class="entry.win ? 'win' : 'lose'">{{ line }}</li>
  <li v-else-if="entry.kind === 'clue'" class="log-entry log-clue">
    <Tok name="clue" title="clue" :size="22" /><span
      >A clue spawns in <b>{{ entry.where }}</b></span
    >
  </li>
  <li v-else-if="entry.kind === 'doom'" class="log-entry log-doom">
    <Tok name="doom" title="doom" :size="22" /><span>{{ entry.text }}</span>
  </li>
  <li v-else class="log-entry" :class="entry.cls">{{ line }}</li>
</template>
