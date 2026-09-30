<script setup lang="ts">
import { computed } from 'vue'
import { cardImg, img, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import ExamineIcon from '@/game/ExamineIcon.vue'
import MythosTok from '@/game/MythosTok.vue'
import { zoom, zoomFlip } from '@/game/overlays'
import Tok from '@/game/Tok.vue'
import { FOCUS, show } from '@/game/util'
import type { CardId, Tagged } from '@/types'

// compact: an investigator named in passing -- traded with, attacked, chosen to
// act -- is their standee, not their whole sheet; only the setup pick needs that
const props = defineProps<{ label: Tagged; compact?: boolean }>()
const ctx = useGame()

const text = computed((): string | null => {
  const l = props.label
  const c = l.contents
  switch (l.tag) {
    case 'TextLabel':
    case 'DoneLabel':
    case 'AmountLabel':
      return String(c)
    case 'ActionLabel':
      return show(c)
        .replace(/Action$/, '')
        .replace(/([a-z])([A-Z])/g, '$1 $2')
    case 'SpaceLabel':
      return ctx.spaceName(c)
    case 'MonsterLabel':
      return 'Monster: ' + ctx.cardName(c)
    case 'CardLabel':
      return ctx.cardName(c)
    case 'DieLabel':
      return `Die ${c[0] + 1} (${c[1]})`
    case 'ScenarioLabel':
      return ctx.scenarioName(c)
    case 'SourceLabel':
      return ctx.sourceLabelText(c)
    case 'CardsLabel':
    case 'InvestigatorLabel':
    case 'SkillLabel':
    case 'TokenLabel':
      return null
    default:
      return show(l)
  }
})

const cards = computed(() =>
  props.label.tag === 'CardsLabel'
    ? (props.label.contents[1] as CardId[]).map((cid) => ({ cid, src: cardImg(ctx.cardCode(cid)) }))
    : [],
)
// choosing an investigator is easier with the sheet in front of you; the
// magnifier opens it flippable, since the back carries the starting possessions
const inv = computed(() => {
  if (props.label.tag !== 'InvestigatorLabel') return null
  const iid = props.label.contents as string
  return {
    iid,
    front: img(`investigators/${iid}/front.webp`),
    back: img(`investigators/${iid}/back.webp`),
    standee: img(`investigators/${iid}/standee.webp`),
  }
})
</script>

<template>
  <template v-if="text !== null">{{ text }}</template>
  <!-- a skill is the focus token that sits on it, which says it without the word -->
  <template v-else-if="label.tag === 'SkillLabel'"
    ><Tok :name="FOCUS[label.contents] ?? 'focus-lore'" :title="`${label.contents} focus`" :size="30" /></template
  >
  <template v-else-if="label.tag === 'TokenLabel'"
    ><MythosTok :token="label.contents" :size="24" /> {{ String(label.contents).replace(/Token$/, '') }}</template
  >
  <template v-else-if="label.tag === 'CardsLabel' && !cards.length">{{ label.contents[0] }}</template>
  <template v-else-if="label.tag === 'CardsLabel'"
    ><span class="label-cards"
      ><span v-for="c in cards" :key="c.cid" class="label-card" :class="{ 'no-art': isBroken(c.src) }"
        ><img v-if="!isBroken(c.src)" :src="c.src" :alt="ctx.cardNameRaw(c.cid) ?? ''" @error="markBroken(c.src)" /><span
          class="label-zoom"
          role="button"
          tabindex="0"
          title="Enlarge"
          @click.stop="zoom(c.src)"
          @keydown.enter.stop.prevent="zoom(c.src)"
          ><ExamineIcon /></span
        ></span
      ></span
    ><span class="label-cards-text">{{ label.contents[0] }}</span></template
  >
  <template v-else-if="inv && compact"
    ><img
      v-if="!isBroken(inv.standee)"
      class="label-standee"
      :src="inv.standee"
      :alt="ctx.invName(inv.iid)"
      @error="markBroken(inv.standee)"
    />{{ ctx.invName(inv.iid) }}</template
  >
  <template v-else-if="inv"
    ><span class="label-cards"
      ><span class="label-card label-sheet" :class="{ 'no-art': isBroken(inv.front) }"
        ><img v-if="!isBroken(inv.front)" :src="inv.front" :alt="ctx.invName(inv.iid)" @error="markBroken(inv.front)" /><span
          class="label-zoom"
          role="button"
          tabindex="0"
          title="Enlarge, then click to flip"
          @click.stop="zoomFlip(inv.front, inv.back, 1126 / 900)"
          @keydown.enter.stop.prevent="zoomFlip(inv.front, inv.back, 1126 / 900)"
          ><ExamineIcon /></span
        ></span
      ></span
    ><span class="label-cards-text">{{ ctx.invName(inv.iid) }}</span></template
  >
</template>
