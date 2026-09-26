<script setup lang="ts">
import { computed } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import MonsterCard from '@/game/MonsterCard.vue'
import Tok from '@/game/Tok.vue'
import { cssName } from '@/game/util'

const props = defineProps<{ sid: string }>()
const ctx = useGame()

const space = computed(() => ctx.game.value!.board.spaces[props.sid])
const invs = computed(() => Object.values(ctx.game.value!.investigators).filter((i) => i.space === props.sid))
const monsters = computed(() =>
  Object.values(ctx.game.value!.monsters).filter((m) => m.space === props.sid && !ctx.inPlayerArea(m)),
)
const tokenSrc = (iid: string) => img(`investigators/${iid}/token.webp`)
// the one walking may be dragged along their route; in debug, anyone may be dragged
// anywhere, which puts them down directly
const draggable = (iid: string) => ctx.moveDrag.value?.iid === iid || ctx.dbgOn.value

function dragStart(e: DragEvent, iid: string) {
  if (!draggable(iid)) return
  e.dataTransfer?.setData('text/plain', iid)
  if (e.dataTransfer) e.dataTransfer.effectAllowed = 'move'
  document.body.classList.add('moving')
}
function dragEnd() {
  document.body.classList.remove('moving')
}
</script>

<template>
  <template v-if="space">
    <span v-if="space.doom" :style="{ viewTransitionName: cssName(`doom-${sid}`), display: 'inline-flex' }"
      ><Tok name="doom" :count="space.doom" :title="`${space.doom} doom`"
    /></span>
    <span v-if="space.clues" :style="{ viewTransitionName: cssName(`clue-${sid}`), display: 'inline-flex' }"
      ><Tok name="clue" :count="space.clues" :title="`${space.clues} clues`"
    /></span>
    <template v-for="(m, k) in space.markers" :key="`marker-${k}`">
      <Tok v-if="m.faceUp" :name="`${m.color}-marker`" :title="`${m.color} marker`" />
      <Tok v-else name="marker-back" title="facedown marker" />
    </template>
    <template v-for="i in invs" :key="`inv-${i.id}`">
      <span
        v-if="isBroken(tokenSrc(i.id))"
        class="chip inv"
        :title="ctx.invName(i.id)"
        :data-inv="cssName(i.id)"
        :class="[ctx.marks(['inv', cssName(i.id)]), { 'draggable-inv': draggable(i.id) }]"
        :draggable="draggable(i.id) ? 'true' : undefined"
        @dragstart="dragStart($event, i.id)"
        @dragend="dragEnd"
        >{{ ctx.initials(i.id) }}</span
      >
      <img
        v-else
        :data-inv="cssName(i.id)"
        :style="{ viewTransitionName: `inv-${cssName(i.id)}` }"
        class="inv-tok"
        :class="[ctx.marks(['inv', cssName(i.id)]), { 'draggable-inv': draggable(i.id) }]"
        :src="tokenSrc(i.id)"
        :title="ctx.invName(i.id)"
        :draggable="draggable(i.id) ? 'true' : undefined"
        @dragstart="dragStart($event, i.id)"
        @dragend="dragEnd"
        @error="markBroken(tokenSrc(i.id))"
      />
    </template>
    <MonsterCard v-for="m in monsters" :key="`mon-${m.card}`" :monster="m" />
  </template>
</template>
