<script setup lang="ts">
import { computed } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import { zoom } from '@/game/overlays'
import Tok from '@/game/Tok.vue'
import { SHROUDED } from '@/game/util'
import type { Monster } from '@/types'

const props = defineProps<{ monster: Monster }>()
const ctx = useGame()

const m = computed(() => props.monster)
const name = computed(() => ctx.cardNameRaw(m.value.card) ?? 'Monster')
const ready = computed(() => m.value.state?.tag === 'Ready')
const engaged = computed(() => m.value.state?.tag === 'Engaged')
const code = computed(() => ctx.cardCode(m.value.card))
const src = computed(() => img(`cards/${code.value}${ready.value ? '' : 'b'}.webp`))
const dmg = computed(() => m.value.damage ?? 0)
const title = computed(
  () =>
    `${name.value}${ready.value ? '' : engaged.value ? ' (engaged)' : ' (exhausted)'}${dmg.value ? ` · ${dmg.value} damage` : ''}`,
)
// a ready shrouded monster keeps its engaged/exhausted side hidden
const other = computed(() =>
  SHROUDED.has(code.value) && ready.value ? null : img(`cards/${code.value}${ready.value ? 'b' : ''}.webp`),
)
const open = () => (other.value ? zoom(src.value, other.value) : zoom(src.value))
</script>

<template>
  <span
    :data-mon="m.card"
    :data-card="m.card"
    :style="{ viewTransitionName: `mon-${m.card}` }"
    class="mon-card"
    :class="[{ engaged, 'no-art': isBroken(src) }, ctx.marks(['mon', m.card], ['card', m.card])]"
    :title="title"
    @click.stop="open"
  >
    <img v-if="!isBroken(src)" :src="src" alt="" @error="markBroken(src)" /><span class="chip mon">{{
      name.slice(0, 10)
    }}</span
    ><span v-if="dmg" class="mon-dmg"><Tok name="damage" :count="dmg" :title="`${dmg} damage`" :size="22" always /></span>
  </span>
</template>
