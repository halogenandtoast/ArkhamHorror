<script setup lang="ts">
import { computed } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import { zoomCard } from '@/game/overlays'
import Tok from '@/game/Tok.vue'
import type { CardId } from '@/types'

const props = defineProps<{ cid: CardId }>()
const ctx = useGame()
const g = computed(() => ctx.game.value!)

const a = computed(() => g.value.assets[props.cid] ?? {})
const name = computed(() => ctx.cardNameRaw(props.cid) ?? String(props.cid))
const src = computed(() => ctx.cardFace(props.cid, !!a.value.flipped))
// a possession's back is its owner's card back, unless the card has a second face of its own
const owner = computed(() => (a.value.flipped ? (a.value.owner ?? '') : ''))
const fallback = computed(() => (owner.value ? img(`investigators/${owner.value}/card-back.webp`) : null))
const shown = computed(() =>
  !isBroken(src.value) ? src.value : fallback.value && !isBroken(fallback.value) ? fallback.value : null,
)
const inTest = computed(() => (g.value.test?.chosenAssets ?? []).includes(props.cid))
</script>

<template>
  <div
    class="asset zoomable"
    :class="[{ 'in-test': inTest, 'no-art': !shown }, ctx.marks(['card', cid])]"
    :data-card="cid"
    :title="`${name}${inTest ? ' (used in this test)' : ''} — click to enlarge`"
    @click="zoomCard(src, owner)"
  >
    <img v-if="shown" :src="shown" :alt="name" @error="markBroken(shown)" /><span class="asset-name">{{ name }}</span
    ><span v-if="inTest" class="in-test-tag">In test</span>
    <div v-if="a.damage || a.horror" class="asset-badges">
      <Tok v-if="a.damage" name="damage" :count="a.damage" :title="`${a.damage} damage`" :size="26" />
      <Tok v-if="a.horror" name="horror" :count="a.horror" :title="`${a.horror} horror`" :size="26" />
    </div>
  </div>
</template>
