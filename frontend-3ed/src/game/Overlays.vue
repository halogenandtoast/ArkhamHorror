<script setup lang="ts">
import { computed, onUnmounted, ref, watch } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import { banner, closeZoom, flashState, zoomState } from '@/game/overlays'
import { NEIGHBOURHOOD_KEY, deckTag } from '@/game/util'
import type { CardId } from '@/types'

const ctx = useGame()
const z = zoomState

// capture, so escape closes the zoom instead of leaving the full-screen map
function onKey(e: KeyboardEvent) {
  if (e.key !== 'Escape') return
  e.preventDefault()
  e.stopPropagation()
  closeZoom()
}
const flipped = ref(false)
watch(z, (v, old) => {
  flipped.value = false
  if (v && !old) document.addEventListener('keydown', onKey, true)
  if (!v && old) document.removeEventListener('keydown', onKey, true)
})
onUnmounted(() => {
  document.removeEventListener('keydown', onKey, true)
  closeZoom()
})

// a possession's back is its owner's card back, unless the card has a second face of its own
const cardShown = computed(() => {
  const v = z.value
  if (v?.kind !== 'card') return null
  if (!isBroken(v.src)) return v.src
  const fb = v.owner ? img(`investigators/${v.owner}/card-back.webp`) : null
  return fb && !isBroken(fb) ? fb : null
})

const browse = computed(() => {
  const v = z.value
  if (v?.kind !== 'browse') return null
  const g = ctx.game.value
  if (!g) return null
  const cards: CardId[] = v.key.startsWith(NEIGHBOURHOOD_KEY)
    ? (g.decks.neighborhoods[v.key.slice(NEIGHBOURHOOD_KEY.length)] ?? [])
    : ((g.decks as unknown as Record<string, CardId[]>)[v.key] ?? [])
  return { key: v.key, cards, title: v.key.replace(NEIGHBOURHOOD_KEY, '') }
})
function drawFromDeck(cid: CardId) {
  const b = browse.value
  if (!b) return
  const iid = ctx.dbgIid()
  const tag = deckTag(b.key)
  closeZoom()
  void ctx.debugAction('DebugDrawCard', [iid, tag, cid])
}
const cardSrc = (cid: CardId) => img(`cards/${ctx.cardCode(cid)}.webp`)
</script>

<template>
  <div v-if="z" class="zoom" @click="closeZoom">
    <template v-if="z.kind === 'images'">
      <img v-for="(src, k) in z.srcs" :key="k" :src="src" alt="" />
    </template>
    <template v-else-if="z.kind === 'card'">
      <img v-if="cardShown" :src="cardShown" alt="" @error="markBroken(cardShown)" />
    </template>
    <template v-else-if="z.kind === 'flip'">
      <div
        class="sheet-flip sheet-zoom"
        :style="{ '--ratio': String(z.ratio) }"
        title="Click to flip"
        @click.stop="flipped = !flipped"
      >
        <div class="flip" :class="{ flipped, 'no-art': isBroken(z.shown) || isBroken(z.other) }">
          <img v-if="!isBroken(z.shown)" class="face back" :src="z.shown" alt="" @error="markBroken(z.shown)" />
          <img v-if="!isBroken(z.other)" class="face front" :src="z.other" alt="" @error="markBroken(z.other)" />
        </div>
      </div>
      <div class="zoom-hint">Click the sheet to flip it</div>
    </template>
    <template v-else-if="z.kind === 'marked'">
      <span class="zoom-marked"
        ><img class="zoom-marked-img" :src="z.src" alt="" /><span
          class="read-arrow"
          :style="{ top: `${z.top}%` }"
          title="Read this section"
        ></span
      ></span>
    </template>
    <div v-else-if="browse" class="browse" @click.stop>
      <h3>
        {{ browse.title }} · {{ browse.cards.length }} card{{ browse.cards.length === 1 ? '' : 's' }} · click one to draw it
      </h3>
      <div class="browse-grid">
        <figure
          v-for="(cid, n) in browse.cards"
          :key="`${cid}-${n}`"
          class="browse-card"
          :class="{ 'no-art': isBroken(cardSrc(cid)) }"
          :title="`${ctx.cardNameRaw(cid) ?? `Card ${cid}`} — click to draw it`"
          @click="drawFromDeck(cid)"
        >
          <img
            v-if="!isBroken(cardSrc(cid))"
            :src="cardSrc(cid)"
            :alt="ctx.cardNameRaw(cid) ?? `Card ${cid}`"
            @error="markBroken(cardSrc(cid))"
          />
          <span class="asset-name">{{ ctx.cardNameRaw(cid) ?? `Card ${cid}` }}</span><span class="browse-pos">{{ n + 1 }}</span>
        </figure>
        <em v-if="!browse.cards.length" class="waiting">This deck is empty.</em>
      </div>
    </div>
  </div>
  <div
    v-if="banner"
    :key="banner.key"
    class="phase-banner"
    role="status"
    aria-live="polite"
    :style="{ '--phase-color': banner.color }"
  >
    <span>{{ banner.name }}</span>
  </div>
  <div v-if="flashState.key" :key="flashState.key" id="flash" class="flash show">{{ flashState.text }}</div>
</template>
