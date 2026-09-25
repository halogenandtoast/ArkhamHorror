<script setup lang="ts">
import { computed } from 'vue'
import { isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import MythosTok from '@/game/MythosTok.vue'
import { zoom, zoomMarked } from '@/game/overlays'
import { LOG_TOKEN } from '@/game/util'
import type { Encounter, Game } from '@/types'

const ctx = useGame()
const g = computed(() => ctx.game.value!)

// the drawn mythos token sits above the active card until its continue prompt is answered;
// keyed on the draw count so unrelated re-renders do not restart the animation
const token = computed(() => g.value.activeToken ?? null)
const tokenKey = computed(() => (token.value ? `${token.value}:${g.value.drawnTokens.length}` : ''))
const tokenName = computed(() => (token.value ? (LOG_TOKEN[token.value] ?? token.value.replace(/Token$/, '')) : ''))

// neighborhood and event cards print one section per location, alphabetically, top to bottom
function encounterSection(game: Game, e: Encounter, where: string | null | undefined) {
  if (e.deck?.tag !== 'NeighborhoodDeck' || !where) return null
  const names = Object.values(game.board.spaces)
    .filter((s) => s.neighborhood === e.deck!.contents)
    .map((s) => s.name)
    .sort((a, b) => a.localeCompare(b))
  const i = names.indexOf(game.board.spaces[where]?.name)
  return names.length === 3 && i >= 0 ? i : null
}
// Where to point on the encounter card: cards read by a doom range (anomaly, terror) carry
// the section the engine picked; anomaly cards print a title band above their sections.
function readArrowTop(game: Game, e: Encounter, where: string | null | undefined) {
  if (e.section) {
    const [i, n] = e.section
    const head = e.deck?.tag === 'AnomalyDeck' ? 8 : 0
    return head + ((i + 0.5) * (100 - head)) / n
  }
  const section = encounterSection(game, e, where)
  return section == null ? null : ((section * 2 + 1) / 6) * 100
}

// The card the players are looking at right now: an encounter, a headline
// being read, or an event the mythos turned up without asking anything.
const card = computed(() => {
  const e = g.value.encounter ?? null
  const cid = e ? e.card : g.value.activeCard
  if (cid == null) return null
  const src = ctx.activeCardImage(cid)
  const name = ctx.cardNameRaw(cid) ?? `Card ${cid}`
  const isEvent = !!ctx.eventImage(cid)
  if (e) {
    const where = g.value.investigators[e.investigator]?.space
    return {
      cid,
      src,
      name,
      isEvent,
      top: readArrowTop(g.value, e, where),
      title: ctx.invName(e.investigator),
      sub: `${where ? ctx.spaceName(where) : ''} · ${name}`,
    }
  }
  const kind = cid === g.value.revealedEvent ? 'Event' : 'Headline'
  return { cid, src, name, isEvent, top: null, title: kind, sub: name }
})
function open() {
  const c = card.value
  if (!c) return
  if (c.top == null) zoom(c.src)
  else zoomMarked(c.src, c.top)
}
</script>

<template>
  <section id="activeSection" class="active-side">
    <h2>Active card</h2>
    <div id="activeToken">
      <div v-if="token" :key="tokenKey" class="drawn-token">
        <MythosTok :token="token" :size="56" />
        <div>
          <div class="drawn-token-name">{{ tokenName }}</div>
          <div class="drawn-token-sub">drawn from the mythos cup</div>
        </div>
      </div>
    </div>
    <div id="activeCard">
      <div v-if="!card" class="active-empty">No card in play</div>
      <div v-else class="encounter">
        <figure
          class="codex-card encounter-card"
          :class="[{ 'no-art': isBroken(card.src) }, ctx.marks(['card', card.cid])]"
          :data-card="card.cid"
          style="view-transition-name: active-card"
          @click="open"
        >
          <span v-if="card.top != null" class="read-arrow" :style="{ top: `${card.top}%` }" title="Read this section"></span>
          <img
            v-if="!isBroken(card.src)"
            :src="card.src"
            :data-event="card.isEvent ? '' : undefined"
            :alt="card.name"
            @error="markBroken(card.src)"
          /><span class="asset-name">{{ card.name }}</span>
        </figure>
        <div>
          <div class="pa-name">{{ card.title }}</div>
          <div class="pa-where">{{ card.sub }}</div>
        </div>
      </div>
    </div>
  </section>
</template>
