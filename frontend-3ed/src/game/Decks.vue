<script setup lang="ts">
import { computed } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import Deck from '@/game/Deck.vue'
import { zoom } from '@/game/overlays'
import Tok from '@/game/Tok.vue'
import { ANOMALY_BACKS } from '@/game/util'
import type { CardId } from '@/types'

const ctx = useGame()
const g = computed(() => ctx.game.value!)
// debug: hand a card from the display straight to whoever debug is acting as
const gainFromDisplay = (cid: CardId) => void ctx.debugAction('DebugGainFromDisplay', [ctx.dbgIid(), cid])
const d = computed(() => g.value.decks)

const hoods = computed(() => (g.value.board.layout?.tiles ?? []).map((t) => t.neighborhood))
const hasStreets = computed(() => Object.values(g.value.board.spaces).some((s) => s.kind?.tag === 'StreetSpace'))
const anomalySet = computed(() => ctx.catalog.scenarios.find((sc) => sc.code === g.value.scenario)?.anomalySet ?? null)

// we always draw from the bottom, so the top card's back gives nothing away
const topBack = (cid: CardId | undefined) => (cid == null ? null : img(`cards/${ctx.cardCode(cid)}b.webp`))
const topFace = (cid: CardId | undefined) => (cid == null ? null : img(`cards/${ctx.cardCode(cid)}.webp`))
// event cards share their neighborhood's back, so the top of the event deck shows where it points
const eventBack = (cid: CardId | undefined) => {
  const nid = cid == null ? null : ctx.view.value?.eventNeighborhoods?.[cid]
  return cid == null || !nid ? null : img(`backs/${nid}.webp`)
}
const faceSrc = (cid: CardId) => ctx.cardFace(cid, false)
</script>

<template>
  <div id="decks" class="decks">
    <div class="deck-group lead">
      <Deck back="headline" name="Headlines" :count="d.headline.length" deck-key="headline" />
      <Deck
        back="headline"
        name="Headline discard"
        :count="d.headlineDiscard.length"
        :src="topFace(d.headlineDiscard[0])"
        deck-key="headlineDiscard"
      />
      <Deck back="monster" name="Monsters" :count="d.monster.length" :src="topBack(d.monster[0])" deck-key="monster" />
      <Deck back="event" name="Events" :count="d.event.length" :src="eventBack(d.event[0])" deck-key="event" />
      <Deck
        back="event"
        name="Event discard"
        :count="d.eventDiscard.length"
        :src="ctx.eventImage(d.eventDiscard[0])"
        deck-key="eventDiscard"
      />
    </div>
    <Deck
      v-for="nid in hoods"
      :key="nid"
      :back="nid"
      :name="g.board.neighborhoods[nid]?.name ?? nid"
      :count="(d.neighborhoods[nid] ?? []).length"
      :deck-key="`neighborhood:${nid}`"
    >
      <span v-if="g.board.neighborhoods[nid]?.clues" class="deck-clue"
        ><Tok
          name="clue"
          :count="g.board.neighborhoods[nid].clues"
          :title="`${g.board.neighborhoods[nid].clues} clue${g.board.neighborhoods[nid].clues > 1 ? 's' : ''}: an event card is among the top cards`"
          :size="34"
      /></span>
    </Deck>
    <Deck v-if="hasStreets" back="the-street" name="Streets" :count="d.street.length" deck-key="street" />
    <Deck
      v-if="d.anomaly.length || anomalySet"
      :back="ANOMALY_BACKS[anomalySet ?? ''] ?? 'temporal-fissures'"
      :name="anomalySet ?? 'Anomalies'"
      :count="d.anomaly.length"
      deck-key="anomaly"
    />
    <Deck back="ally" name="Allies" :count="d.ally.length" deck-key="ally" />
    <Deck back="spell" name="Spells" :count="d.spell.length" deck-key="spell" />
    <Deck back="item" name="Items" :count="d.item.length" deck-key="item" />
    <div v-if="d.display.length" class="display-group">
      <div
        v-for="cid in d.display"
        :key="cid"
        class="deck zoomable"
        :class="ctx.marks(['card', cid])"
        :data-card="cid"
        :title="`${ctx.cardName(cid)} — click to enlarge`"
        @click="zoom(faceSrc(cid))"
      >
        <div class="deck-stack face" :class="{ 'no-art': isBroken(faceSrc(cid)) }">
          <img v-if="!isBroken(faceSrc(cid))" :src="faceSrc(cid)" :alt="ctx.cardName(cid)" @error="markBroken(faceSrc(cid))" />
          <div class="deck-empty">{{ ctx.cardName(cid) }}</div>
          <button
            v-if="ctx.dbgOn.value && ctx.dbgIid()"
            class="dbg-take"
            :title="`Gain ${ctx.cardName(cid)} for ${ctx.invName(ctx.dbgIid())}`"
            @click.stop="gainFromDisplay(cid)"
          >
            Take
          </button>
        </div>
        <div class="deck-name">{{ ctx.cardName(cid) }}</div>
      </div>
    </div>
  </div>
</template>
