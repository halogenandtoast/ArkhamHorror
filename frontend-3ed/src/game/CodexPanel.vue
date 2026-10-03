<script setup lang="ts">
import { computed } from 'vue'
import { cardImg, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import { zoom } from '@/game/overlays'
import Tok from '@/game/Tok.vue'
import { archiveImage } from '@/game/util'
import type { CodexEntry } from '@/types'

const ctx = useGame()
const g = computed(() => ctx.game.value!)

const codexName = (e: CodexEntry) => ctx.cardNameRaw(e.card) || `Card ${e.number}`
const codexSrc = (e: CodexEntry) => archiveImage(e.number, e.flipped)
/* Some scenarios deal a face-down investigation deck that the codex draws from -- Dreams
of R'lyeh rules a line of enquiry out at a time, Tyrants of Ruin turns up relics. The pile
lies under one codex card and the cards are not the player's to look at, so that card is
simply drawn as a stack, with the number underneath on it. */
const buried = computed(() => g.value.decks?.investigation?.length ?? 0)
const buriedUnder = computed(() => g.value.decks?.investigationUnder ?? null)
const under = (e: CodexEntry) => (e.number === buriedUnder.value ? buried.value : 0)
// each card below the top one shows as another edge behind it
const stack = (n: number) =>
  Array.from(
    { length: Math.min(4, n) },
    (_, k) => `${(k + 1) * 3}px ${(k + 1) * 3}px 0 -1px #111, ${(k + 1) * 3}px ${(k + 1) * 3}px 0 0 #555`,
  ).join(', ')
const rumor = computed(() => {
  const r = g.value.rumor
  if (!r) return null
  return { ...r, name: ctx.cardNameRaw(r.card) ?? 'Rumor', src: cardImg(ctx.cardCode(r.card)) }
})
</script>

<template>
  <section>
    <h2>Codex</h2>
    <div id="codexCards" class="codex">
      <figure
        v-for="e in g.codex"
        :key="e.number"
        class="codex-card"
        :class="[{ 'no-art': isBroken(codexSrc(e)) }, ctx.marks(['codex', e.number])]"
        :data-codex="e.number"
        :title="`#${e.number} ${codexName(e)}${e.flipped ? ' · back' : ''}${under(e) ? ` · ${under(e)} face down under it` : ''}`"
        @click="zoom(codexSrc(e))"
      >
        <img
          v-if="!isBroken(codexSrc(e))"
          :src="codexSrc(e)"
          :alt="codexName(e)"
          :style="under(e) ? { boxShadow: stack(under(e)) } : undefined"
          @error="markBroken(codexSrc(e))"
        /><b v-if="under(e)" class="buried-count">{{ under(e) }}</b><span
          class="asset-name"
          >#{{ e.number }} {{ codexName(e) }}</span
        ><span v-if="e.tokens?.clues" class="codex-tok"
          ><Tok name="clue" :count="e.tokens.clues" :title="`${e.tokens.clues} clues on this card`" :size="34" always
        /></span
        ><span v-if="e.tokens?.doom" class="codex-tok doom"
          ><Tok name="doom" :count="e.tokens.doom" :title="`${e.tokens.doom} doom on this card`" :size="34" always
        /></span>
      </figure>
      <figure
        v-if="rumor"
        class="codex-card"
        :class="[{ 'no-art': isBroken(rumor.src) }, ctx.marks(['card', rumor.card])]"
        :data-card="rumor.card"
        :title="rumor.name"
        @click="zoom(rumor.src)"
      >
        <img v-if="!isBroken(rumor.src)" :src="rumor.src" :alt="rumor.name" @error="markBroken(rumor.src)" /><span
          class="asset-name"
          >{{ rumor.name }}</span
        ><span v-if="rumor.doom" class="codex-tok"
          ><Tok name="doom" :count="rumor.doom" :title="`${rumor.doom} doom on this card`" :size="34" always
        /></span>
      </figure>
      <em v-if="!g.codex.length && !rumor" class="waiting">Empty</em>
    </div>
  </section>
</template>
