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
        :title="`#${e.number} ${codexName(e)}${e.flipped ? ' · back' : ''}`"
        @click="zoom(codexSrc(e))"
      >
        <img v-if="!isBroken(codexSrc(e))" :src="codexSrc(e)" :alt="codexName(e)" @error="markBroken(codexSrc(e))" /><span
          class="asset-name"
          >#{{ e.number }} {{ codexName(e) }}</span
        ><span v-if="e.tokens?.clues" class="codex-tok"
          ><Tok name="clue" :count="e.tokens.clues" :title="`${e.tokens.clues} clues on this card`" :size="34" always
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
