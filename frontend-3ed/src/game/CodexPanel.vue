<script setup lang="ts">
import { computed } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import MythosTok from '@/game/MythosTok.vue'
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
  return { ...r, name: ctx.cardNameRaw(r.card) ?? 'Rumor', src: img(`cards/${ctx.cardCode(r.card)}.webp`) }
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
        :title="`${codexName(e)}${e.flipped ? ' (back)' : ''}`"
        @click="zoom(codexSrc(e))"
      >
        <img v-if="!isBroken(codexSrc(e))" :src="codexSrc(e)" :alt="codexName(e)" @error="markBroken(codexSrc(e))" /><span
          class="asset-name"
          >#{{ e.number }} {{ codexName(e) }}</span
        ><span v-if="e.tokens?.clues" class="codex-tok"
          ><Tok name="clue" :count="e.tokens.clues" :title="`${e.tokens.clues} clues on this card`" :size="34" always
        /></span>
        <figcaption>#{{ e.number }}{{ e.flipped ? ' · back' : '' }}</figcaption>
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
        <figcaption>Rumor</figcaption>
      </figure>
      <em v-if="!g.codex.length && !rumor" class="waiting">Empty</em>
    </div>
  </section>
  <section style="margin-top: 12px">
    <h2>Mythos</h2>
    <div id="codex">
      <div class="cup">
        <span>Mythos cup ({{ g.cup.length }})</span><MythosTok v-for="(t, k) in g.cup" :key="k" :token="t" :size="32" />
      </div>
      <div class="cup">
        <span>Drawn</span><MythosTok v-for="(t, k) in g.drawnTokens" :key="k" :token="t" :size="32" /><template
          v-if="!g.drawnTokens.length"
          >—</template
        >
      </div>
    </div>
  </section>
</template>
