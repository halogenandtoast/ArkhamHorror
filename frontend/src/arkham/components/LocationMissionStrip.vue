<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Scenario } from '@/arkham/types/Scenario'
import { cardImage } from '@/arkham/cardImages'
import { useCardInfo } from '@/arkham/composables/useCardNames'
import { useGameChoices } from '@/arkham/composables/useGameChoices'
import { choicesFor, type EntityChoice } from '@/arkham/locationChoices'
import AbilityButton from '@/arkham/components/AbilityButton.vue'

/* The current act and agenda as two banner panels above the map, the way the offline demo shows its
 * mission strip: a small card thumbnail, "ACT 1 / 3", the title, flavour and objective, and for the
 * agenda a doom meter. Text comes from the card database; the abilities are the server's own choices. */
const props = defineProps<{ game: Game; scenario: Scenario; playerId: string }>()
const emit = defineEmits<{ (e: 'choose', index: number): void }>()

const { t } = useI18n()
const cardInfo = useCardInfo()
const choices = useGameChoices(() => props.game, () => props.playerId)

// Printed text carries markup (bold tags, [[Trait]], [action]); the banner shows it plain.
const plain = (text: string | undefined) => (text ?? '')
  .replace(/<br\s*\/?>/gi, ' ').replace(/<[^>]+>/g, '').replace(/\[\[([^\]]+)\]\]/g, '$1').replace(/\[[a-z]+\]/gi, '').trim()

type Panel = {
  key: string; kind: 'act' | 'agenda'; image: string; step: number; total: number
  title: string; flavor: string; text: string; doom: number | null; doomMax: number | null
  choices: EntityChoice[]
}

function describe(kind: 'act' | 'agenda', id: string, deckId: number, step: number, flipped: boolean, doom: number | null): Panel {
  const card = cardInfo(id)
  const stack = kind === 'act'
    ? [props.scenario.actStack[deckId], props.scenario.completedActStack[deckId]]
    : [props.scenario.agendaStack[deckId], props.scenario.completedAgendaStack[deckId]]
  const total = Math.max(step, step + (stack[0]?.length ?? 0) + (stack[1]?.length ?? 0))
  return {
    key: `${kind}${deckId}`, kind, image: cardImage(id, flipped ? 'b' : ''), step, total,
    title: (flipped ? card?.back_name : card?.name) ?? card?.name ?? id,
    flavor: plain(flipped ? card?.back_flavor : card?.flavor),
    text: plain(flipped ? card?.back_text : card?.text),
    doom, doomMax: kind === 'agenda' ? card?.doom ?? null : null,
    choices: choicesFor(choices.value, id),
  }
}

const panels = computed<Panel[]>(() => [
  ...Object.values(props.game.acts).sort((a, b) => a.deckId - b.deckId)
    .map((a) => describe('act', a.id, a.deckId, a.sequence.number, false, null)),
  ...Object.values(props.game.agendas).sort((a, b) => a.deckId - b.deckId)
    .map((a) => describe('agenda', a.id, a.deckId, a.sequence.step, a.flipped, a.doom)),
])
</script>

<template>
  <div v-if="panels.length" class="mission-strip">
    <article v-for="p in panels" :key="p.key" class="mission" :class="p.kind">
      <img class="thumb" :src="p.image" alt="" loading="lazy" />
      <div class="body">
        <span class="eyebrow">{{ t(`locationMap.${p.kind}`) }} {{ p.step }} / {{ p.total }}</span>
        <h3>{{ p.title }}</h3>
        <p v-if="p.flavor" class="flavor" :title="p.flavor">{{ p.flavor }}</p>
        <p v-if="p.text" class="text" :title="p.text">{{ p.text }}</p>
        <div v-if="p.doom !== null" class="doom">
          <span>{{ t('locationMap.doomCount', { count: p.doom, max: p.doomMax ?? '?' }) }}</span>
          <i v-if="p.doomMax"><b :style="{ width: Math.min(100, (100 * p.doom) / p.doomMax) + '%' }"></b></i>
        </div>
        <div v-if="p.kind === 'agenda'" class="total doom-total" :title="t('locationMap.totalDoom')">☠ {{ t('locationMap.totalDoom') }} <b>{{ game.totalDoom }}</b></div>
        <div v-else class="total clue-total" :title="t('locationMap.totalClues')">◇ {{ t('locationMap.totalClues') }} <b>{{ game.totalClues }}</b></div>
        <AbilityButton v-for="c in p.choices" :key="c.index" :game="game" :ability="c.contents" @choose="emit('choose', c.index)" />
      </div>
    </article>
  </div>
</template>

<style scoped>
.mission-strip { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 10px; padding: 8px 10px; background: #101a14; border-bottom: 1px solid #a68b5540; }
.mission { display: flex; gap: 10px; padding: 8px 10px; background: #16221b; border: 1px solid #c6a87366; border-radius: 6px; color: #e5e2d4; min-width: 0; }
.mission.agenda { border-color: #c9887a88; }
.thumb { flex: none; width: 58px; height: 80px; object-fit: cover; object-position: top; border-radius: 4px; border: 1px solid #0006; }
.body { display: flex; flex-direction: column; gap: 3px; min-width: 0; }
.eyebrow { text-transform: uppercase; letter-spacing: 0.18em; font-size: 0.6rem; color: #c6a873; }
.agenda .eyebrow { color: #d99e8e; }
h3 { margin: 0; font: 1.15rem Georgia, serif; }
p { margin: 0; font-size: 0.75rem; line-height: 1.35; }
.flavor { color: #b9c2ad; font-style: italic; display: -webkit-box; -webkit-line-clamp: 1; -webkit-box-orient: vertical; overflow: hidden; }
.text { color: #e9e3cc; display: -webkit-box; -webkit-line-clamp: 2; -webkit-box-orient: vertical; overflow: hidden; }
.doom { display: flex; flex-direction: column; gap: 2px; font-size: 0.7rem; color: #f0d9a8; }
.doom i { display: block; height: 4px; background: #5a4a40; border-radius: 3px; overflow: hidden; }
.total { display: inline-flex; align-items: center; gap: 6px; align-self: flex-start; margin-top: 2px; padding: 2px 8px; border-radius: 999px; font-size: 0.7rem; background: #0006; }
.total b { font-size: 0.9rem; color: #fff4df; }
.doom-total { color: #e8a08d; border: 1px solid #c9684f88; }
.clue-total { color: #9fd3e6; border: 1px solid #5fa8c488; }
.doom b { display: block; height: 100%; background: #c9684f; }
</style>
