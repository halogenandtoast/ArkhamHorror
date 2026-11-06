<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Location } from '@/arkham/types/Location'
import { TokenType } from '@/arkham/types/Token'
import { cardArt, cardImage } from '@/arkham/cardImages'
import { useGameChoices } from '@/arkham/composables/useGameChoices'
import { useDbCardStore } from '@/stores/dbCards'
import { choicesFor, selectLocationIndex, type EntityChoice } from '@/arkham/locationChoices'
import AbilityButton from '@/arkham/components/AbilityButton.vue'

const props = defineProps<{
  game: Game
  playerId: string
  location: Location | null
  /** Route from the investigator to this location, so a faraway location can still be walked to. */
  route: string[]
}>()
const emit = defineEmits<{ (e: 'choose', index: number): void; (e: 'select', id: string): void }>()

const { t } = useI18n()
const dbCards = useDbCardStore()
const choices = useGameChoices(() => props.game, () => props.playerId)

const nameOf = (code: string) => dbCards.getDbCard(cardArt(code))?.name ?? code

type Entry = { id: string; name: string; note: string | null; choices: EntityChoice[] }

function entry(id: string, code: string, note: string | null = null): Entry {
  return { id, name: nameOf(code), note, choices: choicesFor(choices.value, id) }
}

const here = computed(() => props.location)
const selectIndex = computed(() => here.value ? selectLocationIndex(choices.value, here.value) : -1)
const ownChoices = computed(() => here.value ? choicesFor(choices.value, here.value.id) : [])

const enemies = computed<Entry[]>(() => (here.value?.enemies ?? []).flatMap((id) => {
  const enemy = props.game.enemies[id]
  if (!enemy || enemy.placement.tag !== 'AtLocation') return []
  const damage = enemy.tokens[TokenType.Damage] ?? 0
  const notes = [enemy.exhausted ? t('locationMap.exhausted') : null, damage ? t('locationMap.damage', { count: damage }) : null]
  return [entry(id, enemy.cardCode, notes.filter(Boolean).join(' · ') || null)]
}))

const investigators = computed<Entry[]>(() => (here.value?.investigators ?? []).flatMap((id) => {
  const investigator = props.game.investigators[id]
  return investigator ? [entry(id, investigator.cardCode, id === props.playerId ? t('locationMap.you') : null)] : []
}))

const cards = computed<Entry[]>(() => {
  const l = here.value
  if (!l) return []
  return [
    ...l.assets.flatMap((id) => props.game.assets[id] ? [entry(id, props.game.assets[id].cardCode)] : []),
    ...l.treacheries.flatMap((id) => props.game.treacheries[id] ? [entry(id, props.game.treacheries[id].cardCode)] : []),
    ...l.events.flatMap((id) => props.game.events[id] ? [entry(id, props.game.events[id].cardCode)] : []),
  ]
})

const tokenLines = computed(() => Object.entries(here.value?.tokens ?? {})
  .filter(([, n]) => (n ?? 0) > 0)
  .map(([token, n]) => `${token} ${n}`))

const image = computed(() => here.value ? cardImage(here.value.cardCode, here.value.revealed ? '' : 'b') : '')
const nextStep = computed(() => props.route.length > 2 ? props.route[1] : null)
</script>

<template>
  <aside class="map-panel" :aria-label="t('locationMap.panel')">
    <p v-if="!here" class="empty">{{ t('locationMap.nothingSelected') }}</p>
    <template v-else>
      <header>
        <h3>{{ nameOf(here.cardCode) }}</h3>
        <span class="stats">
          <template v-if="here.shroud !== null">{{ t('locationMap.shroud', { count: here.shroud }) }}</template>
          <template v-if="tokenLines.length"> · {{ tokenLines.join(' · ') }}</template>
        </span>
      </header>
      <img class="card-preview" :src="image" :alt="nameOf(here.cardCode)" loading="lazy" />

      <section class="group">
        <h4>{{ t('locationMap.location') }}</h4>
        <button v-if="selectIndex !== -1" class="button" data-game-actionable="true" @click="emit('choose', selectIndex)">
          {{ t('locationMap.chooseHere') }}
        </button>
        <AbilityButton v-for="c in ownChoices" :key="c.index" :game="game" :ability="c.contents" @choose="emit('choose', c.index)" />
        <button v-if="nextStep" class="button route" @click="emit('select', nextStep)">
          {{ t('locationMap.route', { count: route.length - 1 }) }}
        </button>
        <p v-if="selectIndex === -1 && ownChoices.length === 0 && !nextStep" class="muted">{{ t('locationMap.noActions') }}</p>
      </section>

      <section v-if="enemies.length" class="group enemy">
        <h4>{{ t('locationMap.enemies') }}</h4>
        <div v-for="e in enemies" :key="e.id" class="entity">
          <span class="name">{{ e.name }}<small v-if="e.note"> · {{ e.note }}</small></span>
          <AbilityButton v-for="c in e.choices" :key="c.index" :game="game" :ability="c.contents" @choose="emit('choose', c.index)" />
        </div>
      </section>

      <section v-if="cards.length" class="group">
        <h4>{{ t('locationMap.cardsHere') }}</h4>
        <div v-for="c in cards" :key="c.id" class="entity">
          <span class="name">{{ c.name }}</span>
          <AbilityButton v-for="a in c.choices" :key="a.index" :game="game" :ability="a.contents" @choose="emit('choose', a.index)" />
        </div>
      </section>

      <section v-if="investigators.length" class="group">
        <h4>{{ t('locationMap.investigators') }}</h4>
        <div v-for="i in investigators" :key="i.id" class="entity">
          <span class="name">{{ i.name }}<small v-if="i.note"> · {{ i.note }}</small></span>
          <AbilityButton v-for="c in i.choices" :key="c.index" :game="game" :ability="c.contents" @choose="emit('choose', c.index)" />
        </div>
      </section>
    </template>
  </aside>
</template>

<style scoped>
.map-panel { display: flex; flex-direction: column; gap: 10px; padding: 12px; background: rgb(14 20 17 / 92%); border-left: 1px solid rgb(255 255 255 / 12%); overflow: auto; min-width: 0; color: #e5e2d4; }
header h3 { margin: 0; font-size: 1.3rem; }
.stats { font-size: 0.8rem; color: #b9c2ad; }
.card-preview { width: 100%; max-height: 190px; object-fit: contain; border-radius: 6px; }
.group { display: flex; flex-direction: column; gap: 6px; border-top: 1px solid rgb(255 255 255 / 10%); padding-top: 8px; }
.group h4 { margin: 0; font-size: 0.7rem; letter-spacing: 0.12em; text-transform: uppercase; color: #a8b79b; }
.group.enemy h4, .group.enemy .name { color: #e0a595; }
.entity { display: flex; flex-direction: column; gap: 4px; }
.name { font-size: 0.9rem; }
small, .muted, .empty { font-size: 0.8rem; color: #98a38f; }
.route { background: rgb(180 140 70 / 25%); }
</style>
