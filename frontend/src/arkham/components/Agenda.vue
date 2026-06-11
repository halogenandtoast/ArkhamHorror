<script lang="ts" setup>
import { TokenType } from '@/arkham/types/Token';
import { ComputedRef, computed, ref } from 'vue';
import { useCardStore } from '@/stores/cards';
import { useDebug } from '@/arkham/debug';
import { useI18n } from 'vue-i18n';
import { imgsrc, groupBy } from '@/arkham/helpers';
import { type Game } from '@/arkham/types/Game';
import { type Card, cardImage, asCardCode } from '@/arkham/types/Card'
import { cardImage as cardCodeImage } from '@/arkham/cardImages'
import * as ArkhamGame from '@/arkham/types/Game';
import { AbilityLabel, AbilityMessage, type Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import AbilityButton from '@/arkham/components/AbilityButton.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import Event from '@/arkham/components/Event.vue';
import Enemy from '@/arkham/components/Enemy.vue';
import StackIndicator from '@/arkham/components/StackIndicator.vue';
import * as Arkham from '@/arkham/types/Agenda';

const props = defineProps<{
  agenda: Arkham.Agenda
  game: Game
  cardsUnder: Card[]
  cardsNextTo: Card[]
  remainingStack: Card[]
  completedStack: Card[]
  playerId: string
}>()

const emit = defineEmits<{
  show: [cards: ComputedRef<Card[]>, title: string, isDiscards: boolean]
  choose: [value: number]
}>()

const id = computed(() => props.agenda.id)
const debug = useDebug()
const cardStore = useCardStore()

const canViewUnder = computed(() => {
  if (debug.active) return true
  const { scenario } = props.game
  if (!scenario) return true
  if (scenario.id === 'c02195') return false
  return true
})

const { t } = useI18n()

const image = computed(() => {
  if (props.agenda.flipped) {
    // c03276a and c03279a flip to their own 'b' side; other agendas drop the
    // trailing 'a' before appending 'b'.
    if (["c03276a", "c03279a"].includes(id.value)) {
      return cardCodeImage(id.value, 'b')
    }
    return cardCodeImage(id.value.replace(/a$/, ''), 'b')
  }
  return cardCodeImage(id.value)
})

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const viewingUnder = ref(false)
const viewUnderLabel = computed(() => viewingUnder.value ? "Close" : t('cardsUnderneath', { count: props.cardsUnder.length }))

function canInteract(c: Message): boolean {
  return c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value
}

const interactAction = computed(() => choices.value.findIndex(canInteract));

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'AgendaSource') {
    if ("contents" in source) {
      return source.contents === id.value
    }
  }

  return false
}

const abilities = computed(() => {
  return choices.value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i }]
      }

      return acc
    }, [])
})

const cardsUnder = computed(() => props.cardsUnder)
const showCardsUnderAgenda = () => emit('show', cardsUnder, 'Cards Under Agenda', false)

const futureStack = computed(() => props.remainingStack.filter(c => asCardCode(c) !== props.agenda.id))

const cardStage = (card: Card): number | null => {
  const code = asCardCode(card)
  return cardStore.cards.find((cardDef) =>
    cardDef.cardCode === code || cardDef.cardCode === code.replace(/^c/, '')
  )?.stage ?? null
}

type StackIndicatorGroup = {
  label: string
  state: 'completed' | 'current' | 'remaining'
  images: {
    src: string
    current?: boolean
    passed?: boolean
  }[]
}

type AgendaStackGroup = StackIndicatorGroup & {
  stage: number | null
  firstIndex: number
}

const groupedAgendaStack = computed<StackIndicatorGroup[]>(() => {
  const groups: AgendaStackGroup[] = []

  const addToGroup = (
    stage: number | null,
    fallbackKey: string,
    cardImage: StackIndicatorGroup['images'][number],
    preferredState: StackIndicatorGroup['state'],
    firstIndex: number,
  ) => {
    const group = groups.find((g) => stage !== null ? g.stage === stage : g.label === fallbackKey)

    if (group) {
      group.images.push(cardImage)
      if (preferredState === 'current') group.state = 'current'
      return
    }

    groups.push({
      label: stage === null ? fallbackKey : `Agenda ${stage}`,
      stage,
      firstIndex,
      state: preferredState,
      images: [cardImage],
    })
  }

  props.completedStack.forEach((card, i) => {
    const stage = cardStage(card)
    addToGroup(stage, `Agenda ${i + 1}`, { src: imgsrc(cardImage(card)), passed: true }, 'completed', i)
  })

  addToGroup(
    props.agenda.sequence.step,
    `Agenda ${props.agenda.sequence.step}`,
    { src: image.value, current: true },
    'current',
    props.completedStack.length,
  )

  futureStack.value.forEach((card, i) => {
    const stage = cardStage(card)
    addToGroup(
      stage,
      `Agenda ${props.completedStack.length + i + 2}`,
      { src: imgsrc(cardImage(card)) },
      stage === props.agenda.sequence.step ? 'current' : 'remaining',
      props.completedStack.length + i + 1,
    )
  })

  return groups.sort((a, b) => {
    if (a.stage !== null && b.stage !== null) return a.stage - b.stage
    return a.firstIndex - b.firstIndex
  })
})

const totalAgendas = computed(() => groupedAgendaStack.value.length)
const currentAgendaPosition = computed(() => groupedAgendaStack.value.findIndex((group) => group.state === 'current') + 1 || props.agenda.sequence.step)

const nextToTreacheries = computed(() => Object.values(props.game.treacheries).
  filter((t) => t.placement.tag === "NextToAgenda").
  map((t) => t.id))

const nextToEvents = computed(() => Object.values(props.game.events).
  filter((t) => t.placement.tag === "NextToAgenda").
  map((t) => t.id))

const attachedEnemies = computed(() => Object.values(props.game.enemies).
  filter((t) => t.placement.tag === "AttachedToAgenda").
  map((t) => t.id))

const groupedTreacheries = computed(() => Object.entries(groupBy([...props.agenda.treacheries, ...nextToTreacheries.value], (t) => props.game.treacheries[t].cardCode)))

// Which treachery group is slid out. We reveal only when the pointer is over the
// card image, not over its buttons — so mousing straight onto a Forced button
// presses it in place instead of triggering the slide. Once revealed it stays
// out (so you can move onto the now-slid button) until the pointer leaves the
// whole group.
const revealedTreacheryGroup = ref<string | null>(null)
function revealTreacheryGroup(e: PointerEvent, cCode: string) {
  if (e.target instanceof Element && e.target.matches('img.card')) {
    revealedTreacheryGroup.value = cCode
  }
}
function hideTreacheryGroup(cCode: string) {
  if (revealedTreacheryGroup.value === cCode) revealedTreacheryGroup.value = null
}

const isVertical = computed(() => {
  const cardCode = props.agenda.flipped ? id.value.replace(/a?$/, 'b') : id.value
  return ["c01121b", "c03241b", "c06169b", "c50026b", "c07164b", "c07165b", "c07199b", "c82002b", "c90033b", "c90066b"].includes(cardCode) 
})

const eclipses = computed(() => props.agenda.tokens[TokenType.Eclipse])
const wards = computed(() => props.agenda.tokens[TokenType.Ward])
</script>

<template>
  <div class="agenda-container">
    <StackIndicator
      label="Agenda"
      :current="currentAgendaPosition"
      :total="totalAgendas"
      :completedCards="completedStack"
      :currentImage="image"
      :remainingCards="futureStack"
      :groups="groupedAgendaStack"
    />
    <div class="agenda-main">
      <div class="agenda-card">
        <img
        :class="{ 'agenda--can-progress': interactAction !== -1, 'card--sideways': !isVertical }"
          class="card card--agenda"
          @click="$emit('choose', interactAction)"
          :src="image"
        />
        <div class="pool" v-if="!agenda.flipped">
          <template v-if="debug.active">
            <button @click="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'RemoveTokens_', contents: [{'tag': 'GameSource'}, {'tag': 'AgendaTarget', 'contents': id}, 'Doom', 1]}})">-</button>
          </template>

          <PoolItem
            type="doom"
            :amount="agenda.doom"
          />
          <PoolItem class="eclipse" v-if="eclipses" type="resource" :amount="eclipses" />
          <PoolItem class="ward" v-if="wards" type="resource" :amount="wards" />

          <template v-if="debug.active">
            <button
              @click.exact="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'PlaceTokens_', contents: [{'tag': 'GameSource'}, {'tag': 'AgendaTarget', 'contents': id}, 'Doom', 1]}})"
              @click.shift="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'PlaceTokens_', contents: [{'tag': 'GameSource'}, {'tag': 'AgendaTarget', 'contents': id}, 'Doom', 5]}})"
            >+</button>
          </template>
        </div>
      </div>
      <img
        v-for="(card, idx) in cardsNextTo"
        class="card card--sideways"
        :key="idx"
        :src="imgsrc(cardImage(card))"
      />
      <AbilityButton
        v-for="ability in abilities"
        :key="ability.index"
        :ability="ability.contents"
        :data-image="image"
        :game="game"
        class="sideways"
        @click="$emit('choose', ability.index)"
        />
      <Enemy
        v-for="enemyId in attachedEnemies"
        :enemy="game.enemies[enemyId]"
        :game="game"
        :playerId="playerId"
        @choose="$emit('choose', $event)"
      />
      <Event
        v-for="eventId in nextToEvents"
        :event="game.events[eventId]"
        :game="game"
        :playerId="playerId"
        @choose="$emit('choose', $event)"
      />
      <div v-if="groupedTreacheries.length > 0" class="treacheries">
        <div
          v-for="([cCode, treacheries], idx) in groupedTreacheries"
          :key="cCode"
          class="treachery-group"
          :class="{ 'is-revealed': revealedTreacheryGroup === cCode }"
          :style="{ zIndex: (groupedTreacheries.length - idx) * 10 }"
          @pointerover="revealTreacheryGroup($event, cCode)"
          @pointerleave="hideTreacheryGroup(cCode)"
        >
          <div class="treachery-group__cards">
            <div
              v-for="(treacheryId, cardIdx) in treacheries"
              class="treachery-card"
              :key="treacheryId"
              :style="{ '--attachment-index': cardIdx }"
            >
              <Treachery
                :treachery="game.treacheries[treacheryId]"
                :game="game"
                :playerId="playerId"
                @choose="$emit('choose', $event)"
                :overlay-delay="310"
              />
            </div>
          </div>
        </div>
      </div>

      <button v-if="cardsUnder.length > 0 && canViewUnder" class="view-cards-under-button" @click="showCardsUnderAgenda">{{viewUnderLabel}}</button>
      <button v-else-if="cardsUnder.length > 0" class="view-cards-under-button" disabled>{{viewUnderLabel}}</button>
    </div>
  </div>
</template>

<style scoped>
.card {
  width: var(--card-width);
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  border-radius: 6px;
}

.card--sideways {
  width: auto;
  height: var(--card-width);
  max-width: max-content;
}

.card--agenda {
  z-index: 100;
}

.agenda-container {
  display: flex;
  flex-direction: row;
  align-items: flex-start;
  gap: 6px;
}

/* The card and everything attached to it share one column, so attachments line
   up under the card on their own — the stack-indicator pips sit beside that
   column and never need a margin to clear. */
.agenda-main {
  display: flex;
  flex-direction: column;
}

/* Keep the pips centered against the card (the first row of .agenda-main),
   not the full attachment stack below it. The sideways card is var(--card-width)
   tall, so matching that height centers the pips on the card. */
.agenda-container > :deep(.v-popper) {
  align-self: flex-start;
  display: flex;
  align-items: center;
  height: var(--card-width);
}

.agenda--can-progress {
  border: 3px solid #ff00ff;
  border-radius: 8px;
  cursor: pointer;
}

.pool {
  display: flex;
  flex-direction: row;
  height: 2em;
  justify-content: flex-start;
  button {
    height: min-content;
    align-self: center;
  }
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.treacheries {
  position:relative;
  display: flex;
  flex-direction: column;
  gap: 5px;
}

/* The group is the stationary hover target: its layout box stays put while only
   the inner .treachery-group__cards translate. If the group itself moved, it
   would slide out from under the pointer, drop :hover, snap back, and flicker —
   making attachments (and their Forced buttons) hard to click.

   The slide reveals the card downward into the board's region. The agenda lives
   in .scenario-cards (z-index: -2), so on its own the slid card would render
   behind the board and be unclickable — Scenario.vue lifts .scenario-cards while
   a treachery is hovered (:has) so the revealed card stays on top. */
.treachery-group {
  margin-top: -50px;
  position: relative;
}

.treachery-group__cards {
  display: flex;
  gap: 5px;
  flex-direction: row;
  transition: transform 0.3s;
  will-change: transform;
}

.treachery-group.is-revealed .treachery-group__cards,
.treachery-group.is-revealed ~ .treachery-group .treachery-group__cards {
  transform: translateY(50px);
}

.treachery-group.is-revealed .treachery-card {
  transform: translateX(calc(var(--attachment-index, 0) * 50px));
}

.treachery {
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}

.treachery-card {
  margin-left: -50px;
  transition: transform 0.3s;
  will-change: transform;

  &:first-of-type {
    margin-left: 0;
  }
}

.agenda-card {
  > img {
    box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.8);
  }
  z-index: 100;
  position: relative;
  .pool {
    z-index: 101;
    position: absolute;
    left: 0;
    top: 0;
  }
}

.agenda-container :deep(.card--sideways) {
  width: auto;
  height: var(--card-width);
  aspect-ratio: var(--card-sideways-aspect);
}

</style>
