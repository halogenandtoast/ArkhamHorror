<script lang="ts" setup>
import { ComputedRef, computed, ref } from 'vue'
import { useCardStore } from '@/stores/cards'
import { type Game } from '@/arkham/types/Game'
import { type Card, cardImage, asCardCode, cardFacedown } from '@/arkham/types/Card'
import AbilitiesMenu from '@/arkham/components/AbilitiesMenu.vue'
import { useDebug } from '@/arkham/debug';
import PoolItem from '@/arkham/components/PoolItem.vue';
import KeyToken from '@/arkham/components/Key.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import ScarletKey from '@/arkham/components/ScarletKey.vue';
import StackIndicator from '@/arkham/components/StackIndicator.vue';
import * as ArkhamGame from '@/arkham/types/Game'
import { AbilityLabel, AbilityMessage, type Message } from '@/arkham/types/Message'
import { MessageType } from '@/arkham/types/Message'
import { keyToId } from '@/arkham/types/Key'
import { imgsrc } from '@/arkham/helpers'
import * as Arkham from '@/arkham/types/Act'
import { useEventStore } from '@/arkham/stores/event'
import { actContribution, actSpend } from '@/arkham/types/EpicEvent'

const props = defineProps<{
  act: Arkham.Act
  game: Game
  cardsUnder: Card[]
  cardsNextTo: Card[]
  remainingStack: Card[]
  completedStack: Card[]
  playerId: string
}>()

const emits = defineEmits<{
  show: [cards: ComputedRef<Card[]>, title: string, isDiscards: boolean]
  choose: [value: number]
}>()

const showAbilities = ref(false)
const frame = ref(null)
const debug = useDebug()
const cardStore = useCardStore()

const id = computed(() => props.act.id)

const keys = computed(() => props.act.keys)

const cardCode = computed(() => {
  const side = props.act.sequence.side.toLowerCase().replace('a', '')
  const sidePart = id.value.endsWith(side) ? "" : side

  let newId = side == 'b' && id.value === 'c10607a' ? '10607' : id.value.replace(/^c/, '')

  if (sidePart == 'd') {
    newId = newId.replace(/c$/, '')
  }

  if (sidePart == 'f') {
    newId = newId.replace(/e$/, '')
  }

  if (sidePart == 'h') {
    newId = newId.replace(/g$/, '')
  }

  // handle threads of fate as hardcoded values because I don't want to deal with it
  if (parseInt(newId) >= 4117 && parseInt(newId) <= 4140) {
    const adjustedSidePart = sidePart.replace(/[ace]/, '').replace(/[df]/, 'b')
    return `${newId}${adjustedSidePart}`
  }

  if (parseInt(newId) >= 53029 && parseInt(newId) <= 53036) {
    const adjustedSidePart = sidePart.replace(/[g]/, '').replace(/[h]/, 'b')
    return `${newId}${adjustedSidePart}`
  }

  return `${newId}${sidePart}`
})

const image = computed(() => {
  return imgsrc(`cards/${cardCode.value}.avif`)
})

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const viewingUnder = ref(false)
const viewUnderLabel = computed(() => viewingUnder.value ? "Close" : `${props.cardsUnder.length} Cards Underneath`)

function imageForCard(card: Card) {
  return imgsrc(cardImage(card))
}

function isCardAction(c: Message): boolean {
  return c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value
}

const interactAction = computed(() => choices.value.findIndex(isCardAction));

const canInteract = computed(() => abilities.value.length > 0 || interactAction.value !== -1)

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'ActSource') {
    return source.contents === id.value
  }

  return false
}

const abilities = computed(() => {
  return choices.value
    .reduce<AbilityMessage[]>((acc, v, i) =>
      isAbility(v) ? [...acc, { contents: v, displayAsAction: false, index: i}] : acc
    , [])
})

const hasObjective = computed(() =>
  abilities.value.some(({ contents }) =>
    "ability" in contents && contents.ability.type.tag === 'Objective'
  )
)

function revealFacedownCard(card: Card): Card {
  switch (card.tag) {
    case 'PlayerCard':
    case 'EncounterCard':
      return { ...card, contents: { ...card.contents, facedown: false } }
    case 'VengeanceCard': {
      const contents = card.contents
      return { ...card, contents: { ...contents, contents: { ...contents.contents, facedown: false } } }
    }
  }
}

const cardsUnder = computed(() => props.cardsUnder)
const visibleCardsUnder = computed(() => {
  if (debug.active) return props.cardsUnder.map(revealFacedownCard)
  return props.cardsUnder.filter((card) => !cardFacedown(card))
})
const canViewUnder = computed(() => visibleCardsUnder.value.length > 0)

const showCardsUnderAct = () => emits('show', visibleCardsUnder, 'Cards Under Act', false)

const futureStack = computed(() => props.remainingStack.filter(c => asCardCode(c) !== props.act.id))

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

type ActStackGroup = StackIndicatorGroup & {
  stage: number | null
  firstIndex: number
}

const groupedActStack = computed<StackIndicatorGroup[]>(() => {
  const groups: ActStackGroup[] = []

  const addToGroup = (
    stage: number | null,
    fallbackKey: string,
    image: StackIndicatorGroup['images'][number],
    preferredState: StackIndicatorGroup['state'],
    firstIndex: number,
  ) => {
    const group = groups.find((g) => stage !== null ? g.stage === stage : g.label === fallbackKey)

    if (group) {
      group.images.push(image)
      if (preferredState === 'current') group.state = 'current'
      return
    }

    groups.push({
      label: stage === null ? fallbackKey : `Act ${stage}`,
      stage,
      firstIndex,
      state: preferredState,
      images: [image],
    })
  }

  props.completedStack.forEach((card, i) => {
    const stage = cardStage(card)
    addToGroup(stage, `Act ${i + 1}`, { src: imgsrc(cardImage(card)), passed: true }, 'completed', i)
  })

  addToGroup(
    props.act.sequence.number,
    `Act ${props.act.sequence.number}`,
    { src: image.value, current: true },
    'current',
    props.completedStack.length,
  )

  futureStack.value.forEach((card, i) => {
    const stage = cardStage(card)
    addToGroup(
      stage,
      `Act ${props.completedStack.length + i + 2}`,
      { src: imgsrc(cardImage(card)) },
      stage === props.act.sequence.number ? 'current' : 'remaining',
      props.completedStack.length + i + 1,
    )
  })

  return groups.sort((a, b) => {
    if (a.stage !== null && b.stage !== null) return a.stage - b.stage
    return a.firstIndex - b.firstIndex
  })
})

const totalActs = computed(() => groupedActStack.value.length)
const currentActPosition = computed(() => groupedActStack.value.findIndex((group) => group.state === 'current') + 1 || props.act.sequence.number)

async function clicked() {
  if (interactAction.value !== -1) {
    emits('choose', interactAction.value)
  } else {
    if (abilities.value.length > 0) {
      showAbilities.value = !showAbilities.value
    } else {
      showAbilities.value = false
    }
  }
}

async function chooseAbility(index: number) {
  showAbilities.value = false
  emits('choose', index)
}

const isVertical = computed(() => {
  return ["03321b", "04117b", "04118b", "04122b", "04125b", "04126b", "04128b", "04130b", "04133b", "04134b", "04137b", "04209b", "05055b", "05288ab", "05286ab", "05286b", "05288b", "09615b", "10607b", "53029b", "53030b", "53032b", "53034b", "53046b", "82006b"].includes(cardCode.value)
})

const breaches = computed(() => {
  const {breaches} = props.act
  return breaches ?? 0
})

const clues = computed(() => props.act.tokens.Clue ?? 0)
const resources = computed(() => props.act.tokens.Resource ?? 0)

// Epic Multiplayer: shared-clue acts (The Blob's acts 1 & 3) hold ZERO real clue
// tokens — clues are spent into the global pool — so the act looks empty. Render a
// PSEUDO clue-token pool equal to THIS group's clues still on the act for the current
// stage = its contribution minus what the organizer allocated it to spend
// (`act-contribution:<stage>:<ordinal>` − `act-spend:<stage>:<ordinal>`). So the spent
// clues drop off this group's act the moment the organizer allocates (act-spend is
// mirrored before the gate lifts), and the leftover stays until this group advances.
// The viewing group's ordinal comes from the event store's membership (matched by this
// game's id); null for ordinary games. On advance the act stage changes and the new
// stage reads 0, so the pseudo tokens reset naturally. NOTE: shows the VIEWING group's
// pool only; an organizer/shared view could show every group's pool (follow-up).
const eventStore = useEventStore()
const thisGroupOrdinal = computed<number | null>(() => {
  const ev = eventStore.event
  if (!ev) return null
  const group = ev.groups.find((g) => g.gameId === props.game.id)
  return group ? group.ordinal : null
})
const sharedContribution = computed(() => {
  const ordinal = thisGroupOrdinal.value
  if (ordinal === null) return 0
  const stage = props.act.sequence.number
  const contributed = actContribution(eventStore.sharedState, stage, ordinal)
  const spent = actSpend(eventStore.sharedState, stage, ordinal)
  return Math.max(0, contributed - spent)
})

const nextToScarletKeys = computed(() => Object.values(props.game.scarletKeys).
  filter((s) => s.placement.tag === "NextToAct").
  map((s) => s.id))

</script>

<template>
  <div class="act-container">
    <div class="act-row">
      <div class="card-container" :class="{ 'act--objective': hasObjective, 'objective-ring': hasObjective }">
        <img
          :class="{ 'act--can-progress': interactAction !== -1, 'act--can-interact': canInteract, 'card--sideways': !isVertical}"
          class="card"
          @click="clicked"
          :src="image"
          ref="frame"
        />
      </div>
      <StackIndicator
        label="Act"
        :current="currentActPosition"
        :total="totalActs"
        :completedCards="completedStack"
        :currentImage="image"
        :remainingCards="futureStack"
        :groups="groupedActStack"
      />
    </div>
    <AbilitiesMenu
      :frame="frame"
      v-model="showAbilities"
      :abilities="abilities"
      :game="game"
      position="bottom"
      @choose="chooseAbility"
      />
    <button v-if="cardsUnder.length > 0 && canViewUnder" class="view-cards-under-button" @click="showCardsUnderAct">{{viewUnderLabel}}</button>
    <button v-else-if="cardsUnder.length > 0" class="view-cards-under-button" disabled>{{viewUnderLabel}}</button>
    <div class="card-container" v-for="(card, idx) in cardsNextTo" :key="idx">
      <img
        class="card card--sideways"
        :src="imageForCard(card)"
      />
    </div>
    <Treachery
      v-for="treacheryId in act.treacheries"
      :key="treacheryId"
      :treachery="game.treacheries[treacheryId]"
      :game="game"
      :playerId="playerId"
      @choose="$emit('choose', $event)"
    />
    <ScarletKey
      v-for="scarletKeyId in nextToScarletKeys"
      :scarletKey="game.scarletKeys[scarletKeyId]"
      :game="game"
      :playerId="playerId"
      @choose="$emit('choose', $event)"
    />

    <div class="pool">
      <PoolItem
        v-if="clues > 0"
        type="clue"
        :amount="clues"
      />
      <span
        v-if="sharedContribution > 0"
        class="shared-clue-pool"
        :title="$t('event.sharedClues')"
      >
        <PoolItem type="clue" :amount="sharedContribution" />
      </span>
      <PoolItem v-if="resources > 0" type="resource" :amount="resources" />
      <PoolItem v-if="breaches > 0" type="resource" :amount="breaches" />
      <KeyToken v-for="k in keys" :key="keyToId(k)" :keyToken="k" :game="game" :playerId="playerId" @choose="$emit('choose', $event)" />
    </div>
  </div>
</template>

<style scoped>
.act-container :deep(.card) {
  flex: 0;
  width: var(--card-width);
  border-radius: inherit;
}

.card-container {
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  position: relative;
  border-radius: 6px;
  height: var(--card-width);
  width: fit-content;
}

.act--objective {
  --objective-ring-radius: 6px;
}

.act-container {
  display: flex;
  flex-direction: column;
  gap: 5px;
}

.act-row {
  display: flex;
  flex-direction: row;
  align-items: flex-start;
  gap: 6px;
}

.act-row :deep(.v-popper) {
  align-self: center;
}

.act-container :deep(.card--sideways) {
  width: auto;
  height: var(--card-width);
  aspect-ratio: var(--card-sideways-aspect);
}

.act--can-progress {
  border: 2px solid var(--select);
  border-radius: 8px;
  cursor: pointer;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
}

.ability-button {
  background-color: var(--button);
  &:before {
    font-family: "arkham";
    content: "\0049";
    margin-right: 5px;
  }
}

.abilities {
  padding: 10px;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 10px;
  button {
    padding: 4px;
  }
}


.card-container:not(.act--objective) {
  .act--can-interact {
    border: 2px solid var(--select);
    cursor: pointer;
  }
}

/* Pseudo (shared-pool) clue tokens read slightly softer than real act tokens. */
.shared-clue-pool {
  display: inline-flex;
  opacity: 0.85;
  filter: saturate(0.85);
}

</style>
