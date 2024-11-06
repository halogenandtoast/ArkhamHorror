<script lang="ts" setup>
import { ComputedRef, computed, ref } from 'vue'
import { type Game } from '@/arkham/types/Game'
import { type Card, cardImage } from '@/arkham/types/Card'
import AbilitiesMenu from '@/arkham/components/AbilitiesMenu.vue'
import PoolItem from '@/arkham/components/PoolItem.vue';
import Key from '@/arkham/components/Key.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import * as ArkhamGame from '@/arkham/types/Game'
import { AbilityLabel, AbilityMessage, type Message } from '@/arkham/types/Message'
import { MessageType } from '@/arkham/types/Message'
import { imgsrc } from '@/arkham/helpers'
import * as Arkham from '@/arkham/types/Act'

const props = defineProps<{
  act: Arkham.Act
  game: Game
  cardsUnder: Card[]
  cardsNextTo: Card[]
  playerId: string
}>()

const emits = defineEmits<{
  show: [cards: ComputedRef<Card[]>, title: string, isDiscards: boolean]
  choose: [value: number]
}>()

const showAbilities = ref(false)
const frame = ref(null)

const id = computed(() => props.act.id)

const keys = computed(() => props.act.keys)

const cardCode = computed(() => {
  const side = props.act.sequence.side.toLowerCase().replace('a', '')
  const sidePart = id.value.endsWith(side) ? "" : side

  let newId = id.value.replace(/^c/, '')

  if (sidePart == 'd') {
    newId = newId.replace(/c$/, '')
  }

  if (sidePart == 'f') {
    newId = newId.replace(/e$/, '')
  }

  // handle threads of fate as hardcoded values because I don't want to deal with it
  if (parseInt(newId) >= 4117 && parseInt(newId) <= 4140) {
    const adjustedSidePart = sidePart.replace(/[ace]/, '').replace(/[df]/, 'b')
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

const cardsUnder = computed(() => props.cardsUnder)

const showCardsUnderAct = () => emits('show', cardsUnder, 'Cards Under Act', false)

async function clicked() {
  if (interactAction.value !== -1) {
    emits('choose', interactAction.value)
  } else {
    showAbilities.value = !showAbilities.value
  }
}

async function chooseAbility(index: number) {
  showAbilities.value = false
  emits('choose', index)
}

const isOtherEncounterCard = computed(() => {
  return ["04134b", "04137b"].includes(cardCode.value)
})

const breaches = computed(() => {
  const {breaches} = props.act
  return breaches ?? 0
})

</script>

<template>
  <div class="act-container">
    <div class="card-container" :class="{ 'act--objective': hasObjective }">
      <img
        :class="{ 'act--can-progress': interactAction !== -1, 'act--can-interact': canInteract, 'card--sideways': !isOtherEncounterCard}"
        class="card"
        @click="clicked"
        :src="image"
        ref="frame"
      />

    </div>
    <AbilitiesMenu
      :frame="frame"
      v-model="showAbilities"
      :abilities="abilities"
      position="bottom"
      @choose="chooseAbility"
      />
    <button v-if="cardsUnder.length > 0" class="view-cards-under-button" @click="showCardsUnderAct">{{viewUnderLabel}}</button>
    <img
      v-for="(card, idx) in cardsNextTo"
      class="card card--sideways"
      :key="idx"
      :src="imageForCard(card)"
    />
    <Treachery
      v-for="treacheryId in act.treacheries"
      :key="treacheryId"
      :treachery="game.treacheries[treacheryId]"
      :game="game"
      :playerId="playerId"
      @choose="$emit('choose', $event)"
    />

    <div class="pool">
      <PoolItem
        v-if="act.clues && act.clues > 0"
        type="clue"
        :amount="act.clues"
      />
      <PoolItem v-if="breaches > 0" type="resource" :amount="breaches" />
      <Key v-for="key in keys" :key="key" :name="key" />
    </div>
  </div>
</template>

<style scoped lang="scss">
.act-container :deep(.card) {
  flex: 0;
  width: var(--card-width);
  border-radius: inherit;
}

@keyframes rotation {
  0% { --gradient-angle: 0deg; }
  100% { --gradient-angle: 360deg; }
}

.card-container {
  --gradient-angle: 0deg;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  position: relative;
  border-radius: 6px;
  height: var(--card-width);
  width: fit-content;
}

@property --gradient-angle {
  syntax: "<angle>";
  initial-value: 0deg;
  inherits: false;
}

.act--objective {
  --clr-1: #198891;
  --clr-2: #2d8f85;
  --clr-3: #73fb22;

  &::before, &::after {
    content: "";
    position: absolute;
    inset: -0.1rem;
    z-index: -1;
    background: conic-gradient(
      from var(--gradient-angle),
      var(--clr-1),
      var(--clr-2),
      var(--clr-3),
      var(--clr-2),
      var(--clr-1));
    border-radius: inherit;
    animation: rotation 1s linear infinite;
  }

  &::after {
    filter: blur(3.5rem);
  }
}

.act-container {
  display: flex;
  flex-direction: column;
  gap: 5px;
}

.act-container :deep(.card--sideways) {
  width: auto;
  height: var(--card-width);
  aspect-ratio: var(--card-sideways-aspect);
}

.act--can-progress {
  border: 2px solid #ff00ff;
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
  background-color: #555;
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
</style>
