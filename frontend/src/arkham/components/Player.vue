<script lang="ts" setup>
import gsap from 'gsap';
import { computed, ref, ComputedRef, reactive } from 'vue';
import { useDebug } from '@/arkham/debug';
import { Game } from '@/arkham/types/Game';
import { toCardContents } from '@/arkham/types/Card';
import { imgsrc, pluralize } from '@/arkham/helpers';
import * as ArkhamCard from '@/arkham/types/Card';
import * as ArkhamGame from '@/arkham/types/Game';
import CommittedSkills from '@/arkham/components/CommittedSkills.vue';
import Enemy from '@/arkham/components/Enemy.vue';
import Story from '@/arkham/components/Story.vue';
import Location from '@/arkham/components/Location.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import Asset from '@/arkham/components/Asset.vue';
import Event from '@/arkham/components/Event.vue';
import HandCard from '@/arkham/components/HandCard.vue';
import Card from '@/arkham/components/Card.vue';
import CardRow from '@/arkham/components/CardRow.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import ChoiceModal from '@/arkham/components/ChoiceModal.vue';
import { TarotCard, tarotCardImage } from '@/arkham/types/TarotCard';
import * as Arkham from '@/arkham/types/Investigator';

interface RefWrapper<T> {
  ref: ComputedRef<T>
}

export interface Props {
  game: Game
  player: Arkham.Investigator
  investigatorId: string
  tarotCards: TarotCard[]
}

const props = defineProps<Props>()

const stories = computed(() =>
  Object.
    values(props.game.stories).
    filter((s) => s.placement.tag === "InThreatArea" && s.placement.contents === props.investigatorId && s.otherSide === null)
)

const engagedEnemies = computed(() =>
  props.player.engagedEnemies.map((e) => props.game.enemies[e]).filter((e) => e.placement.tag === "InThreatArea" && e.placement.contents === props.investigatorId)
)

const inHandEnemies = computed(() =>
  game.enemies.filter((e) => e.placement.tag === "StillInHand" && e.placement.contents === props.investigatorId)
)

const discards = computed<ArkhamCard.Card[]>(() => props.player.discard.map(c => { return { tag: 'PlayerCard', contents: c }}))

const topOfDiscard = computed(() => discards.value[0])

const topOfDeckRevealed = computed(() =>
  props.player.modifiers?.some((m) => m.type.tag === "OtherModifier" && m.type.contents === "TopCardOfDeckIsRevealed")
)

const topOfDeck = computed(() => {
  const topCard = props.player.deck[0]
  if  (topOfDeckRevealed.value && topCard) {
    return imgsrc(`cards/${topCard.cardCode.replace(/^c/, '')}.jpg`)
  }
  return imgsrc("player_back.jpg")
})

const hunchDeck = computed(() => {
  const match = props.player.decks.find(([k,]) => k === "HunchDeck")
  if (match) {
    return match[1]
  }

  return null
})

const topOfHunchDeckRevealed = computed(() => {
  const { revealedHunchCard } = props.player
  const hunchCard = topOfHunchDeck.value
  if (hunchCard) {
    return toCardContents(hunchCard).id === revealedHunchCard
  }

  return false
})

const topOfHunchDeck = computed(() => {
  if (hunchDeck.value) {
    return hunchDeck.value[0]
  }

  return null
})

const playTopOfDeckAction = computed(() => {
  const topOfDeck = props.player.deck[0]
  if (topOfDeck !== undefined && topOfDeck !== null) {
    return choices.value.findIndex((c) => c.tag === "TargetLabel" && c.target.contents === props.player.deck[0].id)
  }
  return -1
})

const viewingDiscard = ref(false)
const viewDiscardLabel = computed(() => viewingDiscard.value ? "Close" : pluralize('Card', discards.value.length))

const id = computed(() => props.player.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

const tarotCardAbility = (card: TarotCard) => {
  return choices.value.findIndex((c) => {
    if (c.tag === "AbilityLabel") {
      return c.ability.source.tag === "TarotSource" && c.ability.source.contents.arcana === card.arcana
    }

    return false
  })
}

const drawCardsAction = computed(() => {
  return choices
    .value
    .findIndex((c) => {
      if (c.tag === "ComponentLabel") {
        return (c.component.tag == "InvestigatorDeckComponent")
      }
      return false
    });
})

const noCards = computed<ArkhamCard.Card[]>(() => [])

// eslint-disable-next-line
const showCards = reactive<RefWrapper<any>>({ ref: noCards })
const cardRowTitle = ref("")

const inHandTreacheries = computed(() => Object.values(props.game.treacheries).
  filter((t) => t.placement.tag === "TreacheryInHandOf" && t.placement.contents === id.value).
  map((t) => t.id))

const doShowCards = (event: Event, cards: ComputedRef<ArkhamCard.Card[]>, title: string, isDiscards: boolean) => {
  cardRowTitle.value = title
  showCards.ref = cards
  viewingDiscard.value = isDiscards
}

const showDiscards = (e: Event) => doShowCards(e, discards, 'Discards', true)
const hideCards = () => {
  showCards.ref = noCards
  viewingDiscard.value = false
}

const committedCards = computed(() => props.game.skillTest?.committedCards || [])
const playerHand = computed(() => props.player.hand.
  filter((card) =>
    !committedCards.value.some((cc) => toCardContents(card).id == toCardContents(cc).id))
)

const locations = computed(() => Object.values(props.game.locations).
  filter((a) => a.inFrontOf === props.player.id))

const debug = useDebug()
const events = computed(() => props.player.events.map((e) => props.game.events[e]).filter(e => e))
const emptySlots = computed(() => props.player.slots.filter((s) => s.empty))


const slotImg = (slot: Arkham.Slot) => {
  switch (slot.tag) {
    case 'HandSlot':
      return imgsrc('slots/hand.png')
    case 'BodySlot':
      return imgsrc('slots/body.png')
    case 'AccessorySlot':
      return imgsrc('slots/accessory.png')
    case 'ArcaneSlot':
      return imgsrc('slots/arcane.png')
    case 'TarotSlot':
      return imgsrc('slots/tarot.png')
    case 'AllySlot':
      return imgsrc('slots/ally.png')
  }

  return ""
}

// function beforeLeaveHand(e: Element) {
//   const el = e as HTMLElement
//   const {marginLeft, marginTop, width, height} = window.getComputedStyle(el)
//   el.style.left = `${el.offsetLeft - parseFloat(marginLeft)}px`
//   el.style.top = `${el.offsetTop - parseFloat(marginTop)}px`
//   el.style.width = width
//   el.style.height = height
// }

// global position information for animation
const rectData = ref ([])

function onBeforeEnter(el) {
  if(Object.values(el.classList).includes("committed-skills")) {
    return
  }

  const index = el.dataset.index
  if(!index) { return }

  const data = rectData.value.find(([idx,]) => idx === index)
  if (!data) { return }
  el.style.opacity = 0
  el.style.width = 0
}

function onEnter(el, done) {
  if(Object.values(el.classList).includes("committed-skills")) {
    el.removeAttribute("style")
    done();
  }
  const index = el.dataset.index
  const finalRect = el.getBoundingClientRect()
  if(!index) {
    const width = window.getComputedStyle(el).width
    gsap.to(el, { opacity: 1, width, onComplete: () => {
      el.removeAttribute("style")
      done()
    }})
    return
  }
  const data = rectData.value.find(([idx,]) => idx === index)
  rectData.value = rectData.value.filter(([idx,]) => idx !== index)
  if (!data) {
    //gsap.to(el, { startAt: { height: 0, marginLeft: 40, marginTop: 56 }, ease: "power4.out", opacity: 1, width: 80, height: 113, marginLeft: 0, marginTop: 0, onComplete: () => {
      el.removeAttribute("style")
      done()
    //} })
    return
  }
  const [,rect] = data
  const startX = rect.left - finalRect.left
  const startY = rect.top - finalRect.top
  const c = el.cloneNode(true)
  c.style.position = "fixed"
  c.style.width = rect.width + "px"
  el.parentNode.insertBefore(c, el)
  const cRect = c.getBoundingClientRect()
  const finalX = finalRect.left - cRect.left
  const tl = gsap.timeline()
  tl.
    add("start").
    to(el, {
      startAt: { opacity: 0, width: 0 },
      width: rect.width,
      clearProps: "width",
      duration: 0.3
    }, "start").
    to(c, {
      startAt: { x: startX, y: startY, opacity: 1 },
      y: 0,
      x: finalX,
      onComplete: () => {
        c.remove()
        el.style.opacity = "1"
        done()
      },
      duration: 0.3
    }, "start")
}
function onLeave(el, done) {
  if(Object.values(el.classList).includes("committed-skills")) {
    done();
  }
  rectData.value = [...rectData.value, [el.dataset.index, el.getBoundingClientRect()]]
  gsap.to(el, {
    startAt: { opacity: 0 },
    width: 0,
    margin: 0,
    onComplete: done,
    duration: 0.3
  })
}
</script>

<template>
  <div class="player-cards">
    <transition name="grow">
    <transition-group tag="section" class="in-play" @enter="onEnter" @leave="onLeave" @before-enter="onBeforeEnter">

        <template v-if="tarotCards.length > 0">
          <div v-for="tarotCard in tarotCards" :key="tarotCard.arcana">
            <img :src="imgsrc(`tarot/${tarotCardImage(tarotCard)}`)" class="card tarot-card" :class="{ [tarotCard.facing]: true, 'can-interact': tarotCardAbility(tarotCard) !== -1 }" @click="choose(tarotCardAbility(tarotCard))"/>
          </div>
        </template>
        <Event
          v-for="event in events"
          :event="event"
          :game="game"
          :investigatorId="investigatorId"
          :key="event.id"
          :data-index="event.cardId"
          @choose="$emit('choose', $event)"
          @showCards="doShowCards"
        />
        <Asset
          v-for="asset in player.assets"
          :asset="game.assets[asset]"
          :game="game"
          :investigatorId="investigatorId"
          :key="asset"
          @choose="$emit('choose', $event)"
          @showCards="doShowCards"
        />

        <Story
          v-for="story in stories"
          :key="story.id"
          :story="story"
          :game="game"
          :data-index="story.cardId"
          :investigatorId="investigatorId"
          @choose="$emit('choose', $event)"
        />

        <Enemy
          v-for="enemy in engagedEnemies"
          :key="enemy.id"
          :enemy="enemy"
          :game="game"
          :data-index="enemy.cardId"
          :investigatorId="investigatorId"
          @choose="$emit('choose', $event)"
        />

        <Treachery
          v-for="treacheryId in player.treacheries"
          :key="treacheryId"
          :treachery="game.treacheries[treacheryId]"
          :game="game"
          :data-index="game.treacheries[treacheryId].cardId"
          :investigatorId="investigatorId"
          @choose="$emit('choose', $event)"
        />

        <Location
          v-for="(location, key) in locations"
          class="location"
          :key="key"
          :game="game"
          :investigatorId="investigatorId"
          :location="location"
          :data-index="location.cardId"
          :style="{ 'grid-area': location.label, 'justify-self': 'center' }"
          @choose="$emit('choose', $event)"
        />

        <div v-for="(slot, idx) in emptySlots" :key="idx" class="slot">
          <img :src="slotImg(slot)" />
        </div>

        <div v-if="committedCards.length > 0" class="committed-skills">
          <h2>Committed Skills</h2>
          <CommittedSkills
            :game="game"
            :cards="committedCards"
            :investigatorId="investigatorId"
            @choose="$emit('choose', $event)"
          />
        </div>
      </transition-group>
    </transition>

    <ChoiceModal
      :game="game"
      :investigatorId="id"
      @choose="$emit('choose', $event)"
    />

    <div class="player">
      <div v-if="hunchDeck" class="top-of-deck hunch-deck">
        <HandCard
          v-if="topOfHunchDeck && topOfHunchDeckRevealed"
          :card="topOfHunchDeck"
          :game="game"
          :ownerId="player.id"
          :investigatorId="investigatorId"
          @choose="$emit('choose', $event)"
        />
        <img
          v-else
          class="deck card"
          :src="imgsrc('player_back.jpg')"
          width="150px"
        />
        <span class="deck-size">{{hunchDeck.length}}</span>
      </div>

      <Investigator
        :game="game"
        :player="player"
        :choices="choices"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
        @showCards="doShowCards"
      />

      <div class="discard">
        <Card v-if="topOfDiscard" :game="game" :card="topOfDiscard" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
        <button v-if="discards.length > 0" class="view-discard-button" @click="showDiscards">{{viewDiscardLabel}}</button>
      </div>

      <div class="deck-container">
        <div class="top-of-deck">
          <img
            :class="{ 'deck--can-draw': drawCardsAction !== -1, 'card': topOfDeckRevealed }"
            class="deck"
            :src="topOfDeck"
            width="150px"
            @click="$emit('choose', drawCardsAction)"
          />
          <span class="deck-size">{{player.deckSize}}</span>
          <button v-if="playTopOfDeckAction !== -1" @click="$emit('choose', playTopOfDeckAction)">Play</button>
        </div>
        <template v-if="debug.active">
          <button @click="debug.send(game.id, {tag: 'Search', contents: ['Looking', investigatorId, {tag: 'GameSource', contents: []}, { tag: 'InvestigatorTarget', contents: investigatorId }, [[{tag: 'FromDeck', contents: []}, 'ShuffleBackIn']], {tag: 'AnyCard', contents: []}, { tag: 'DrawFound', contents: [investigatorId, 1]}]})">Select Draw</button>
        </template>
      </div>
      <transition-group tag="section" class="hand" @enter="onEnter" @leave="onLeave" @before-enter="onBeforeEnter">
        <HandCard
          v-for="card in playerHand"
          :card="card"
          :game="game"
          :investigatorId="investigatorId"
          :ownerId="player.id"
          :key="toCardContents(card).id"
          @choose="$emit('choose', $event)"
        />

        <Enemy
          v-for="enemy in inHandEnemies"
          :key="enemy.id"
          :enemy="enemy"
          :game="game"
          :data-index="enemy.cardId"
          :investigatorId="investigatorId"
          @choose="$emit('choose', $event)"
        />

        <Treachery
          v-for="treacheryId in inHandTreacheries"
          :key="treacheryId"
          :treachery="game.treacheries[treacheryId]"
          :game="game"
          :data-index="treacheryId"
          :investigatorId="investigatorId"
          @choose="$emit('choose', $event)"
        />

      </transition-group>
    </div>

    <CardRow
      v-if="showCards.ref.length > 0"
      :game="game"
      :investigatorId="investigatorId"
      :cards="showCards.ref"
      :isDiscards="viewingDiscard"
      :title="cardRowTitle"
      @choose="$emit('choose', $event)"
      @close="hideCards"
    />
  </div>
</template>

<style scoped lang="scss">
.player {
  display: flex;
  align-self: center;
  align-items: flex-start;
  padding: 10px;
  box-sizing: border-box;
}

.deck--can-draw {
  border: 3px solid $select;
  border-radius: 10px;
  cursor: pointer;
}

:deep(.location) {
  margin-left: $card-width * 0.8;
}


.discard {
  width: $card-width;
  margin-top: 10px;
  button {
    white-space: nowrap;
  }
  &:deep(.card) {
    margin: 0;
    box-shadow: none;
  }
  &:deep(.card-container) {
    width: $card-width;
    margin: 0;
    position:relative;
    display: inline-flex;
    &::after {
      pointer-events: none;
      border-radius: 6px;
      content: "";
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background-color: #FFF;
      opacity: .85;
      mix-blend-mode: saturation;
    }
  }
}

.deck, .card {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  max-width: $card-width;
}

.deck {
  width: auto;
}

.in-play {
  display: flex;
  background: #999;
  padding: 10px;
  box-sizing: border-box;

  @media (prefers-color-scheme: dark) {
    background: #1C1C1C;
  }

  & :deep(.asset) {
    margin-right: 5px;
  }
  & :deep(.enemy) {
    margin-right: 5px;
  }
}

.player-cards {
  box-sizing: border-box;
}

.hand {
  overflow-x: overlay;
  flex-grow: 1;
  display: flex;
  box-sizing: border-box;
  gap: 2px;
  margin-left: 2px;
  align-self: center;
}

.view-discard-button {
  width: 100%;
}

.deck-container {
  display: flex;
  flex-direction: column;
  align-self: center;
}

.top-of-deck {
  position: relative;
  display: flex;
  flex-direction: column;
  width: fit-content;
}

.deck-size {
  pointer-events: none;
  position: absolute;
  font-weight: bold;
  font-size: 1.2em;
  color: rgba(255, 255, 255, 0.6);
  left: 50%;
  top: 40%;
  background: rgba(0, 0, 0, 0.6);
  padding: 10px;
  border-radius: 20px;
  transform: translateX(-50%) translateY(-50%);
}

.hand-move,
.hand-enter-active,
.hand-leave-active {
  transition: all 0.3s ease;
}

.hand-enter-from,
.hand-leave-to {
  opacity: 0;
  transform: translateY(-40px);
}

.hand-leave-active {
  position: absolute;
}

.in-play-move,
.in-play-enter-active,
.in-play-leave-active {
  transition: all 0.3s ease;
}

.in-play-enter-from,
.in-play-leave-to {
  opacity: 0;
  transform: translateY(-40px);
}

.in-play-leave-active {
  position: absolute;
}

.deck-label {
  text-transform: uppercase;
  width: 80px;
  font-size: 12px;
  background: hsla(255 100% 100% / 0.5)
}

.hunch-deck {
  display: flex;
  justify-self: self-start;
  align-self: start;
}

.committed-skills {
  margin-left: auto;
  display: flex;
  h2 {
    text-align: center;
    text-transform: uppercase;
    font-size: 1.4vh;
    margin: 0;
    margin-top: -10px;
    margin-bottom: -10px;
    writing-mode: vertical-rl;
    orientation: mixed;
    color: rgba(255, 255, 255, 0.75);
  }
}

.slot {
  width: $card-width;
  background: rgba(0,0,0,0.5);
  aspect-ratio: 5 / 7;
  height: calc($card-width * 7 / 5);
  border-radius: 6px;
  overflow: hidden;
  display: grid;
  place-items: center;
  margin: 2px;
  border: 1px solid rgba(255, 255, 255, 0.3);
  box-sizing: border-box;
  img {
    width: calc($card-width / 2);
    filter: invert(75%);
  }
}
</style>
