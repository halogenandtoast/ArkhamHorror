<script lang="ts" setup>
import type { CardContents } from '@/arkham/types/Card';
import * as CardT from '@/arkham/types/Card';
import gsap from 'gsap';
import { computed, inject, Ref, ref, ComputedRef, reactive, watch, onMounted, onBeforeUnmount } from 'vue';
import { useDebug } from '@/arkham/debug';
import { Game } from '@/arkham/types/Game';
import { toCardContents } from '@/arkham/types/Card';
import { imgsrc, pluralize } from '@/arkham/helpers';
import * as ArkhamCard from '@/arkham/types/Card';
import * as ArkhamGame from '@/arkham/types/Game';
import Enemy from '@/arkham/components/Enemy.vue';
import Story from '@/arkham/components/Story.vue';
import Location from '@/arkham/components/Location.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import Asset from '@/arkham/components/Asset.vue';
import Event from '@/arkham/components/Event.vue';
import Skill from '@/arkham/components/Skill.vue';
import HandCard from '@/arkham/components/HandCard.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import ChoiceModal from '@/arkham/components/ChoiceModal.vue';
import { TarotCard, tarotCardImage } from '@/arkham/types/TarotCard';
import * as Arkham from '@/arkham/types/Investigator';
import { useI18n } from 'vue-i18n';
import Draw from '@/arkham/components/Draw.vue'
import { IsMobile } from '@/arkham/isMobile';
const { t } = useI18n();

interface RefWrapper<T> {
  ref: ComputedRef<T>
}

export interface Props {
  game: Game
  investigator: Arkham.Investigator
  playerId: string
  tarotCards: TarotCard[]
}

const props = defineProps<Props>()

const investigatorId = computed(() => props.investigator.id)

const solo = inject<Ref<boolean>>('solo')
const encounterBack = computed(() => {
  return imgsrc("encounter_back.jpg")
})

const assets = computed(() => {
  const xs = props.investigator.assets.map((a) => props.game.assets[a])
  xs.sort((a, b) => {
    if (a.cardCode < b.cardCode) {
      return -1;
    }
    if (a.cardCode > b.cardCode) {
      return 1;
    }

    if (a.cardId < b.cardId) {
      return -1;
    }
    if (a.cardId > b.cardId) {
      return 1;
    }

    return 0;
  })
  xs.sort((a, b) => b.permanent - a.permanent)
  return xs
})

const currentTreacheries = computed(() => {
  return Object.
    values(props.game.treacheries).
    filter((t) => t.placement.tag === 'Limbo' && t.drawnBy === investigatorId.value && (props.playerId === props.investigator.playerId || !t.peril))
})

const stories = computed(() =>
  Object.
    values(props.game.stories).
    filter((s) => s.placement.tag === "InThreatArea" && s.placement.contents === investigatorId.value && s.otherSide === null)
)

const engagedEnemies = computed(() =>
  props.investigator.engagedEnemies.map((e) => props.game.enemies[e]).filter((e) =>
    e.placement.tag === "InThreatArea" && e.placement.contents === investigatorId.value
  )
)

const inHandEnemies = computed(() =>
  Object.values(props.game.enemies).filter((e) => (e.placement.tag === "StillInHand" || e.placement.tag === "HiddenInHand") && e.placement.contents === investigatorId.value)
)

const discards = computed<ArkhamCard.Card[]>(() => props.investigator.discard.map(c => { return { tag: 'PlayerCard', contents: c }}))

const hunchDeck = computed(() => {
  const match = props.investigator.decks.find(([k,]) => k === "HunchDeck")
  if (match) {
    return match[1]
  }

  return null
})

const topOfHunchDeckRevealed = computed(() => {
  const { revealedHunchCard } = props.investigator
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

const viewingDiscard = ref(false)

const id = computed(() => props.investigator.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const tarotCardAbility = (card: TarotCard) => {
  if(props.playerId !== props.investigator.playerId) {
    return -1
  }
  return choices.value.findIndex((c) => {
    if (c.tag === "AbilityLabel") {
      return c.ability.source.sourceTag === "TarotSource" && c.ability.source.contents.arcana === card.arcana
    }

    return false
  })
}

const noCards = computed<ArkhamCard.Card[]>(() => [])

// eslint-disable-next-line
const showCards = reactive<RefWrapper<any>>({ ref: noCards })
const cardRowTitle = ref("")

const inHandTreacheries = computed(() => Object.values(props.game.treacheries).
  filter((t) => t.placement.tag === "HiddenInHand" && t.placement.contents === id.value))

const totalHandSize = computed(() => {
  const sizeModifiers: Record<string, number> = props.game.modifiers.reduce((a, m) => {
    if (m[1][0].type?.tag === "HandSizeCardCount") {
      if (m[0].tag === "CardIdTarget") {
        if(typeof m[0].contents === 'string') {
          return {...a, [m[0].contents]: m[1][0].type.contents}
        }
      }
    }
    return a
  }, {})

  const playerHandSize = playerHand.value.reduce((a, c) => {
    return a + (sizeModifiers[toCardContents(c).id] ?? 1)
  }, 0)

  const treacheryHandSize = inHandTreacheries.value.reduce((a, c) => {
    return a + (sizeModifiers[c.cardId] ?? 1)
  }, 0)

  const enemyHandSize = inHandEnemies.value.reduce((a, c) => {
    return a + (sizeModifiers[c.cardId] ?? 1)
  }, 0)

  return playerHandSize + treacheryHandSize + enemyHandSize
})

const handSizeClasses = computed(() => ({
  'hand-size-ok': (props.investigator.handSize ?? 8) > totalHandSize.value,
  'hand-size-warn': (props.investigator.handSize ?? 8) == totalHandSize.value,
  'hand-size-alert': (props.investigator.handSize ?? 8) < totalHandSize.value,
}))

const doShowCards = (event: Event, cards: ComputedRef<ArkhamCard.Card[]>, title: string, isDiscards: boolean) => {
  cardRowTitle.value = title
  showCards.ref = cards
  viewingDiscard.value = isDiscards
}

const showDiscards = (e: Event) => doShowCards(e, discards, t('investigator.discards'), true)

watch(choices, async (newChoices) => {
  const shouldShow = newChoices.some((c) => c.tag === "AbilityLabel" && discards.value.map((d) => toCardContents(d).id).includes(c.ability.source.contents))
  if (shouldShow) {
    showDiscards(new CustomEvent("showDiscards"))
  }
})

const committedCards = computed(() => props.game.skillTest?.committedCards || [])
const playerHand = computed(() => props.investigator.hand.
  filter((card) =>
    !committedCards.value.some((cc) => toCardContents(card).id == toCardContents(cc).id))
)

const locations = computed(() => Object.values(props.game.locations).
  filter((a) => a.inFrontOf === props.investigator.id))

const debug = useDebug()
const events = computed(() => props.investigator.events.map((e) => props.game.events[e]).filter(e => e))
const skills = computed(() => props.investigator.skills.map((e) => props.game.skills[e]).filter(e => e))
const emptySlots = computed(() => props.investigator.slots.filter((s) => s.empty))
const { isMobile } = IsMobile();

const slotImg = (slot: Arkham.Slot, idx: number) => {
  switch (slot.tag) {
    case 'HandSlot':
      return imgsrc(idx === 0 ? 'slots/left-hand.png' : 'slots/hand.png')
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
}

// global position information for animation
const rectData = ref<Array<[string, DOMRect]>>([])

function isHtmlElement(el: Element): el is HTMLElement {
  return el instanceof HTMLElement
}

function onBeforeEnter(el: Element) {
  if (!isHtmlElement(el)) { return }
  if(Object.values(el.classList).includes("committed-skills")) {
    return
  }

  const index = el.dataset.index
  if(!index) { return }

  const data = rectData.value.find(([idx,]) => idx === index)
  if (!data) { return }
  el.style.opacity = "0"
  el.style.width = "0"
}

function onEnter(el: Element, done: () => void) {
  if (!isHtmlElement(el)) { return }

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
  const c = el.cloneNode(true) as HTMLElement
  c.style.position = "fixed"
  c.style.width = rect.width + "px"
  el.parentNode?.insertBefore(c, el)
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
function onLeave(el: Element, done: () => void) {
  if (!isHtmlElement(el)) { return }
  if(Object.values(el.classList).includes("committed-skills")) {
    done();
    return
  }
  if(!el.dataset.index) {
    console.error("No data-index on element", el)
    done()
    return
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

const realityAcid = ref('89005')

const dragover = (e: DragEvent) => {
  e.preventDefault()
  if (e.dataTransfer) {
    e.dataTransfer.dropEffect = 'copy'
  }
}

function onDropHand(event: DragEvent) {
  event.preventDefault()
  if (event.dataTransfer) {
    const data = event.dataTransfer.getData('text/plain')
    if (data) {
      const json = JSON.parse(data)
      if (json.tag === "CardTarget") {
        debug.send(props.game.id, {tag: 'DebugAddToHand', contents: [id.value, json.contents]})
      }
    }
  }
}

function startHandDrag(event: DragEvent, card: (CardContents | CardT.Card)) {
  if (!debug.active) {
    event.preventDefault()
    return
  }
  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'copy'
    const cardId = CardT.toCardContents(card).id
    event.dataTransfer.setData('text/plain', JSON.stringify({ "tag": "CardTarget", "contents": cardId }))
  }
}

const touchStartX = ref(0)
const touchEndX = ref(0)
const emit = defineEmits(['swipe-right', 'swipe-left'])

function onTouchStart(event: TouchEvent) {
  touchStartX.value = event.changedTouches[0].screenX
}
function onTouchEnd(event: TouchEvent) {
  touchEndX.value = event.changedTouches[0].screenX
  handleSwipe()
}
function handleSwipe() {
  const deltaX = touchEndX.value - touchStartX.value
  if (Math.abs(deltaX) > 50) {
    if (deltaX > 0) {
      emit('swipe-right')
    } else {
      emit('swipe-left')
    }
  }
}
const handCardHeight = Math.min(7 * window.innerWidth / 50 + 114, 340);
const handCardExposedHeight_MIN = `${-0.85 * handCardHeight}`;
const handCardExposedHeight_MAX = `${-0.35 * handCardHeight}`;
const handAreaMarginBottom = ref(handCardExposedHeight_MIN);
const handAreaPointerEvents = ref('none');

onMounted(() => {
  if (isMobile) {
      document.addEventListener('click',toggleHandAreaMarginBottom)
  }
});

onBeforeUnmount(() => {
  if (isMobile) {
      document.removeEventListener('click', toggleHandAreaMarginBottom)
  }
});

function toggleHandAreaMarginBottom(event: Event) {
  const target = event.target as HTMLElement
  if (target.classList.contains('hand-area-IsMobile')) {
    handAreaMarginBottom.value = handCardExposedHeight_MAX;
    handAreaPointerEvents.value = 'auto'
  }
  else if(!target.classList.contains('in-hand')){
    handAreaMarginBottom.value = handCardExposedHeight_MIN;
    handAreaPointerEvents.value = 'none'
  }
}

</script>

<template>
  <div class="player-cards">
    <transition name="grow">
      <transition-group tag="section" class="in-play" @enter="onEnter" @leave="onLeave" @before-enter="onBeforeEnter">

        <template v-if="tarotCards.length > 0">
          <div v-for="tarotCard in tarotCards" :key="tarotCard.arcana" :data-index="tarotCard.arcana">
            <img :src="imgsrc(`tarot/${tarotCardImage(tarotCard)}`)" class="card tarot-card" :class="{ [tarotCard.facing]: true, 'can-interact': tarotCardAbility(tarotCard) !== -1 }" @click="$emit('choose', tarotCardAbility(tarotCard))"/>
          </div>
        </template>

        <img
          v-if="investigatorId === 'c89001'"
          class="card"
          @click="realityAcid = realityAcid === '89005' ? '89005b' : '89005'"
          :src="imgsrc(`cards/${realityAcid}.avif`)"
        />
        <Enemy
          v-if="isMobile"
          v-for="enemy in engagedEnemies"
          :key="enemy.id"
          :enemy="enemy"
          :game="game"
          :data-index="enemy.cardId"
          :playerId="playerId"
          @choose="$emit('choose', $event)"
        />
        <Treachery
          v-if="isMobile"
          v-for="treacheryId in investigator.treacheries"
          :key="treacheryId"
          :treachery="game.treacheries[treacheryId]"
          :game="game"
          :data-index="game.treacheries[treacheryId].cardId"
          :playerId="playerId"
          @choose="$emit('choose', $event)"
        />

        <Location
          v-if="isMobile"
          v-for="(location, key) in locations"
          class="location"
          :key="key"
          :game="game"
          :playerId="playerId"
          :location="location"
          :data-index="location.cardId"
          :style="{ 'grid-area': location.label, 'justify-self': 'center' }"
          @choose="$emit('choose', $event)"
        />
        <Treachery
          v-for="treachery in currentTreacheries"
          :key="treachery.id"
          :treachery="treachery"
          :game="game"
          :data-index="treachery.cardId"
          :playerId="playerId"
          @choose="$emit('choose', $event)"
        />

        <Skill
          v-for="skill in skills"
          :skill="skill"
          :game="game"
          :playerId="playerId"
          :key="skill.id"
          :data-index="skill.cardId"
          @choose="$emit('choose', $event)"
          @showCards="doShowCards"
        />
        <Event
          v-for="event in events"
          :event="event"
          :game="game"
          :playerId="playerId"
          :key="event.id"
          :data-index="event.cardId"
          @choose="$emit('choose', $event)"
          @showCards="doShowCards"
        />
        <Asset
          v-for="asset in assets"
          :asset="asset"
          :game="game"
          :playerId="playerId"
          :key="asset.id"
          :data-index="asset.cardId"
          @choose="$emit('choose', $event)"
          @showCards="doShowCards"
        />

        <Story
          v-for="story in stories"
          :key="story.id"
          :story="story"
          :game="game"
          :data-index="story.cardId"
          :playerId="playerId"
          @choose="$emit('choose', $event)"
        />


        <div v-for="(slot, idx) in emptySlots" :key="idx" class="slot" :data-index="`${slot}${idx}`">
          <img :src="slotImg(slot,idx)" />
        </div>

        <Enemy
          v-if="!isMobile"
          v-for="enemy in engagedEnemies"
          :key="enemy.id"
          :enemy="enemy"
          :game="game"
          :data-index="enemy.cardId"
          :playerId="playerId"
          @choose="$emit('choose', $event)"
        />

        <Treachery
          v-if="!isMobile"
          v-for="treacheryId in investigator.treacheries"
          :key="treacheryId"
          :treachery="game.treacheries[treacheryId]"
          :game="game"
          :data-index="game.treacheries[treacheryId].cardId"
          :playerId="playerId"
          @choose="$emit('choose', $event)"
        />

        <Location
          v-if="!isMobile"
          v-for="(location, key) in locations"
          class="location"
          :key="key"
          :game="game"
          :playerId="playerId"
          :location="location"
          :data-index="location.cardId"
          :style="{ 'grid-area': location.label, 'justify-self': 'center' }"
          @choose="$emit('choose', $event)"
        />
      </transition-group>
    </transition>

    <ChoiceModal
      v-if="playerId === investigator.playerId"
      :game="game"
      :playerId="playerId"
      @choose="$emit('choose', $event)"
    />

     <div class="player"  @touchstart="onTouchStart" @touchend="onTouchEnd">
      <div v-if="hunchDeck" class="top-of-deck hunch-deck">
        <HandCard
          v-if="topOfHunchDeck && topOfHunchDeckRevealed"
          :card="topOfHunchDeck"
          :game="game"
          :ownerId="investigator.id"
          :playerId="playerId"
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

      <div class="investigator-and-deck">
        <Investigator
          :game="game"
          :investigator="investigator"
          :choices="choices"
          :playerId="playerId"
          @choose="$emit('choose', $event)"
          @showCards="doShowCards"
        />
        <Draw
          v-if="!isMobile"
          :game="game"
          :playerId="playerId"
          :investigator="investigator"
          @choose="$emit('choose', $event)"
        />
      </div>
      <div v-if="!isMobile" class="hand hand-area">
        <transition-group tag="section" class="hand" @enter="onEnter" @leave="onLeave" @before-enter="onBeforeEnter"
          @drop="onDropHand($event)"
          @dragover.prevent="dragover($event)"
          @dragenter.prevent
          >
          <HandCard
            v-for="card in playerHand"
            :card="card"
            :game="game"
            :playerId="playerId"
            :ownerId="investigator.id"
            :key="toCardContents(card).id"
            @choose="$emit('choose', $event)"
            :draggable="debug.active"
            @dragstart="startHandDrag($event, card)"
          />

          <template v-for="enemy in inHandEnemies" :key="enemy.id">
            <Enemy
              v-if="solo || (playerId == investigator.playerId)"
              :enemy="enemy"
              :game="game"
              :data-index="enemy.cardId"
              :playerId="playerId"
              @choose="$emit('choose', $event)"
            />
            <div class="card-container" v-else>
              <img class="card" :src="encounterBack" />
            </div>
          </template>

          <template v-for="treachery in inHandTreacheries" :key="treachery.id">
            <Treachery
              v-if="solo || (playerId == investigator.playerId)"
              :treachery="treachery"
              :game="game"
              :data-index="treachery.id"
              :playerId="playerId"
              @choose="$emit('choose', $event)"
            />
            <div class="card-container" v-else>
              <img class="card" :src="encounterBack" />
            </div>
          </template>

        </transition-group>
        <div v-if="investigator.handSize" class="hand-size" :class="handSizeClasses" :current-length="totalHandSize">Hand Size: {{totalHandSize}}/{{investigator.handSize}}</div>
      </div>
    </div>
    <div v-if="isMobile" class="hand hand-area-IsMobile" :style="{ marginBottom: `${handAreaMarginBottom}px` }" @click="toggleHandAreaMarginBottom">
        <transition-group tag="section" class="hand" @enter="onEnter" @leave="onLeave" @before-enter="onBeforeEnter"
          @drop="onDropHand($event)"
          @dragover.prevent="dragover($event)"
          @dragenter.prevent
          :style="{ pointerEvents: `${handAreaPointerEvents}` }"
          >
          <HandCard
            v-for="card in playerHand"
            :card="card"
            :game="game"
            :playerId="playerId"
            :ownerId="investigator.id"
            :key="toCardContents(card).id"
            @choose="$emit('choose', $event)"
            :draggable="debug.active"
            @dragstart="startHandDrag($event, card)"
          />

          <template v-for="enemy in inHandEnemies" :key="enemy.id">
            <Enemy
              v-if="solo || (playerId == investigator.playerId)"
              :enemy="enemy"
              :game="game"
              :data-index="enemy.cardId"
              :playerId="playerId"
              @choose="$emit('choose', $event)"
            />
            <div class="card-container" v-else>
              <img class="card" :src="encounterBack" />
            </div>
          </template>

          <template v-for="treacheryId in inHandTreacheries" :key="treacheryId">
            <Treachery
              v-if="solo || (playerId == investigator.playerId)"
              :treachery="game.treacheries[treacheryId]"
              :game="game"
              :data-index="treacheryId"
              :playerId="playerId"
              @choose="$emit('choose', $event)"
            />
            <div class="card-container" v-else>
              <img class="card" :src="encounterBack" />
            </div>
          </template>

        </transition-group>
    </div>
  </div>
</template>

<style scoped lang="scss">
.player {
  display: flex;
  gap: 5px;
  align-self: safe center;
  align-items: flex-start;
  padding: 10px;
  background: var(--background-dark);
}

:deep(.location) {
  .location-container {
    margin: 0 10px;
  }

  .location-investigator-column {
    position: unset;
  }

  .location-asset-column {
    position: unset;
    width: auto;
    min-width: unset;
  }

  .location-asset-column .exhausted{
    margin-left: calc(var(--card-width) - (var(--card-width) * 7 / 9));
    margin-right: 10px;
    transform: rotate(90deg) translateX(-10px);
  }
}

.deck {
  width: auto;
  box-shadow: var(--card-shadow);
}

.in-play {
  display: flex;
  flex-wrap: wrap;
  overflow-x: auto;
  gap: 5px;
  background: #999;
  padding: 10px;
  background: var(--background-dark);
  border-bottom: 1px solid var(--background);
  border-top: 1px solid var(--background);
  img {
    pointer-events: none;
    user-select: none;
    -webkit-user-drag: none;
  }
  @media (max-width: 800px) and (orientation: portrait) {
    flex-wrap: nowrap; 
    scrollbar-width: thin;
    :deep(div){
      flex-shrink: 0;
    }
    :deep(div.slot) {
      width: 10.71vw;
      height: 14.994vw;
    }
    :deep(.card) {
      width: 10.71vw ;
      height: 14.994vw;
      max-width: 10.71vw;
    }
  }
}

.hand {
  flex: 0;
  display: flex;
  gap: 5px;
  overflow-x: auto;
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
  width: var(--card-width);
  background: rgba(0,0,0,0.2);
  aspect-ratio: 5 / 7;
  height: calc(var(--card-width) * 7 / 5);
  border-radius: 6px;
  overflow: hidden;
  display: grid;
  place-items: center;
  border: 1px solid rgba(255, 255, 255, 0.3);
  img {
    width: calc(var(--card-width) / 2);
    filter: invert(75%);
  }
  @media (max-width: 800px) and (orientation: portrait) {
    img {
      width: 7vw;
    }
  }
}

.tarot-card {
  &.can-interact {
    border: 2px solid var(--select);
  }
}

.split-view .hand {
  flex-wrap: wrap;
  min-height: fit-content;
  overflow: unset;
}

.investigator-and-deck {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  gap: 5px;
  @media (max-width: 600px) {
      width: 100%;
  }
}

.hand-size {
  padding: 3px;
  justify-items: center;
  font-size: 0.7rem;
  color: white;
  background-color: var(--neutral-dark);
  display: grid;
  grid-template-columns: 1fr;
  width: calc((v-bind(totalHandSize) * var(--card-width)) + ((v-bind(totalHandSize) - 1) * 5px));
  max-width: 100%;
  min-width: fit-content;

  &-ok {
    background-color: var(--rogue-dark);
  }

  &-warn {
    background-color: var(--seeker-dark);
  }

  &-alert {
    background-color: var(--survivor-dark);
  }
}


.hand-area {
  display: flex;
  flex-direction: column;
  gap: 5px;
  align-items: flex-start;
  flex: 1;
  max-width: 100%;
}

.hand-area-IsMobile {
  display: flex;
  flex-direction: column;
  gap: 5px;
  align-items: flex-start;
  flex: 1;
  max-width: 100%;
  :deep(.card){
    width: calc(var(--card-width) * 4);
    min-width: calc(var(--card-width) * 4);
  }
}

</style>
