<script lang="ts" setup>
import UpgradeDeck from '@/arkham/components/UpgradeDeck.vue';
import { EyeIcon, QuestionMarkCircleIcon, ViewColumnsIcon } from '@heroicons/vue/20/solid'
import {
  watchEffect,
  onMounted,
  onUpdated,
  computed,
  ref,
  ComputedRef,
  reactive,
} from 'vue';
import { type Game } from '@/arkham/types/Game';
import { type Scenario } from '@/arkham/types/Scenario';
import { type Card } from '@/arkham/types/Card';
import { TarotCard, tarotCardImage } from '@/arkham/types/TarotCard';
import { TokenType } from '@/arkham/types/Token';
import { Source } from '@/arkham/types/Source';
import { Message } from '@/arkham/types/Message';
import { waitForImagesToLoad, imgsrc, pluralize } from '@/arkham/helpers';
import { useMenu } from '@/composeable/menu';
import { useSettings } from '@/stores/settings';
import Act from '@/arkham/components/Act.vue';
import CardView from '@/arkham/components/Card.vue';
import Draggable from '@/components/Draggable.vue';
import ChaosBag from '@/arkham/components/ChaosBag.vue';
import Agenda from '@/arkham/components/Agenda.vue';
import Enemy from '@/arkham/components/Enemy.vue';
import CardRow from '@/arkham/components/CardRow.vue';
import Key from '@/arkham/components/Key.vue';
import PlayerTabs from '@/arkham/components/PlayerTabs.vue';
import Connections from '@/arkham/components/Connections.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import EncounterDeck from '@/arkham/components/EncounterDeck.vue';
import VictoryDisplay from '@/arkham/components/VictoryDisplay.vue';
import SkillTest from '@/arkham/components/SkillTest.vue';
import ScenarioDeck from '@/arkham/components/ScenarioDeck.vue';
import Story from '@/arkham/components/Story.vue';
import Location from '@/arkham/components/Location.vue';
import * as ArkhamGame from '@/arkham/types/Game';
import { useDebug } from '@/arkham/debug'
import { storeToRefs } from 'pinia';
import { useI18n } from 'vue-i18n';
const { t } = useI18n();

// types
interface RefWrapper<T> {
  ref: ComputedRef<T>
}

// Setup
export interface Props {
  game: Game
  scenario: Scenario
  playerId: string
}
const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const debug = useDebug()
const { addEntry, removeEntry } = useMenu()

const upgradeDeck = computed(() => Object.values(props.game.question).some((q) => q.tag === 'ChooseUpgradeDeck'))

// emit helpers
const choose = async (idx: number) => emit('choose', idx)

//Refs
const settingsStore = useSettings()
const { splitView } = storeToRefs(settingsStore)
const { toggleSplitView } = settingsStore
const needsInit = ref(true)
const showChaosBag = ref(false)
const showOutOfPlay = ref(false)
const forcedShowOutOfPlay = ref(false)
const locationMap = ref<Element | null>(null)
const viewingDiscard = ref(false)
const cardRowTitle = ref("")
// Atlach Nacha specific refs
const previousRotation = ref(0)
const legsSet = ref(["legs1", "legs2", "legs3", "legs4"])

const locationsZoom = ref(1);

// callbacks
onMounted(() => {
  if(props.scenario.id === "c06333") {
    waitForImagesToLoad(() => {
      rotateImages(true);
    })
  }
});

onUpdated(() => {
  if(props.scenario.id === "c06333") {
    rotateImages(needsInit.value)
  }
});

// Menu
addEntry({
  id: "viewChaosBag",
  icon: QuestionMarkCircleIcon,
  content: t('gameBar.viewChaosBag'),
  shortcut: "c",
  nested: 'view',
  action: () => showChaosBag.value = !showChaosBag.value
})

addEntry({
  id: "splitView",
  icon: ViewColumnsIcon,
  content: t('gameBar.splitView'),
  nested: 'view',
  action: toggleSplitView
})

// Computed
const scenarioGuide = computed(() => {
  const { reference, difficulty } = props.scenario
  const difficultySuffix = difficulty === 'Hard' || difficulty === 'Expert'
    ? 'b'
    : ''
  return imgsrc(`cards/${reference.replace('c', '')}${difficultySuffix}.avif`)
})
const scenarioDecks = computed(() => {
  if (!props.scenario.decks) return null
  return Object.entries(props.scenario.decks)
})

const isVertical = function(area: string) {
  const [start, end] = area.split('--')
  const startLocation = locations.value.find((l) => l.id === start);
  const endLocation = locations.value.find((l) => l.id === end);

  if (!startLocation || !endLocation) return false

  return startLocation.label[startLocation.label.length - 1] !== endLocation.label[endLocation.label.length - 1]
}

const barriers = computed(() => props.scenario.meta?.barriers)

const locationStyles = computed(() => {
  const { locationLayout } = props.scenario
  if (!locationLayout) return null
  let cleaned = locationLayout

  if (barriers.value) {
    let grid = {};
    locationLayout.forEach((row) => {
      row.split(' ').forEach((cell) => {
        const location = locations.value.find((l) => l.label === cell);
        if (!location) return;
        grid[cell] = location.id;
      });
    });

    // Process rows to insert barriers
    const cleanedRows = locationLayout.map((row) => row.split(' '));
    let newCleanedRows = [];

    for (let rowIndex = 0; rowIndex < cleanedRows.length; rowIndex++) {
      const row = cleanedRows[rowIndex];
      let newRow = [];

      for (let colIndex = 0; colIndex < row.length; colIndex++) {
        const cell = row[colIndex];
        newRow.push(cell);

        // Check for horizontal barriers
        if (colIndex < row.length - 1) {
          const cellA = cell;
          const cellB = row[colIndex + 1];
          const idA = grid[cellA];
          const idB = grid[cellB];
          if (idA && idB) {
            const ids = `barrier-${[idA, idB].sort().join('--')}`;
            newRow.push(ids); // Insert barrier
          } else {
            newRow.push('.'); // Insert period
          }
        }
      }
      newCleanedRows.push(newRow);
    }

    // Now process columns to insert vertical barriers
    let finalRows = [];

    for (let rowIndex = 0; rowIndex < newCleanedRows.length; rowIndex++) {
      const row = newCleanedRows[rowIndex];
      finalRows.push(row);

      // Check if we need to insert a row of barriers below this row
      if (rowIndex < newCleanedRows.length - 1) {
        const nextRow = newCleanedRows[rowIndex + 1];
        let barrierRow = [];
        let needBarrierRow = false;

        for (let colIndex = 0; colIndex < row.length; colIndex++) {
          const cellA = row[colIndex];
          const cellB = nextRow[colIndex];
          const idA = grid[cellA];
          const idB = grid[cellB];

          if (idA && idB) {
            const ids = `barrier-${[idA, idB].sort().join('--')}`;
            barrierRow.push(ids); // Insert vertical barrier
            needBarrierRow = true;
          } else {
            barrierRow.push('.'); // Insert period
          }
        }

        if (needBarrierRow) {
          finalRows.push(barrierRow);
        }
      }
    }

    // Update the 'cleaned' variable
    cleaned = finalRows.map((row) => row.join(' '));
  }

  return {
    display: 'grid',
    gap: '20px',
    'grid-template-areas': cleaned.map((row) => `"${row}"`).join(' '),
    zoom: locationsZoom.value
  }
})
const scenarioDeckStyles = computed(() => {
  const { decksLayout } = props.scenario
  return {
    display: 'grid',
    'grid-template-areas': decksLayout.map((row) => `"${row}"`).join(' '),
    'grid-row-gap': '10px',
  }
})
const players = computed(() => props.game.investigators)
const playerOrder = computed(() => props.game.playerOrder)
const discards = computed<Card[]>(() => props.scenario.discard.map(c => ({ tag: 'EncounterCard', contents: c })))
const outOfPlayEnemies = computed(() => Object.values(props.game.enemies).filter(e => e.placement.tag === 'OutOfPlay'))
const outOfPlay = computed(() => props.scenario?.setAsideCards || [])
const removedFromPlay = computed(() => props.game.removedFromPlay)
const noCards = computed<Card[]>(() => [])
const viewUnderScenarioReference = computed(() => `${cardsUnderScenarioReference.value.length} Cards Underneath`)
const viewDiscardLabel = computed(() => pluralize(t('scenario.discardCard'), discards.value.length))
const topOfEncounterDiscard = computed(() => {
  if (!props.scenario.discard[0]) return null
  const { cardCode } = props.scenario.discard[0]
  return imgsrc(`cards/${cardCode.replace('c', '')}.avif`)
})
const spectralEncounterDeck = computed(() => props.scenario.encounterDecks['SpectralEncounterDeck']?.[0])
const spectralDiscard = computed(() => props.scenario.encounterDecks['SpectralEncounterDeck']?.[1])
const topOfSpectralDiscard = computed(() => {
  if (!spectralDiscard.value || !spectralDiscard.value[0]) return null
  const { cardCode } = spectralDiscard.value[0]
  return imgsrc(`cards/${cardCode.replace('c', '')}.avif`)
})
const topEnemyInVoid = computed(() => {
  const inVoidEnemy = Object.values(props.game.enemies).filter((e) => e.placement.tag === 'OutOfPlay' && (['VoidZone', 'TheDepths'] as string[]).includes(e.placement.contents))[0]
  return inVoidEnemy
})
const activePlayerId = computed(() => props.game.activeInvestigatorId)
const pursuit = computed(() => Object.values(outOfPlayEnemies.value).filter((enemy) =>
  enemy.placement.tag === 'OutOfPlay' && enemy.placement.contents === 'PursuitZone'
))
const globalEnemies = computed(() => Object.values(props.game.enemies).filter((enemy) =>
  enemy.placement.tag === "OtherPlacement" && enemy.placement.contents === "Global" && enemy.asSelfLocation === null
))
const globalStories = computed(() => Object.values(props.game.stories).filter((story) =>
  story.placement.tag === "OtherPlacement" && story.placement.contents === "Global"
))
const enemiesAsLocations = computed(() => Object.values(props.game.enemies).filter((enemy) => enemy.asSelfLocation !== null))
const cardsUnderScenarioReference = computed(() => props.scenario.cardsUnderScenarioReference)
const cardsUnderAgenda = computed(() => props.scenario.cardsUnderAgendaDeck)
const cardsUnderAct = computed(() => props.scenario.cardsUnderActDeck)
const cardsNextToAct = computed(() => props.scenario.cardsNextToActDeck)
const cardsNextToAgenda = computed(() => props.scenario.cardsNextToAgendaDeck)
const keys = computed(() => props.scenario.setAsideKeys)
const spentKeys = computed(() => props.scenario.keys)
// TODO: not showing cosmos should be more specific, as there could be a cosmos location in the future?
const locations = computed(() => Object.values(props.game.locations).
  filter((a) => a.inFrontOf === null && a.label !== "cosmos"))
const usedLabels = computed(() => locations.value.map((l) => l.label))
const unusedLabels = computed(() => {
  const { locationLayout, usesGrid } = props.scenario;
  if (!locationLayout || !usesGrid) return []
  return locationLayout.flatMap((row) => row.split(' ')).filter((x) => !usedLabels.value.includes(x) && x !== '.')
})
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const resources = computed(() => props.scenario.tokens[TokenType.Resource])
const hasPool = computed(() => resources.value && resources.value > 0)
const tarotCards = computed(() => props.scenario.tarotCards.filter((c) => c.scope.tag === 'GlobalTarot'))
const phase = computed(() => props.game.phase)
const phaseStep = computed(() => props.game.phaseStep)
const currentDepth = computed(() => props.scenario.counts["CurrentDepth"])
const signOfTheGods = computed(() => props.scenario.counts["SignOfTheGods"])
const gameOver = computed(() => props.game.gameState.tag === "IsOver")

// Reactive
const showCards = reactive<RefWrapper<any>>({ ref: noCards })

// Watchers
watchEffect(() => {
  const oop = outOfPlay.value.length + outOfPlayEnemies.value.length
  if (oop == 0) {
    removeEntry("showOutOfPlay")
    showOutOfPlay.value = false
  } else {
    addEntry({
      id: "showOutOfPlay",
      icon: EyeIcon,
      content: t('gameBar.showOutOfPlay'),
      nested: 'view',
      shortcut: 'o',
      action: () => showOutOfPlay.value = !showOutOfPlay.value
    })
  }
})

watchEffect(() => {
  const isOutOfPlaySource = (source: Source) => {
    switch (source.tag) {
      case "TreacherySource": {
       return outOfPlayEnemies.value.some((e) => {
          if (source.contents) return e.treacheries.includes(source.contents)
          return false
       })
      }
      default: return false
    }
  }
  const isOutOfPlayChoice = (c: Message) => {
    if (c.tag !== "AbilityLabel") return false
    return isOutOfPlaySource(c.ability.source)
  }
  const needsShowOutOfPlay = choices.value.some(isOutOfPlayChoice)
  forcedShowOutOfPlay.value = needsShowOutOfPlay
})


// Helpers
function rotateImages(init: boolean) {
  const atlachNacha = document.querySelector('[data-label=atlachNacha]')
  const locationCards = document.querySelector('.location-cards')
  if (atlachNacha && locationCards) {
    needsInit.value = false
    const inLocation = locationCards.querySelector('[data-label=atlachNacha]')

    if (inLocation) {
      ["legs1", "legs2", "legs3", "legs4"].forEach((legs) =>  {
        const legsDiv = locationCards.querySelector(`[data-label=${legs}]`)
        if (!legsDiv) {
          legsSet.value = legsSet.value.filter(item => item !== legs);

          const newDiv = document.createElement('div');
          newDiv.setAttribute('data-label', legs); // Setting the data-label attribute
          newDiv.style.width = '60px'; // Setting the width of the div
          newDiv.style.height = '84px'; // Setting the width of the div
          newDiv.style.gridArea = legs; // Assuming 'legs1' is a valid grid-area name

          locationCards.appendChild(newDiv); // Append the new div to the parent container
        }
      })
    }

    const degrees = parseFloat(atlachNacha.dataset.rotation) || 0
    const middleCardImg = atlachNacha.querySelector('img')
    const middleCardRect = atlachNacha.getBoundingClientRect()
    const middleCardImgRect = middleCardImg.getBoundingClientRect()
    const originX = middleCardImgRect.left + middleCardImgRect.width / 2 - middleCardRect.left
    const originY = middleCardImgRect.top + middleCardImgRect.height / 2 - middleCardRect.top

    if (init) atlachNacha.style.transformOrigin = `${originX}px ${originY}px`
    atlachNacha.style.transition = 'none'
    atlachNacha.style.transform = `rotate(${previousRotation.value}deg)`
    const oX = middleCardImgRect.left + middleCardImgRect.width / 2
    const oY = middleCardImgRect.top + middleCardImgRect.height / 2

    document.querySelectorAll('[data-label=legs1],[data-label=legs2],[data-label=legs3],[data-label=legs4]').forEach((img) => {

      if (init || !legsSet.value.includes(img.dataset.label)) {
        if(!legsSet.value.includes(img.dataset.label)) {
          legsSet.value = [...legsSet.value, img.dataset.label]
        }
        const thisRect = img.getBoundingClientRect()
        const thisX = thisRect.left
        const thisY = thisRect.top
        img.style.transformOrigin = `${oX - thisX}px ${oY - thisY}px`
      }
      img.style.transition = 'none'
      img.style.transform = `rotate(${previousRotation.value}deg)`
    });
    if (degrees !== previousRotation.value) {
      previousRotation.value = degrees
      requestAnimationFrame(() => {
        requestAnimationFrame(() => {
          atlachNacha.style.transform = `rotate(${previousRotation.value}deg)`
          atlachNacha.style.transition = 'transform 0.5s'
          document.querySelectorAll('[data-label=legs1],[data-label=legs2],[data-label=legs3],[data-label=legs4]').forEach((img) => {
            img.style.transition = 'transform 0.5s'
            img.style.transform = `rotate(${degrees}deg)`
          })
        })
      })
    }
  }
}

function beforeLeave(e: Element) {
  const el = e as HTMLElement
  const {marginLeft, marginTop, width, height} = window.getComputedStyle(el)

  el.style.left = `${el.offsetLeft - parseFloat(marginLeft)}px`
  el.style.top = `${el.offsetTop - parseFloat(marginTop)}px`
  el.style.width = width
  el.style.height = height
}
const doShowCards = (cards: ComputedRef<Card[]>, title: string, isDiscards: boolean) => {
  cardRowTitle.value = title
  showCards.ref = cards
  viewingDiscard.value = isDiscards
}
const showRemovedFromPlay = () => doShowCards(removedFromPlay, t('scenario.removedFromPlay'), true)
const showDiscards = () => doShowCards(discards, t('scenario.discards'), true)
const hideCards = () => showCards.ref = noCards
const showCardsUnderScenarioReference = () => doShowCards(cardsUnderScenarioReference, t('scenario.cardsUnderScenarioReference'), false)
const unusedCanInteract = (u: string) => choices.value.findIndex((c) =>
  c.tag === "GridLabel" && c.gridLabel === u
)
const tarotCardAbility = (card: TarotCard) => {
  return choices.value.findIndex((c) => {
    if (c.tag === "AbilityLabel") {
      return c.ability.source.sourceTag === "TarotSource" && c.ability.source.contents.arcana === card.arcana
    }

    return false
  })
}

const victoryDisplay = computed(() => props.scenario.victoryDisplay)

const showVictoryDisplay = () => doShowCards(victoryDisplay, t('scenario.victoryDisplay'), true)

</script>

<template>
  <div v-if="upgradeDeck" id="game" class="game">
    <UpgradeDeck :game="game" :key="playerId" :playerId="playerId" @choose="choose"/>
  </div>
  <div v-else-if="!gameOver" id="scenario" class="scenario" :data-scenario="scenario.id">
    <div class="scenario-body" :class="{'split-view': splitView }">
      <Draggable v-if="showOutOfPlay || forcedShowOutOfPlay">
        <template #handle><header><h2>{{ $t('gameBar.outOfPlay') }}</h2></header></template>
        <div class="card-row-cards">
          <div v-for="card in outOfPlay" :key="card.id" class="card-row-card">
            <CardView :game="game" :card="card" :playerId="playerId" @choose="$emit('choose', $event)" />
          </div>
          <Enemy
            v-for="enemy in outOfPlayEnemies"
            :key="enemy.id"
            :enemy="enemy"
            :game="game"
            :playerId="playerId"
            @choose="choose"
          />
        </div>
        <button v-if="!forcedShowOutOfPlay" class="close button" @click="showOutOfPlay = false">{{$t('close')}}</button>
      </Draggable>
      <Draggable v-if="showChaosBag">
        <template #handle><header><h2>{{$t('gameBar.chaosBag')}}</h2></header></template>
        <ChaosBag :game="game" :skillTest="null" :chaosBag="scenario.chaosBag" :playerId="playerId" @choose="choose" />
        <div v-if="debug.active" class="buttons buttons-row">
          <button class="button blessed" @click="debug.send(game.id, {tag: 'AddChaosToken', contents: 'BlessToken'})">{{$t('gameBar.add')}} <span class="bless-icon"></span></button>
          <button class="button cursed" @click="debug.send(game.id, {tag: 'AddChaosToken', contents: 'CurseToken'})">{{$t('gameBar.add')}} <span class="curse-icon"></span></button>
          <button class="button frost" @click="debug.send(game.id, {tag: 'AddChaosToken', contents: 'FrostToken'})">{{$t('gameBar.add')}} <span class="frost-icon"></span></button>
        </div>
        <button class="button" @click="showChaosBag = false">{{$t('close')}}</button>
      </Draggable>
      <CardRow
        v-if="showCards.ref.length > 0"
        :game="game"
        :cards="showCards.ref"
        :isDiscards="viewingDiscard"
        :title="cardRowTitle"
        :playerId="playerId"
        @choose="choose"
        @close="hideCards"
      />
      <div class="scenario-cards">
        <div v-if="tarotCards.length > 0" class="tarot-cards">
          <div
            v-for="tarotCard in tarotCards"
            :key="tarotCard.arcana"
            class="tarot-card-container"
            :class="{ [tarotCard.facing]: true, 'can-interact': tarotCardAbility(tarotCard) !== -1 }"
            @click="choose(tarotCardAbility(tarotCard))"
          >
            <img :src="imgsrc(`tarot/${tarotCardImage(tarotCard)}`)" :class="tarotCard.facing" class="card tarot-card" />
          </div>
        </div>
        <div v-if="topEnemyInVoid">
          <Enemy
            :enemy="topEnemyInVoid"
            :game="game"
            :playerId="playerId"
            @choose="choose"
          />
        </div>
        <ScenarioDeck
          :deck="scenarioDeck"
          :key="scenarioDeck[0]"
          v-for="[,scenarioDeck] in scenarioDecks"
        />
        <VictoryDisplay :game="game" :victoryDisplay="victoryDisplay" @show="showVictoryDisplay" @choose="choose" :playerId="playerId" />
        <div class="scenario-encounter-decks">

          <div v-if="topOfEncounterDiscard" class="discard" style="grid-area: encounterDiscard">
            <div class="discard-card">
              <img
                :src="topOfEncounterDiscard"
                class="card"
              />
            </div>


            <div v-if="discards.length > 0" class="buttons">
              <button v-if="discards.length > 0" class="view-discard-button" @click="showDiscards">{{viewDiscardLabel}}</button>
              <template v-if="debug.active">
                <button @click="debug.send(game.id, {tag: 'ShuffleEncounterDiscardBackIn'})">Shuffle Back In</button>
              </template>
            </div>
          </div>

          <EncounterDeck
            :game="game"
            :playerId="playerId"
            @choose="choose"
            style="grid-area: encounterDeck"
            v-if="props.scenario.hasEncounterDeck"
          />

          <div v-if="topOfSpectralDiscard" class="discard" style="grid-area: spectralDiscard"
            >
            <img
              :src="topOfSpectralDiscard"
              class="card"
            />
          </div>


          <EncounterDeck
            v-if="spectralEncounterDeck"
            :spectral="spectralEncounterDeck.length"
            :game="game"
            :playerId="playerId"
            @choose="choose"
            style="grid-area: spectralDeck"
          />
        </div>

        <div class="scenario-decks" :style="scenarioDeckStyles">
          <Agenda
            v-for="(agenda, key) in game.agendas"
            :key="key"
            :agenda="agenda"
            :cardsUnder="cardsUnderAgenda"
            :cardsNextTo="cardsNextToAgenda"
            :game="game"
            :playerId="playerId"
            :style="{ 'grid-area': `agenda${agenda.deckId}`, 'justify-self': 'center' }"
            @choose="choose"
            @show="doShowCards"
          />

          <Act
            v-for="(act, key) in game.acts"
            :key="key"
            :act="act"
            :cardsUnder="cardsUnderAct"
            :cardsNextTo="cardsNextToAct"
            :game="game"
            :playerId="playerId"
            :style="{ 'grid-area': `act${act.deckId}`, 'justify-self': 'center' }"
            @choose="choose"
            @show="doShowCards"
          />
        </div>

        <Enemy
          v-for="enemy in pursuit"
          :key="enemy.id"
          :enemy="enemy"
          :game="game"
          :playerId="playerId"
          @choose="choose"
        />

        <Enemy
          v-for="enemy in globalEnemies"
          :key="enemy.id"
          :enemy="enemy"
          :game="game"
          :playerId="playerId"
          @choose="choose"
        />

        <Story
          v-for="story in globalStories"
          :key="story.id"
          :story="story"
          :game="game"
          :playerId="playerId"
          @choose="choose"
        />

        <div class="scenario-guide">
          <div class="scenario-guide-card">
            <img
              class="card"
              :src="scenarioGuide"
              :data-spent-keys="JSON.stringify(spentKeys)"
            />
            <PoolItem class="depth" v-if="currentDepth" type="resource" :amount="currentDepth" />
            <div class="spent-keys" v-if="spentKeys.length > 0">
              <Key v-for="key in spentKeys" :key="key" :name="key" />
            </div>
            <PoolItem class="signOfTheGods" v-if="signOfTheGods" type="resource" :amount="signOfTheGods" />
          </div>
          <div class="pool" v-if="hasPool">
            <PoolItem v-if="resources && resources > 0" type="resource" :amount="resources" />
          </div>
          <div class="keys" v-if="keys.length > 0">
            <Key v-for="key in keys" :key="key" :name="key" />
          </div>
          <button v-if="cardsUnderScenarioReference.length > 0" class="view-cards-under-button" @click="showCardsUnderScenarioReference">{{viewUnderScenarioReference}}</button>
        </div>
        <SkillTest
            v-if="game.skillTest"
            :game="game"
            :chaosBag="scenario.chaosBag"
            :skillTest="game.skillTest"
            :playerId="playerId"
            @choose="choose"
        >
        </SkillTest>

        <button v-if="removedFromPlay.length > 0" class="view-removed-from-play-button" @click="showRemovedFromPlay"><font-awesome-icon icon="eye" /> Removed from Play</button>
      </div>


      <div class="location-cards-container">
        <Connections :game="game" :playerId="playerId" />
        <input v-model="locationsZoom" type="range" min="1" max="3" step="0.25" class="zoomer" />
        <transition-group name="map" tag="div" ref="locationMap" class="location-cards" :style="locationStyles" @before-leave="beforeLeave">
          <Location
            v-for="location in locations"
            class="location"
            :key="location.label"
            :game="game"
            :playerId="playerId"
            :location="location"
            :style="{ 'grid-area': location.label, 'justify-self': 'center' }"
            @choose="choose"
          />
          <Enemy
            v-for="enemy in enemiesAsLocations"
            :key="enemy.id"
            :enemy="enemy"
            :game="game"
            :playerId="playerId"
            :data-label="enemy.asSelfLocation"
            :data-rotation="enemy.meta?.rotation ?? null"
            :style="{ 'grid-area': enemy.asSelfLocation, 'justify-self': 'center', 'align-items': 'center' }"
            @choose="choose"
          />

          <template v-if="barriers">
            <div v-for="[area, amount] in Object.entries(barriers)" :key="area" class="barrier" :class="{ vertical: isVertical(area) }" :style="{ 'grid-area': `barrier-${area}` }">
              <img v-for="n in amount" :key="n" :src="imgsrc('resource.png')" />
              <button v-if="debug.active && (amount as number > 0)" @click="debug.send(game.id, {tag: 'ScenarioCountDecrementBy', contents: [{ 'tag': 'Barriers', 'contents': area.split('--') }, 1]})">x</button>
            </div>
          </template>

          <template v-if="scenario.usesGrid">
            <template v-for="u in unusedLabels" :key="u">
              <div
                v-if="unusedCanInteract(u) !== -1"
                class="empty-grid-position card"
                :class="{ 'can-interact': unusedCanInteract(u) !== -1}"
                :style="{ 'grid-area': u}"
                @click="choose(unusedCanInteract(u))"
                >
              </div>
            </template>
          </template>
        </transition-group>
      </div>

      <PlayerTabs
        :game="game"
        :playerId="playerId"
        :players="players"
        :playerOrder="playerOrder"
        :activePlayerId="activePlayerId"
        :tarotCards="props.scenario.tarotCards"
        @choose="choose"
      />
    </div>
    <div class="phases">
      <div class="phase" :class="{ 'active-phase': phase == 'MythosPhase' }">
        <div class="subphases">
          <div v-tooltip.left="$t('phase.mythosPhaseBeginsStep')" :class="{'current': phaseStep?.contents === 'MythosPhaseBeginsStep' }">1.1</div>
          <div v-tooltip.left="$t('phase.placeDoomOnAgendaStep')" :class="{'current': phaseStep?.contents === 'PlaceDoomOnAgendaStep'}">1.2</div>
          <div v-tooltip.left="$t('phase.checkDoomThresholdStep')" :class="{'current': phaseStep?.contents === 'CheckDoomThresholdStep'}">1.3</div>
          <div v-tooltip.left="$t('phase.eachInvestigatorDrawsEncounterCardStep')" :class="{'current': phaseStep?.contents === 'EachInvestigatorDrawsEncounterCardStep'}">1.4</div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'MythosPhaseWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.mythosPhaseEndsStep')" :class="{'current': phaseStep?.contents === 'MythosPhaseEndsStep'}">1.5</div>
        </div>
        <div>{{$t('phase.mythosPhase')}}</div>
      </div>
      <div class="phase" :class="{ 'active-phase': phase == 'InvestigationPhase' }">
        <div class="subphases">
          <div v-tooltip.left="$t('phase.investigationPhaseBeginsStep')" :class="{'current': phaseStep?.contents === 'InvestigationPhaseBeginsStep'}">2.1</div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'InvestigationPhaseBeginsWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.nextInvestigatorsTurnBeginsStep')" :class="{'current': phaseStep?.contents === 'NextInvestigatorsTurnBeginsStep'}">2.2</div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'NextInvestigatorsTurnBeginsWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.investigatorTakesActionStep')" :class="{'current': phaseStep?.contents === 'InvestigatorTakesActionStep'}">2.2.1</div>
          <div v-tooltip.left="$t('phase.investigatorsTurnEndsStep')" :class="{'current': phaseStep?.contents === 'InvestigatorsTurnEndsStep'}">2.2.2</div>
          <div v-tooltip.left="$t('phase.investigationPhaseEndsStep')" :class="{'current': phaseStep?.contents === 'InvestigationPhaseEndsStep'}">2.3</div>
        </div>
        <div>{{$t('phase.investigationPhase')}}</div>
      </div>
      <div class="phase" :class="{ 'active-phase': phase == 'EnemyPhase' }">
        <div class="subphases">
          <div v-tooltip.left="$t('phase.enemyPhaseBeginsStep')" :class="{'current': phaseStep?.contents === 'EnemyPhaseBeginsStep'}">3.1</div>
          <div v-tooltip.left="$t('phase.hunterEnemiesMoveStep')" :class="{'current': phaseStep?.contents === 'HunterEnemiesMoveStep'}">3.2</div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'ResolveAttacksWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.resolveAttacksStep')" :class="{'current': phaseStep?.contents === 'ResolveAttacksStep'}">3.3</div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'AfterResolveAttacksWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.enemyPhaseEndsStep')" :class="{'current': phaseStep?.contents === 'EnemyPhaseEndsStep'}">3.4</div>
        </div>  
        <div>{{$t('phase.enemyPhase')}}</div>
      </div>
      <div class="phase" :class="{ 'active-phase': phase == 'UpkeepPhase' }">
        <div class="subphases">
          <div v-tooltip.left="$t('phase.upkeepPhaseBeginsStep')" :class="{'current': phaseStep?.contents === 'UpkeepPhaseBeginsStep'}">4.1</div>
          <div v-tooltip.left="$t('phase.playerWindow')" :class="{'current': phaseStep?.contents === 'UpkeepPhaseBeginsWindow'}"><i class="fast-icon" /></div>
          <div v-tooltip.left="$t('phase.resetActionsStep')" :class="{'current': phaseStep?.contents === 'ResetActionsStep'}">4.2</div>
          <div v-tooltip.left="$t('phase.readyExhaustedStep')" :class="{'current': phaseStep?.contents === 'ReadyExhaustedStep'}">4.3</div>
          <div v-tooltip.left="$t('phase.drawCardAndGainResourceStep')" :class="{'current': phaseStep?.contents === 'DrawCardAndGainResourceStep'}">4.4</div>
          <div v-tooltip.left="$t('phase.checkHandSizeStep')" :class="{'current': phaseStep?.contents === 'CheckHandSizeStep'}">4.5</div>
          <div v-tooltip.left="$t('phase.upkeepPhaseEndsStep')" :class="{'current': phaseStep?.contents === 'UpkeepPhaseEndsStep'}">4.6</div>
        </div>
        <div>{{$t('phase.upkeepPhase')}}</div>
      </div>
    </div>
  </div>
</template>

<style scoped lang="scss">
.card {
  border-radius: 5px;
  width: var(--card-width);
  height: auto;
  aspect-ratio: var(--card-aspect);
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}

.card--sideways {
  width: auto;
  height: calc(var(--card-width) * 2);
  aspect-ratio: var(--card-sideways-ratio);
}

.scenario-cards {
  display: flex;
  align-self: center;
  align-items: flex-start;
  justify-content: center;
  padding: 10px 0;
  position: relative;
  width: 100%;
  gap: 10px;
  z-index: -2;
}

.clue {
  position: relative;
  width: 57px;
  height: 54px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: black;
  font-weight: 900;
  font-size: 1.5em;

  img {
    position: absolute;
    top: 0;
    bottom: 0;
    left: 0;
    right: 0;
    margin: auto;
    z-index: -1;
  }
}

@mixin splitView {
    grid-template-columns: 1fr 2fr;
    grid-template-rows: 1fr 3fr;
    padding-bottom: 10px;
    row-gap: 30px;

    &:deep(.player-info) {
      grid-column: 1;
      grid-row: 2 / 5;
      display: flex;
      flex-direction: column;

      .tab {
        display: flex;
        flex-direction: column;
        flex: 1;
        border-top-right-radius: 10px;
        overflow: hidden;
      }

      .player-cards {
        overflow: auto;
        display: flex;
        flex-direction: column;
        flex: 1;
        border-top-right-radius: 10px;
      }

      .player {
        display: flex;
        flex-direction: column;
        gap: 10px;
        width: 100%;
        flex: 1;
      }
    }

    .scenario-cards {
      grid-column: 1;
      grid-row: 1 / 2;
      flex-wrap: wrap;
    }

    .location-cards-container {
      grid-column: 2;
      grid-row: 1 / 5;
    }
}

.scenario-body {
  display: flex;
  flex-direction: column;
  background: var(--background);
  z-index: 1;
  width: 100%;
  flex: 1;
  inset: 0;

  display: grid;
  grid-template-rows: auto 1fr auto;

  &.split-view {
    @include splitView;
  }
}

.location-cards {
  display: flex;
  width: 100%;
  height: 100%;
  margin: auto;
  overflow: auto;
  scrollbar-gutter: stable both-edges;
  place-content: safe center;
}

.location-cards-container {
  display: flex;
  overflow: hidden;
  flex: 1;
  padding-top: 32px;
  padding-bottom: 32px;
  position: relative;
}

.portrait {
  border-radius: 3px;
}

.portrait--can-move {
  cursor: pointer;
  border: 3px solid var(--select);
}

.location--can-move-to {
  border: 3px solid var(--select);
  cursor: pointer;
}

.agenda-container, .act-container {
  align-self: flex-start;
}

.discard {
  height: 100%;
  position: relative;
  display: flex;
  flex-direction: column;
  gap: 5px;
  &:deep(.card) {
    margin: 0;
    box-shadow: none;
  }
  .buttons {
    display: flex;
    flex-direction: column;
    gap: 5px;
  }
}

.discard-card {
  position: relative;
  width: fit-content;
  line-height: 0;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  .card {
    box-shadow: unset;
  }
  &::after {
    border-radius: 6px;
    pointer-events: none;
    content: "";
    position: absolute;
    inset: 0;
    background-color: #FFF;
    opacity: .85;
    mix-blend-mode: saturation;
  }
}


.view-out-of-play-button {
  text-decoration: none;
  position: absolute;
  transform: translate(100%, -50%) rotate(90deg) translate(0%, 50%) translate(0%, 10px);
  svg {
    transform: rotate(-90deg)
  }
  transform-origin: center left;
  top: 0px;
  right: 0px;
  border: 0;
  color: white;
  background: var(--background-mid);
  font-size: 1.2em;
  padding: 5px 15px;
}

.view-removed-from-play-button {
  text-decoration: none;
  position: absolute;
  transform: translate(100%, -50%) rotate(90deg) translate(0%, 50%) translate(0%, 50px);
  svg {
    transform: rotate(-90deg)
  }
  transform-origin: center left;
  top: 0px;
  right: 0px;
  border: 0;
  color: white;
  background: #a5b5bc;
  font-size: 1.2em;
  padding: 5px 15px;
}

.phases {
  display: flex;
  align-items: flex-end;
  writing-mode: vertical-rl;
  text-orientation: mixed;
  justify-content: space-around;
  background-color: #b8c1c6;
  text-transform: uppercase;
  font-family: Arial;
  > div {
    flex: 1;
    text-align: center;
  }
}


.phase {
  display: flex;
  flex-direction: column;
  width: 100%;
}

.subphases {
  position: relative;
  font-size: 0.7em;
  flex: 1;
  writing-mode: lr-tb;
  text-orientation: revert;
  display: flex;
  min-width: min-content;
  flex-direction: column;
  height: 100%;
  justify-content: space-around;
  color: #b8c1c6;
  background: #484E51;
  text-transform: uppercase;
  font-family: Arial;
  .current {
    background: rgba(0, 0, 0, 0.5) !important;
  }
  > div {
    width: 100%;
    padding: 0 5px;
    display: flex;
    justify-content: center;
    flex: 1;
    align-items: center;
    &:hover {
      background: rgba(0, 0, 0, 0.5);
    }
  }
  > div:nth-of-type(2n) {
    background: #5a6062;
    &:hover {
      background: rgba(0, 0, 0, 0.5);
    }
  }
}

.scenario {
  display: flex;
  user-select: none;
  width: 100%;
  height: 100%;
  flex: 1;
}

.active-phase {
  font-weight: bold;
  background-color: #8e9ca4;
}

.scenario-guide {
  display: flex;
  flex-direction: column;
  position: relative;

  .depth {
    position: absolute;
    bottom: 0;
    right: 0;
    pointer-events: none;
  }

  .signOfTheGods {
    position: absolute;
    bottom: 0;
    right: 0;
    pointer-events: none;
  }
}

.map-move {
  transition: all 0.6s cubic-bezier(0.23, 1, 0.32, 1);
}

.map-leave-to {
  opacity: 0;
}

.map-leave-active {
  position: absolute;
}

.scenario-decks {
  gap: 5px;
}

.scenario-encounter-decks {
  display: grid;
  grid-template: "encounterDiscard encounterDeck" "spectralDiscard spectralDeck";
  gap: 10px;
}

.empty-grid-position {
  content: " ";
  box-shadow: unset;
}

.can-interact {
  background: rgba(0, 0, 0, 0.5);
  outline: 2px solid var(--select);
  cursor: pointer;
}

.pool {
  position: absolute;
  top: 10%;
  align-items: center;
  display: flex;
  align-self: flex-start;
  align-items: flex-end;
  pointer-events: none;

  * {
    transform: scale(0.6);
  }
}

.tarot-card {
  width: var(--card-width);
  aspect-ratio: var(--card-tarot-aspect);
  margin: 0;
}

.tarot-card-choices {
  background: url("/img/arkham/background.jpg");
  background-position: center;
  background-size: contain;
  position: absolute;
  z-index: 1000;
  margin: auto;
  inset: 0;
  width: fit-content;
  height: fit-content;
  display: flex;
  gap: 10px;
  padding: 10px;
}

.tarot-card-container {
  transition: transform 0.5s ease-in-out;
  display:flex;
  position: relative;
  border-radius: 5px;

  &.Reversed {
    transform: rotateZ(180deg);
    &:before {
      transform-origin: center;
      animation-fill-mode: forwards;
      animation: shadow-rotate 0.5s linear;
      transform: translate(0, -12px);
    }
  }
}

@keyframes shadow-rotate {
  0% {
    transform: translate(0, 12px);
  }
  25% {
    transform: translate(6px, 12px);
  }
  50% {
    transform: translate(12px, 0px);
  }
  75% {
    transform: translate(6px, -12px);
  }
  100% {
    transform: translate(0, -12px);
  }
}

.tarot-cards {
  display: flex;
  gap: 10px;
  margin-inline: 10px;
}

// We lower the margin so things line up a bit better.
[data-scenario='c06333'] .location-cards:deep(.location-container) {
  margin: 20px !important;
}

.buttons-row {
  display: flex;
  flex-direction: row;

  .blessed {
    background-color: var(--blessed);
  }

  .cursed {
    background-color: var(--cursed);
  }

  .frost {
    background-color: var(--frost);
  }

}

.button {
  padding: 5px 10px;
  font-size: 1em;
}

.card-row-cards {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 10px;
  gap: 2px;
  flex-wrap: wrap;

  .card-row-card {
    position: relative;
  }
}

.location {
  &:hover {
    z-index: 100;
  }
}

.button{
  border: 0;
  margin-top: 2px;
  color: #fff;
  cursor: pointer;
  border-radius: 4px;
  background-color: #555;
  z-index: 1000;
  width: 100%;
  min-width: max-content;
}

.keys {
  display: flex;
  flex-direction: row;
  gap: 2px;
}

.scenario-guide-card {
  position: relative;
}

.spent-keys {
  pointer-events: none;
  display: flex;
  flex-direction: row;
  gap: 2px;
  position: absolute;
  bottom: 20px;
  inset-inline: 5px;
  margin-inline: auto;

  &:deep(img) {
    width: 10px;
  }
}

.zoomer {
  position: absolute;
  right: 10px;
  bottom: 10px;
}

.barrier {
  display: flex;
  flex-direction: column;
  width: calc(var(--card-width) / 4);
  height: calc(var(--card-width) / var(--card-aspect));
  align-self: flex-start;
  justify-content: center;

  img {
    width: 20px;
  }

  &.vertical {
    flex-direction: row;
    height: 40px;
    width: 100px;
    justify-self: center;
    img {
      height: 20px;
    }
  }
}
</style>
