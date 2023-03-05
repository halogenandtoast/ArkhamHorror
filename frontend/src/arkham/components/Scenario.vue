<script lang="ts" setup>
import {
  onMounted,
  onBeforeUnmount,
  computed,
  ref,
  ComputedRef,
  reactive,
  inject
} from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Scenario } from '@/arkham/types/Scenario';
import type { Card } from '@/arkham/types/Card';
import Act from '@/arkham/components/Act.vue';
import Agenda from '@/arkham/components/Agenda.vue';
import Enemy from '@/arkham/components/Enemy.vue';
import CardRow from '@/arkham/components/CardRow.vue';
import StatusBar from '@/arkham/components/StatusBar.vue';
import ChaosBag from '@/arkham/components/ChaosBag.vue';
import PlayerTabs from '@/arkham/components/PlayerTabs.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import EncounterDeck from '@/arkham/components/EncounterDeck.vue';
import VictoryDisplay from '@/arkham/components/VictoryDisplay.vue';
import ScenarioDeck from '@/arkham/components/ScenarioDeck.vue';
import Location from '@/arkham/components/Location.vue';

export interface Props {
  game: Game
  scenario: Scenario
  investigatorId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['choose'])

function beforeLeave(el) {
  const {marginLeft, marginTop, width, height} = window.getComputedStyle(el)

  el.style.left = `${el.offsetLeft - parseFloat(marginLeft, 10)}px`
  el.style.top = `${el.offsetTop - parseFloat(marginTop, 10)}px`
  el.style.width = width
  el.style.height = height
}

async function choose(idx: number) {
  emit('choose', idx)
}

function handleConnections() {
  const toConnection = (div1: HTMLElement, div2: HTMLElement) => {
    const [leftDiv, rightDiv] = [div1, div2].sort((a, b) => {
      const { id: div1Id } = a.dataset
      const { id: div2Id } = b.dataset

      if (div1Id < div2Id) {
        return -1;
      }
      if (div1Id > div2Id) {
        return 1;
      }

      // names must be equal
      return 0;
    })

    const { id: leftDivId } = leftDiv.dataset
    const { id: rightDivId } = rightDiv.dataset

    if (leftDivId && rightDivId) {
      return leftDivId + ":" + rightDivId
    }
  }
  const makeLine = function(div1: HTMLElement, div2: HTMLElement) {
    const [leftDiv, rightDiv] = [div1, div2].sort((a, b) => {
      const { id: div1Id } = a.dataset
      const { id: div2Id } = b.dataset

      if (div1Id < div2Id) {
        return -1;
      }
      if (div1Id > div2Id) {
        return 1;
      }

      // names must be equal
      return 0;
    })

    const closeEnough = (a, b) => {
      return diff(a, b) < 0.5
    }

    const diff = (a, b) => {
      const x = parseFloat(a)
      const y = parseFloat(b)

      return Math.abs(x - y)
    }

    const { id: leftDivId } = leftDiv.dataset
    const { id: rightDivId } = rightDiv.dataset

    if (leftDivId && rightDivId) {
      const line = document.querySelector<HTMLElement>(".line")
      const parentNode = line?.parentNode

      if(line && parentNode) {
        const connection = leftDivId + ":" + rightDivId
        const {left: bodyLeft, top: bodyTop} = document.body.getBoundingClientRect()
        const {left: leftDivLeft, top: leftDivTop, right: leftDivRight, bottom: leftDivBottom } = leftDiv.getBoundingClientRect();
        const {left: rightDivLeft, top: rightDivTop, right: rightDivRight, bottom: rightDivBottom } = rightDiv.getBoundingClientRect();
        const leftDivWidth = leftDivRight - leftDivLeft;
        const rightDivWidth = rightDivRight - rightDivLeft;
        const leftDivHeight = leftDivBottom - leftDivTop;
        const rightDivHeight = rightDivBottom - rightDivTop;
        const x1 = (leftDivLeft - bodyLeft) + (leftDivWidth/2)
        const y1 = (leftDivTop - bodyTop) + (leftDivHeight/2)
        const x2 = (rightDivLeft - bodyLeft) + (rightDivWidth/2)
        const y2 = (rightDivTop - bodyTop) + (rightDivHeight/2)
        const existingNode = document.querySelector(`[data-connection="${connection}"]`)

        if (existingNode) {
          if (closeEnough(existingNode.getAttribute("x1"), x1) && closeEnough(existingNode.getAttribute("y1"), y1) && closeEnough(existingNode.getAttribute("x2"), x2) && closeEnough(existingNode.getAttribute("y2"), y2)) {
            return
          } else {
            parentNode.removeChild(existingNode);
          }
        }

        const node = line.cloneNode(true) as HTMLElement
        node.dataset.connection = connection
        node.classList.remove("original")

        const investigator = props.game.investigators[props.investigatorId]
        const { connectedLocations } = investigator
        const activeLine = (leftDivId == investigator.location && connectedLocations.includes(rightDivId)) || (rightDivId == investigator.location && connectedLocations.includes(leftDivId))
        if (activeLine) {
          node.classList.add("active")
        }
        parentNode.insertBefore(node, line.nextSibling)

        node.setAttribute('x1',x1.toString())
        node.setAttribute('y1',y1.toString())
        node.setAttribute('x2',x2.toString())
        node.setAttribute('y2',y2.toString())
      }
    }
  }

  const allConnections = []
  for(const [id,location] of Object.entries(props.game.locations)) {
    const connections = typeof location.connectedLocations == "object" ? Object.values(location.connectedLocations) : location.connectedLocations
    connections.forEach((connection) => {
      const start = document.querySelector(`[data-id="${id}"]`) as HTMLElement
      const end = document.querySelector(`[data-id="${connection}"]`) as HTMLElement
      if(start && end) {
        allConnections.push(toConnection(start, end))
        makeLine(start, end)
      }
    });
  }

  document.querySelectorAll(".line:not(.original").forEach((node) => {
    const con = node.dataset.connection
    if(con) {
      if(!allConnections.some((c) => c === con)) {
        node.parentNode?.removeChild(node)
      }
    }
  })

}

interface RefWrapper<T> {
  ref: ComputedRef<T>
}

const baseUrl = inject('baseUrl')
const locationMap = ref<Element | null>(null)

const requestId = ref(undefined)
const drawHandler = () => {
  requestId.value = undefined
  handleConnections()
  requestId.value = window.requestAnimationFrame(drawHandler);
}

onBeforeUnmount(() => window.cancelAnimationFrame(requestId.value))

onMounted(() => requestId.value = window.requestAnimationFrame(drawHandler))

const scenarioGuide = computed(() => {
  const { id, difficulty } = props.scenario;
  const difficultySuffix = difficulty === 'Hard' || difficulty === 'Expert'
    ? 'b'
    : '';

  return `${baseUrl}/img/arkham/cards/${id.replace('c', '')}${difficultySuffix}.jpg`;
})

const scenarioDecks = computed(() => {
  if (!props.scenario.decks) {
    return null;
  }

  return Object.entries(props.scenario.decks);

})

const locationStyles = computed(() => {
  const { locationLayout } = props.scenario;
  if (locationLayout) {
    return {
      display: 'grid',
      'grid-template-areas': locationLayout.map((row) => `"${row}"`).join(' '),
      'grid-row-gap': '10px',
    };
  }
  return null;
})

const scenarioDeckStyles = computed(() => {
  const { decksLayout } = props.scenario
  return {
    display: 'grid',
    'grid-template-areas': decksLayout.map((row) => `"${row}"`).join(' '),
    'grid-row-gap': '10px',
  }
})

const activeCard = computed(() => {
  if (props.game.activeCard) {
    const { cardCode } = props.game.activeCard.contents;
    return `${baseUrl}/img/arkham/cards/${cardCode.replace('c', '')}.jpg`;
  }

  return null;
})

const players = computed(() => props.game.investigators)
const playerOrder = computed(() => props.game.playerOrder)
const discards = computed<Card[]>(() => props.scenario.discard.map(c => { return { tag: 'EncounterCard', contents: c }}))
const outOfPlay = computed(() => (props.scenario?.setAsideCards || []))
const removedFromPlay = computed(() => props.game.removedFromPlay)
const noCards = computed<Card[]>(() => [])

// eslint-disable-next-line
const showCards = reactive<RefWrapper<any>>({ ref: noCards })
const viewingDiscard = ref(false)
const cardRowTitle = ref("")


const doShowCards = (event: Event, cards: ComputedRef<Card[]>, title: string, isDiscards: boolean) => {
  cardRowTitle.value = title
  showCards.ref = cards
  viewingDiscard.value = isDiscards
}

const showOutOfPlay = (e: Event) => doShowCards(e, outOfPlay, 'Out of Play', true)
const showRemovedFromPlay = (e: Event) => doShowCards(e, removedFromPlay, 'Removed from Play', true)
const showDiscards = (e: Event) => doShowCards(e, discards, 'Discards', true)
const hideCards = () => showCards.ref = noCards

const viewDiscardLabel = computed(() => `${discards.value.length} Cards`)
const topOfEncounterDiscard = computed(() => {
  if (props.scenario.discard[0]) {
    const { cardCode } = props.scenario.discard[0];

    return `${baseUrl}/img/arkham/cards/${cardCode.replace('c', '')}.jpg`;
  }

  return null;
})

const topEnemyInVoid = computed(() => Object.values(props.game.enemiesInVoid)[0])
const activePlayerId = computed(() => props.game.activeInvestigatorId)

const pursuit = computed(() => Object.values(props.game.outOfPlayEnemies).filter((enemy) => enemy.placement?.tag === "Pursuit"))

const enemiesAsLocations = computed(() => Object.values(props.game.enemies).filter((enemy) => enemy.asSelfLocation !== null))

const cardsUnderAgenda = computed(() => {
  return props.scenario.cardsUnderAgendaDeck
})

const cardsUnderAct = computed(() => {
  return props.scenario.cardsUnderActDeck
})

const cardsNextToAct = computed(() => {
  return props.scenario.cardsNextToActDeck
})

const phase = computed(() => props.game.phase)
const currentDepth = computed(() => props.scenario.counts["CurrentDepth"])
</script>

<template>
  <div v-if="!game.gameOver" id="scenario" class="scenario">
    <div class="scenario-body">
      <StatusBar :game="game" :investigatorId="investigatorId" @choose="choose" />
      <CardRow
        v-if="showCards.ref.length > 0"
        :game="game"
        :cards="showCards.ref"
        :isDiscards="viewingDiscard"
        :title="cardRowTitle"
        :investigatorId="investigatorId"
        @choose="choose"
        @close="hideCards"
      />
      <div class="scenario-cards">
        <div v-if="topEnemyInVoid">
          <Enemy
            :enemy="topEnemyInVoid"
            :game="game"
            :investigatorId="investigatorId"
            @choose="choose"
          />
        </div>
        <ScenarioDeck
          :deck="scenarioDeck"
          :key="scenarioDeck[0]"
          v-for="[,scenarioDeck] in scenarioDecks"
        />
        <VictoryDisplay :game="game" :victoryDisplay="scenario.victoryDisplay" @show="doShowCards" :investigatorId="investigatorId" />
        <div v-if="topOfEncounterDiscard" class="discard">
          <img
            :src="topOfEncounterDiscard"
            class="card"
          />

          <button v-if="discards.length > 0" class="view-discard-button" @click="showDiscards">{{viewDiscardLabel}}</button>
        </div>

        <EncounterDeck
          :game="game"
          :investigatorId="investigatorId"
          @choose="choose"
        />

        <div class="scenario-decks" :style="scenarioDeckStyles">
          <Agenda
            v-for="(agenda, key) in game.agendas"
            :key="key"
            :agenda="agenda"
            :cardsUnder="cardsUnderAgenda"
            :game="game"
            :investigatorId="investigatorId"
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
            :investigatorId="investigatorId"
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
          :investigatorId="investigatorId"
          @choose="choose"
        />

        <div class="scenario-guide">
          <img
            class="card"
            :src="scenarioGuide"
          />
          <PoolItem class="depth" v-if="currentDepth" type="resource" :amount="currentDepth" />
        </div>
        <ChaosBag
          :game="game"
          :chaosBag="scenario.chaosBag"
          :skillTest="game.skillTest"
          :investigatorId="investigatorId"
          @choose="choose"
        />

        <img
          v-if="activeCard"
          :src="activeCard"
          class="card"
        />

        <button v-if="removedFromPlay.length > 0" class="view-removed-from-play-button" @click="showRemovedFromPlay"><font-awesome-icon icon="eye" /> Removed from Play</button>

        <button v-if="outOfPlay.length > 0" class="view-out-of-play-button" @click="showOutOfPlay"><font-awesome-icon icon="eye" /> Out of Play</button>
      </div>

      <svg id="svg">
        <line id="line" class="line original" stroke-dasharray="5, 5"/>
      </svg>

      <transition-group name="map" tag="div" ref="locationMap" class="location-cards" :style="locationStyles" @before-leave="beforeLeave">
        <Location
          v-for="(location, key) in game.locations"
          class="location"
          :key="key"
          :game="game"
          :investigatorId="investigatorId"
          :location="location"
          :style="{ 'grid-area': location.label, 'justify-self': 'center' }"
          @choose="choose"
        />
        <Enemy
          v-for="enemy in enemiesAsLocations"
          :key="enemy.id"
          :enemy="enemy"
          :game="game"
          :investigatorId="investigatorId"
          :style="{ 'grid-area': enemy.asSelfLocation, 'justify-self': 'center' }"
          @choose="choose"
        />
      </transition-group>

      <PlayerTabs
        :game="game"
        :investigatorId="investigatorId"
        :players="players"
        :playerOrder="playerOrder"
        :activePlayerId="activePlayerId"
        @choose="choose"
      />
    </div>
    <div class="phases">
      <div class="phase" :class="{ 'active-phase': phase == 'MythosPhase' }">
        <div class="subphases">
          <div v-tooltip="'Round begins. Mythos phase begins.'">1.1</div>
          <div v-tooltip="'Place 1 doom on the current agenda.'">1.2</div>
          <div v-tooltip="'Check doom threshold.'">1.3</div>
          <div v-tooltip="'Each investigator draws 1 encounter card.'">1.4</div>
          <div v-tooltip="'PLAYER WINDOW'"><i class="fast-icon" /></div>
          <div v-tooltip="'Mythos phase ends.'">1.5</div>
        </div>
        <div>Mythos</div>
      </div>
      <div class="phase" :class="{ 'active-phase': phase == 'InvestigationPhase' }">
        <div class="subphases">
          <div v-tooltip="'Investigation phase begins.'">2.1</div>
          <div v-tooltip="'PLAYER WINDOW'"><i class="fast-icon" /></div>
          <div v-tooltip="'Next investigator\'s turn begins.'">2.2</div>
          <div v-tooltip="'PLAYER WINDOW'"><i class="fast-icon" /></div>
          <div v-tooltip="'Active investigator may take an action, if able. If an action was taken, return to previous player window. If no action was taken, proceed to 2.2.2.'">2.2.1</div>
          <div v-tooltip="'Investigator\'s turn ends. If an investigator has not yet taken a turn this phase, return to 2.2. If each investigator has taken a turn this phase, proceed to 2.3.'">2.2.2</div>
          <div v-tooltip="'Investigation phase ends.'">2.3</div>
        </div>
        <div>Investigation</div>
      </div>
      <div class="phase" :class="{ 'active-phase': phase == 'EnemyPhase' }">
        <div class="subphases">
          <div v-tooltip="'Enemy phase begins.'">3.1</div>
          <div v-tooltip="'Hunter enemies move.'">3.2</div>
          <div v-tooltip="'PLAYER WINDOW'"><i class="fast-icon" /></div>
          <div v-tooltip="'Next investigator resolves engaged enemy attacks. If an investigator has not yet resolved enemy attacks this phase, return to previous player window. After final investigator resolves engaged enemy attacks, proceed to next player window.'">3.3</div>
          <div v-tooltip="'PLAYER WINDOW'"><i class="fast-icon" /></div>
          <div v-tooltip="'Enemy phase ends.'">3.4</div>
        </div>
        <div>Enemy</div>
      </div>
      <div class="phase" :class="{ 'active-phase': phase == 'UpkeepPhase' }">
        <div class="subphases">
          <div v-tooltip="'Upkeep phase begins.'">4.1</div>
          <div v-tooltip="'PLAYER WINDOW'"><i class="fast-icon" /></div>
          <div v-tooltip="'Reset actions.'">4.2</div>
          <div v-tooltip="'Ready each exhausted card.'">4.3</div>
          <div v-tooltip="'Each investigator draws 1 card and gains 1 resource.'">4.4</div>
          <div v-tooltip="'Each investigator checks hand size.'">4.5</div>
          <div v-tooltip="'Upkeep phase ends. Round ends.'">4.6</div>
        </div>
        <div>Upkeep</div>
      </div>
    </div>
  </div>
</template>

<style scoped lang="scss">
.card {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  width: $card-width;
}

.card--sideways {
  width: auto;
  height: $card-width * 2;
}

.scenario-cards {
  display: flex;
  align-self: center;
  align-items: flex-start;
  justify-content: center;
  padding-bottom: 10px;
  position: relative;
  width: 100%;
  gap: 2px;
}

.clue--can-investigate {
  border: 3px solid #ff00ff;
  border-radius: 100px;
  cursor: pointer;
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

.scenario-body {
  display: flex;
  flex-direction: column;
  background-image: linear-gradient(darken(#E5EAEC, 10), #E5EAEC);

  @media (prefers-color-scheme: dark) {
    background-image: linear-gradient(#282A36, darken(#282A36, 2));
  }

  z-index: 1;
  width: 100%;
  height: calc(100vh - 40px);
  flex: 1;
}

.location-cards {
  display: flex;
  flex: 1;
  justify-content: center;
  align-items: center;
  overflow: auto;
  min-height: 350px;
}

.portrait {
  border-radius: 3px;
}

.portrait--can-move {
  cursor: pointer;
  border: 3px solid $select;
}

.location--can-move-to {
  border: 3px solid $select;
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
  &:deep(.card) {
    margin: 0;
    box-shadow: none;
  }
  &::after {
    border-radius: 6px;
    pointer-events: none;
    content: "";
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: #FFF;
    /* background-image: linear-gradient(120deg, #eaee44, #33d0ff); */
    opacity: .85;
    mix-blend-mode: saturation;
  }
}

#svg {
  pointer-events: none;
  position: absolute;
  isolation: isolate;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  z-index: -100000000;
}

#line{
  stroke-width:6px;
  /* stroke:#a6b5bb; */
  stroke:rgba(0,0,0, 0.2);

  @media (prefers-color-scheme: dark) {
    stroke:rgba(255,255,255, 0.2);
  }
}

.active {
  stroke:rgba(0,0,0,0.5) !important;

  @media (prefers-color-scheme: dark) {
    stroke:rgba(255,255,255, 0.7) !important;
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
  background: #a5b5bc;
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
  font-size: 0.7em;
  flex: 1;
  writing-mode: lr-tb;
  text-orientation: revert;
  display: flex;
  flex-direction: column;
  height: 100%;
  justify-content: space-around;
  color: #b8c1c6;
  background: #484E51;
  text-transform: uppercase;
  font-family: Arial;
  > div {
    width: 100%;
    padding: 0 5px;
    box-sizing: border-box;
    display: flex;
    justify-content: center;
    flex: 1;
    align-items: center;
    &:hover {
      background: rgba(0, 0, 0, 0.5);
    }
  }
  > div:nth-of-type(2n) {
    background: lighten(#484E51, 10%);
    &:hover {
      background: rgba(0, 0, 0, 0.5);
    }
  }
}

.scenario {
  display: flex;
  user-select: none;
  width: 100%;
  flex: 1;
}

.active-phase {
  font-weight: bold;
  background-color: #8e9ca4;
}

.scenario-guide {
  position: relative;
  .depth {
    position: absolute;
    bottom: 0;
    right: 0;
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
  gap: 2px;
}
</style>
