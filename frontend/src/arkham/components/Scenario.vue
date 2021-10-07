<template>
  <div v-if="!game.gameOver" id="scenario" class="scenario">
    <div class="scenario-body">
      <StatusBar :game="game" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
      <PlayerOrder :game="game" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
      <PlayerSelector
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
      <CommittedSkills
        v-if="(game.skillTest?.committedCards?.length || 0) > 0"
        :game="game"
        :cards="game.skillTest.committedCards"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
      <CardRow
        v-if="showCards.ref.length > 0"
        :game="game"
        :cards="showCards.ref"
        :isDiscards="viewingDiscard"
        :title="cardRowTitle"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
        @close="hideCards"
      />
      <div class="scenario-cards">
        <div v-if="topEnemyInVoid">
          <Enemy
            :enemy="topEnemyInVoid"
            :game="game"
            :investigatorId="investigatorId"
            @choose="$emit('choose', $event)"
          />
        </div>
        <div class="deck" :key="scenarioDeck[0]" v-for="[,scenarioDeck] in scenarioDecks">
          <img
            :src="scenarioDeckImage(scenarioDeck)"
            class="card"
          />
          <span v-if="scenarioDeckLabel" class="deck-label">{{scenarioDeckLabel(scenarioDeck)}}</span>
          <span class="deck-size">{{scenarioDeck[1].length}}</span>
        </div>
        <VictoryDisplay :game="game" @show="doShowCards" />
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
          @choose="$emit('choose', $event)"
        />

        <Agenda
          v-for="(agenda, key) in game.agendas"
          :key="key"
          :agenda="agenda"
          :cardsUnder="cardsUnderAgenda"
          :game="game"
          :investigatorId="investigatorId"
          @choose="$emit('choose', $event)"
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
          @choose="$emit('choose', $event)"
          @show="doShowCards"
        />
        <img
          class="card"
          :src="scenarioGuide"
        />
        <ChaosBag
          :game="game"
          :skillTest="game.skillTest"
          :investigatorId="investigatorId"
          @choose="$emit('choose', $event)"
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

      <div ref="locationMap" class="location-cards" :style="locationStyles">
        <Location
          v-for="(location, key) in game.locations"
          class="location"
          :key="key"
          :game="game"
          :investigatorId="investigatorId"
          :location="location"
          :style="{ 'grid-area': location.contents.label, 'justify-self': 'center' }"
          @choose="$emit('choose', $event)"
        />
        <Enemy
          v-for="enemy in enemiesAsLocations"
          :key="enemy.contents.id"
          :enemy="enemy"
          :game="game"
          :investigatorId="investigatorId"
          :style="{ 'grid-area': enemy.contents.asSelfLocation, 'justify-self': 'center' }"
          @choose="$emit('choose', $event)"
        />
      </div>

      <PlayerTabs
        :game="game"
        :investigatorId="investigatorId"
        :players="players"
        :playerOrder="playerOrder"
        :activePlayerId="activePlayerId"
        @choose="$emit('choose', $event)"
      />
    </div>
    <div class="phases">
      <div :class="{ 'active-phase': phase == 'MythosPhase' }">Mythos</div>
      <div :class="{ 'active-phase': phase == 'InvestigationPhase' }">Investigation</div>
      <div :class="{ 'active-phase': phase == 'EnemyPhase' }">Enemy</div>
      <div :class="{ 'active-phase': phase == 'UpkeepPhase' }">Upkeep</div>
    </div>
  </div>
</template>

<script lang="ts">
import throttle from 'lodash/throttle'
import { defineComponent, computed, onMounted, onUnmounted, onUpdated, nextTick, ref, ComputedRef, reactive } from 'vue';
import { Game } from '@/arkham/types/Game';
import { Card } from '@/arkham/types/Card';
import Act from '@/arkham/components/Act.vue';
import Agenda from '@/arkham/components/Agenda.vue';
import Enemy from '@/arkham/components/Enemy.vue';
import CardRow from '@/arkham/components/CardRow.vue';
import CommittedSkills from '@/arkham/components/CommittedSkills.vue';
import StatusBar from '@/arkham/components/StatusBar.vue';
import ChaosBag from '@/arkham/components/ChaosBag.vue';
import PlayerTabs from '@/arkham/components/PlayerTabs.vue';
import PlayerOrder from '@/arkham/components/PlayerOrder.vue';
import PlayerSelector from '@/arkham/components/PlayerSelector.vue';
import EncounterDeck from '@/arkham/components/EncounterDeck.vue';
import VictoryDisplay from '@/arkham/components/VictoryDisplay.vue';
import Location from '@/arkham/components/Location.vue';

function handleConnections(investigatorId: string, game: Game) {
  const makeLine = function(div1: HTMLElement, div2: HTMLElement) {
    const { id: div1Id } = div1.dataset
    const { id: div2Id } = div2.dataset
    const { connectedLocations, contents: investigator } = game.investigators[investigatorId]
    if(div1Id && div2Id) {
      const [left, right] = [div1Id, div2Id].sort()
      const activeLine = (div1Id == investigator.location && connectedLocations.includes(div2Id)) || (div2Id == investigator.location && connectedLocations.includes(div1Id))
      const connection = left + ":" + right
      const line = document.querySelector<HTMLElement>(".line")
      const parentNode = line?.parentNode
      if(line && parentNode && !document.querySelector(`[data-connection="${connection}"]`)) {
        const node = line.cloneNode(true) as HTMLElement
        node.dataset.connection = connection
        node.classList.remove("original")
        if (activeLine) {
          node.classList.add("active")
        }
        parentNode.insertBefore(node, line.nextSibling)
        const {left: bodyLeft, top: bodyTop} = document.body.getBoundingClientRect()
        const {left: div1Left, top: div1Top, right: div1Right, bottom: div1Bottom } = div1.getBoundingClientRect();
        const {left: div2Left, top: div2Top, right: div2Right, bottom: div2Bottom } = div2.getBoundingClientRect();
        const div1Width = div1Right - div1Left;
        const div2Width = div2Right - div2Left;
        const div1Height = div1Bottom - div1Top;
        const div2Height = div2Bottom - div2Top;
        const x1 = (div1Left - bodyLeft) + (div1Width/2)
        const y1 = (div1Top - bodyTop) + (div1Height/2)
        const x2 = (div2Left - bodyLeft) + (div2Width/2)
        const y2 = (div2Top - bodyTop) + (div2Height/2)

        node.setAttribute('x1',x1.toString())
        node.setAttribute('y1',y1.toString())
        node.setAttribute('x2',x2.toString())
        node.setAttribute('y2',y2.toString())
      }
    }
  }

  document.querySelectorAll(".line:not(.original").forEach((node) => node.parentNode?.removeChild(node))

  for(const [id,location] of Object.entries(game.locations)) {
    const connections = location.connectedLocations
    connections.forEach((connection) => {
      const start = document.querySelector(`[data-id="${id}"]`) as HTMLElement
      const end = document.querySelector(`[data-id="${connection}"]`) as HTMLElement
      if(start && end) {
        makeLine(start, end)
      }
    });
  }
}

interface RefWrapper<T> {
  ref: ComputedRef<T>
}

export default defineComponent({
  components: {
    Act,
    Agenda,
    Location,
    StatusBar,
    ChaosBag,
    PlayerTabs,
    CardRow,
    CommittedSkills,
    EncounterDeck,
    PlayerOrder,
    PlayerSelector,
    VictoryDisplay,
    Enemy,
  },
  props: {
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props, { emit }) {
    const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';
    const locationMap = ref<Element | null>(null)

    const drawHandler = throttle(() => handleConnections(props.investigatorId, props.game), 10)

    onMounted(async () => {
      window.addEventListener("resize", drawHandler)
      await nextTick()
      locationMap.value?.addEventListener("scroll", drawHandler)
      handleConnections(props.investigatorId, props.game)
    })

    onUnmounted(() => {
      window.removeEventListener("resize", drawHandler)
      locationMap.value?.removeEventListener("scroll", drawHandler)
    })

    onUpdated(async () => {
      await nextTick()
      handleConnections(props.investigatorId, props.game)
    })

    const scenarioGuide = computed(() => {
      const { scenario } = props.game;
      if (!scenario) {
        return '';
      }

      const { id, difficulty } = scenario.contents;
      const difficultySuffix = difficulty === 'Hard' || difficulty === 'Expert'
        ? 'b'
        : '';

      return `${baseUrl}/img/arkham/cards/${id.replace('c', '')}${difficultySuffix}.jpg`;
    })

    const scenarioDecks = computed(() => {
      const { scenario } = props.game;
      if (!scenario || !scenario.contents.decks) {
        return null;
      }

      return Object.entries(scenario.contents.decks);

    })

    const scenarioDeckImage = (deck: [string, Card[]]) => {
      switch(deck[0]) {
        case 'ExhibitDeck':
          return `${baseUrl}/img/arkham/cards/02132b.jpg`;
        default:
          return `${baseUrl}/img/arkham/back.png`;
      }
    }

    const scenarioDeckLabel = (deck: [string, Card[]]) => {
      switch(deck[0]) {
        case 'CultistDeck':
          return "Cultists"
        default:
          return ""
      }
    }

    const locationStyles = computed(() => {
      const { scenario } = props.game;
      if (!scenario) {
        return null;
      }
      const { locationLayout } = scenario.contents;
      if (locationLayout) {
        return {
          display: 'grid',
          'grid-template-areas': locationLayout.map((row) => `"${row}"`).join(' '),
          'grid-row-gap': '10px',
        };
      }
      return null;
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
    const discards = computed<Card[]>(() => props.game.discard.map(c => { return { tag: 'EncounterCard', contents: c }}))
    const outOfPlay = computed(() => (props.game.scenario?.contents?.setAsideCards || []))
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
      if (props.game.discard[0]) {
        const { cardCode } = props.game.discard[0];

        return `${baseUrl}/img/arkham/cards/${cardCode.replace('c', '')}.jpg`;
      }

      return null;
    })

    const topEnemyInVoid = computed(() => Object.values(props.game.enemiesInVoid)[0])
    const activePlayerId = computed(() => props.game.activeInvestigatorId)

    function update(game: Game) {
      emit('update', game);
    }

    const enemiesAsLocations = computed(() => Object.values(props.game.enemies).filter((enemy) => enemy.contents.asSelfLocation !== null))

    const cardsUnderAgenda = computed(() => {
      const { scenario } = props.game
      if (!scenario) {
        return []
      }

      return scenario.contents.cardsUnderAgendaDeck
    })

    const cardsUnderAct = computed(() => {
      const { scenario } = props.game
      if (!scenario) {
        return []
      }

      return scenario.contents.cardsUnderActDeck
    })

    const cardsNextToAct = computed(() => {
      const { scenario } = props.game
      if (!scenario) {
        return []
      }

      return scenario.contents.cardsNextToActDeck
    })

    const skills = computed(() => Object.values(props.game.skills))
    const phase = computed(() => props.game.phase)

    return {
      hideCards,
      showOutOfPlay,
      showRemovedFromPlay,
      showDiscards,
      phase,
      removedFromPlay,
      skills,
      outOfPlay,
      locationMap,
      update,
      activePlayerId,
      topOfEncounterDiscard,
      players,
      playerOrder,
      activeCard,
      locationStyles,
      scenarioGuide,
      scenarioDecks,
      scenarioDeckImage,
      scenarioDeckLabel,
      topEnemyInVoid,
      enemiesAsLocations,
      cardsUnderAgenda,
      cardsUnderAct,
      cardsNextToAct,
      doShowCards,
      viewDiscardLabel,
      viewingDiscard,
      showCards,
      discards,
      cardRowTitle,
    }
  },

})
</script>

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
  background-image: linear-gradient(darken(#E5EAEC, 10), #E5EAEC);

  @media (prefers-color-scheme: dark) {
    background-image: linear-gradient(#282A36, darken(#282A36, 2));
  }

  width: 100%;
  height: 100%;
  z-index: 1;
  display: grid;
  grid-template-rows: min-content min-content 1fr min-content;
  flex: 1;
}

.location-cards {
  display: flex;
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
  top: 0;
  left: 0;
  width: 100%;
  height: 100vh;
  z-index: -100000000;
}

#line{
  stroke-width:6px;
  /* stroke:#a6b5bb; */
  stroke:rgba(0,0,0, 0.2);
}

.active {
  stroke:rgba(0,0,0,0.5) !important;
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
  writing-mode: vertical-rl;
  text-orientation: mixed;
  justify-content: space-around;
  background-color: #b8c1c6;
  text-transform: uppercase;
  font-family: Arial;
  div {
    flex: 1;
    text-align: center;
  }
}

.scenario {
  display: flex;
  user-select: none;
  width: 100%;
}

.active-phase {
  font-weight: bold;
  background-color: #8e9ca4;
}

.deck {
  position: relative;
}

.deck-label {
  position: absolute;
  top: 0;
  left: 50%;
  font-weight: bold;
  border-radius: 3px;
  padding: 0 2px;
  transform: translateX(-50%) translateY(50%);
  background: rgba(255,255,255,0.8);
}

.deck-size {
  position: absolute;
  font-weight: bold;
  font-size: 1.2em;
  width: 1.3em;
  height: 1.3em;
  border-radius: 1.3em;
  text-align: center;
  color: rgba(255, 255, 255, 0.7);
  background-color: rgba(0, 0, 0, 0.8);
  left: 50%;
  bottom: 0%;
  transform: translateX(-50%) translateY(-50%);
}
</style>
