<script lang="ts" setup>
import { computed, ref, watchEffect, inject } from 'vue';
import type { Ref } from 'vue';
import type { Game } from '@/arkham/types/Game';
import Tab from '@/arkham/components/Tab.vue';
import Player from '@/arkham/components/Player.vue';
import * as ArkhamGame from '@/arkham/types/Game';
import type { Investigator } from '@/arkham/types/Investigator';
import type { TarotCard } from '@/arkham/types/TarotCard';
import { imgsrc } from '@/arkham/helpers';

export interface Props {
  game: Game
  playerId: string
  players: Record<string, Investigator>
  playerOrder: string[]
  activePlayerId: string
  tarotCards: TarotCard[]
}

const props = defineProps<Props>()
const selectedTab = ref(props.playerId)
const solo = inject<Ref<boolean>>('solo')
const switchInvestigator = inject<((i: string) => void)>('switchInvestigator')
const hasChoices = (iid: string) => ArkhamGame.choices(props.game, iid).length > 0
const investigators = computed(() => props.playerOrder.map(iid => props.players[iid]))
const inactiveInvestigators = computed(() => Object.values(props.players).filter((p) => !props.playerOrder.includes(p.id)))
const lead = computed(() => `url('${imgsrc(`lead-investigator.png`)}')`)

function tabClass(investigator: Investigator) {
  const pid = investigator.playerId
  return [
    {
      'tab--selected': pid === selectedTab.value,
      'tab--active-player': investigator.id === props.activePlayerId,
      'tab--lead-player': investigator.id === props.game.leadInvestigatorId,
      'tab--has-actions': pid !== props.playerId && hasChoices(investigator.playerId),
    },
    `tab--${investigator.class}`,
  ]
}

function hasSwitch(investigator: Investigator) {
  const pid = investigator.playerId
  return pid !== props.playerId && hasChoices(investigator.playerId)
}

function instructions(investigator: Investigator) {
  if (investigator.playerId !== props.playerId) {
    return "Switch to this investigator's perspective"
  }

  return null
}

function selectTab(i: string) {
  selectedTab.value = i
}

function selectTabExtended(i: string) {
  selectedTab.value = i
  if (solo?.value && props.playerId !== i && switchInvestigator) {
    switchInvestigator(i)
  }
}

function tarotCardsFor(i: string) {
  return props.tarotCards.filter(c => c.scope.tag === 'InvestigatorTarot' && c.scope.contents === i)
}


watchEffect(() => selectedTab.value = props.playerId)
</script>

<template>
  <div class="player-info">
    <ul class='tabs__header'>
      <li v-for='investigator in investigators'
        :key='investigator.name.title'
        @click='selectTab(investigator.playerId)'
        :class='tabClass(investigator)'
      >
        <span>{{ investigator.name.title }}</span>
        <button
          v-if="solo"
          v-tooltip="instructions(investigator)"
          :disabled="investigator.playerId === props.playerId"
          class="switch-investigators"
          @click="selectTabExtended(investigator.playerId)"><font-awesome-icon icon="eye" :class="{ 'fa-icon': hasSwitch(investigator) }" /></button>
      </li>
      <li v-for='investigator in inactiveInvestigators'
        :key='investigator.name.title'
        @click='selectTab(investigator.playerId)'
        class="inactive"
        :class='tabClass(investigator)'
      >
        <span>{{ investigator.name.title }}</span>
        <button
          v-if="solo"
          v-tooltip="instructions(investigator)"
          :disabled="investigator.playerId === props.playerId"
          class="switch-investigators"
          @click="selectTabExtended(investigator.playerId)"><font-awesome-icon icon="eye" :class="{ 'fa-icon': hasSwitch(investigator) }" /></button>
      </li>
    </ul>
    <Tab
      v-for="investigator in investigators"
      :key="investigator.id"
      :index="investigator.playerId"
      :selectedTab="selectedTab"
      :playerClass="investigator.class"
      :title="investigator.name.title"
      :playerId="playerId"
      :investigatorId="investigator.id"
      :activePlayer="investigator.playerId == activePlayerId"
    >
      <Player
        :game="game"
        :playerId="playerId"
        :investigator="investigator"
        :tarotCards="tarotCardsFor(investigator.id)"
        @choose="$emit('choose', $event)"
      />
    </Tab>
    <Tab
      v-for="investigator in inactiveInvestigators"
      :key="investigator.id"
      :index="investigator.playerId"
      :selectedTab="selectedTab"
      :playerClass="investigator.class"
      :title="investigator.name.title"
      :playerId="playerId"
      :investigatorId="investigator.id"
      :activePlayer="investigator.playerId == activePlayerId"
    >
      <Player
        :game="game"
        :playerId="playerId"
        :investigator="investigator"
        :tarotCards="tarotCardsFor(investigator.id)"
        @choose="$emit('choose', $event)"
      />
    </Tab>
  </div>
</template>

<style lang="scss">
ul.tabs__header {
  display: flex;
  list-style: none;
  padding: 0;
  margin: 0;
  user-select: none;
  padding-left: 5px;
}

ul.tabs__header > li {
  margin: 0;
  margin-right: 5px;
  cursor: pointer;
  color: white;
  filter: contrast(50%);
  border-radius: 5px 5px 0 0;
  display: flex;
  span {
    display: block;
    width: fit-content;
    padding: 5px 10px;
  }
}

ul.tabs__header > li.tab--selected {
  font-weight: bold;
  filter: contrast(100%);
}

.tab--Guardian {
  background-color: $guardian;
}

.tab--Seeker {
  background-color: $seeker;
}

.tab--Rogue {
  background-color: $rogue;
}

.tab--Mystic {
  background-color: $mystic;
}

.tab--Survivor {
  background-color: $survivor;
}

.tab--Neutral {
  background-color: $neutral;
}

.tab--active-player {
  &:before {
    font-weight: normal;
    font-family: "Arkham";
    content: "\0058" / "Active Player";
    margin-left: 5px;
    align-self: center;
  }
}

.player-info {
  margin-top: -32px;
}

.tab--lead-player {
  &:before {
    position: absolute;
    content: "";
    inset: 0;
    top: -5px;
    margin-inline: auto;
    transform: translateY(-100%);
    width: 25px;
    height: 25px;
    background-image: v-bind(lead);
    background-size: contain;
  }
}

.switch-investigators {
  background: none;
  border: none;
  color: white;
  border-left: 1px solid rgba(0, 0, 0, 0.5);
  padding: 4px 8px;
  background-color: rgba(0, 0, 0, 0.5);
  border-radius: 0 5px 0 0;
  margin: 0;
  cursor: pointer;

  &[disabled] {
    background: rgba(0, 0, 0, 0.5);
    color: rgba(255, 255, 255, 0.2);
  }
  &:not([disabled]):hover {
    filter: contrast(200%);
    color: black;
  }
}

.fa-icon {
  animation: glow 1.5s infinite alternate;
}

@keyframes glow {
  from {
    color: #000; /* Or any other default color */
    text-shadow: 0 0 0px #ff00ff;
  }
  to {
    color: #ff00ff; /* Glowing color */
    text-shadow: 0 0 10px #ff00ff;
  }
}

ul.tabs__header > li.inactive {
  filter: grayscale(100%);
}
</style>
