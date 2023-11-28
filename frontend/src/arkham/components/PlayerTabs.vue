<script lang="ts" setup>
import { computed, ref, watchEffect, inject } from 'vue';
import type { Ref } from 'vue';
import type { Game } from '@/arkham/types/Game';
import Tab from '@/arkham/components/Tab.vue';
import Player from '@/arkham/components/Player.vue';
import * as ArkhamGame from '@/arkham/types/Game';
import type { Investigator } from '@/arkham/types/Investigator';
import type { TarotCard } from '@/arkham/types/TarotCard';

export interface Props {
  game: Game
  playerId: string
  players: Record<string, Investigator>
  playerOrder: string[]
  activePlayerId: string
  tarotCards: TarotCard[]
}

const props = defineProps<Props>()

const investigatorId = Object.values(props.game.investigators).find(i => i.playerId === props.playerId)?.id

const selectedTab = ref(props.playerId)

const solo = inject<Ref<boolean>>('solo')
const switchInvestigator = inject<((i: string) => void)>('switchInvestigator')

const hasChoices = (iid: string) => ArkhamGame.choices(props.game, iid).length > 0

const investigators = computed(() => props.playerOrder.map(iid => props.players[iid]))

function tabClass(investigator: Investigator) {
  const pid = investigator.playerId
  return [
    {
      'tab--selected': pid === selectedTab.value,
      'tab--active-player': investigator.id === props.activePlayerId,
      'tab--has-actions': pid !== props.playerId && hasChoices(investigator.playerId),
    },
    `tab--${investigator.class}`,
  ]
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
        @click.exact='selectTab(investigator.playerId)'
        @click.shift='selectTabExtended(investigator.playerId)'
        :class='tabClass(investigator)'
      >
        {{ investigator.name.title }}
      </li>
    </ul>
    <Tab
      v-for="investigator in investigators"
      :key="investigator.playerId"
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
  display: block;
  list-style: none;
  padding: 0;
  margin: 0;
  user-select: none;
  padding-left: 5px;
}

ul.tabs__header > li {
  margin: 0;
  display: inline-block;
  margin-right: 5px;
  cursor: pointer;
  color: white;
  padding: 5px 10px;
  filter: contrast(50%);
  border-radius: 5px 5px 0 0;
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
    content: "\0058";
    margin-right: 5px;
  }
}

.tab--has-actions {
  box-shadow: 0 0 5px #ff00ff;
}

.player-info {
  margin-top: -32px;
}
</style>
