<script lang="ts" setup>
import { ref, watchEffect, inject } from 'vue';
import type { Ref } from 'vue';
import type { Game } from '@/arkham/types/Game';
import Tab from '@/arkham/components/Tab.vue';
import Player from '@/arkham/components/Player.vue';
import * as ArkhamGame from '@/arkham/types/Game';
import type { Investigator } from '@/arkham/types/Investigator';
import type { TarotCard } from '@/arkham/types/TarotCard';

export interface Props {
  game: Game
  investigatorId: string
  players: Record<string, Investigator>
  playerOrder: string[]
  activePlayerId: string
  tarotCards: TarotCard[]
}

const props = defineProps<Props>()

const selectedTab = ref(props.investigatorId)

const solo = inject<Ref<boolean>>('solo')
const switchInvestigator = inject<((i: string) => void)>('switchInvestigator')

const hasChoices = (iid: string) => ArkhamGame.choices(props.game, iid).length > 0

function tabClass(index: string) {
  const pid = props.players[index].id
  return [
    {
      'tab--selected': index === selectedTab.value,
      'tab--active-player': pid == props.activePlayerId,
      'tab--has-actions': pid !== props.investigatorId && hasChoices(props.players[index].id),
    },
    `tab--${props.players[index].class}`,
  ]
}

function selectTab(i: string) {
  selectedTab.value = i
}

function selectTabExtended(i: string) {
  selectedTab.value = i
  if (solo?.value && props.investigatorId !== i && switchInvestigator) {
    switchInvestigator(i)
  }
}

function tarotCardsFor(i: string) {
  return props.tarotCards.filter(c => c.scope.tag === 'InvestigatorTarot' && c.scope.contents === i)
}


watchEffect(() => selectedTab.value = props.investigatorId)
</script>

<template>
  <div class="player-info">
    <ul class='tabs__header'>
      <li v-for='iid in playerOrder'
        :key='players[iid].name.title'
        @click.exact='selectTab(iid)'
        @click.shift='selectTabExtended(iid)'
        :class='tabClass(iid)'
      >
        {{ players[iid].name.title }}
      </li>
    </ul>
    <Tab
      v-for="(player, index) in players"
      :key="index"
      :index="index"
      :selectedTab="selectedTab"
      :playerClass="player.class"
      :title="player.name.title"
      :investigatorId="index"
      :activePlayer="player.id == activePlayerId"
    >
      <Player
        :game="game"
        :investigatorId="investigatorId"
        :player="player"
        :tarotCards="tarotCardsFor(player.id)"
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
</style>
