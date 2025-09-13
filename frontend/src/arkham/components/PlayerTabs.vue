<script lang="ts" setup>
import { useStorage } from '@vueuse/core'
import { computed, ref, watchEffect, inject } from 'vue';
import type { Ref } from 'vue';
import type { Game } from '@/arkham/types/Game';
import Tab from '@/arkham/components/Tab.vue';
import Player from '@/arkham/components/Player.vue';
import * as ArkhamGame from '@/arkham/types/Game';
import { Message, AbilityLabel } from '@/arkham/types/Message';
import type { Investigator } from '@/arkham/types/Investigator';
import type { TarotCard } from '@/arkham/types/TarotCard';
import type { Placement } from '@/arkham/types/Placement';
import type { Source } from '@/arkham/types/Source';
import { imgsrc } from '@/arkham/helpers';
import { IsMobile } from '@/arkham/isMobile';
import { useDbCardStore } from '@/stores/dbCards'

export interface Props {
  game: Game
  playerId: string
  players: Record<string, Investigator>
  playerOrder: string[]
  activePlayerId: string
  tarotCards: TarotCard[]
}

const props = defineProps<Props>()

const storageKey = computed(() => `selected-tab:${props.game.id}`)
const selectedTab = useStorage<string>(storageKey, props.playerId)
const userPicked = ref(false)

const solo = inject<Ref<boolean>>('solo')
const switchInvestigator = inject<((i: string) => void)>('switchInvestigator')
const hasChoices = (iid: string) => ArkhamGame.choices(props.game, iid).length > 0
const investigators = computed(() => props.playerOrder.map(iid => props.players[iid]))
const inactiveInvestigators = computed(() => Object.values(props.players).filter((p) => !props.playerOrder.includes(p.id)))
const lead = computed(() => `url('${imgsrc(`lead-investigator.png`)}')`)
const { isMobile } = IsMobile();
const store = useDbCardStore()

function tabClass(investigator: Investigator) {
  const pid = investigator.playerId
  return [
    {
      'tab--selected': pid === selectedTab.value,
      'tab--active-player': investigator.id === props.activePlayerId,
      'tab--lead-player': investigator.id === props.game.leadInvestigatorId,
      'tab--has-actions': pid !== props.playerId && hasChoices(investigator.playerId),
      'glow-effect': investigator.id === 'c89001',
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
  userPicked.value = true
}

function selectTabExtended(i: string) {
  selectedTab.value = i
  userPicked.value = true
  if (solo?.value && props.playerId !== i && switchInvestigator) {
    switchInvestigator(i)
  }
}

function tarotCardsFor(i: string) {
  return props.tarotCards.filter(c => c.scope.tag === 'InvestigatorTarot' && c.scope.contents === i)
}

function getInvestigatorName(cardTitle: string): string {
  const language = localStorage.getItem('language') || 'en'
  return language === 'en'? cardTitle : store.getCardName(cardTitle, "investigator")
}

const isForcedAbility = (ability: Message): ability is AbilityLabel => {
  return ability.tag === "AbilityLabel" && ability.ability.type.tag === "ForcedAbility"
}

const sourceToPlacement = (source: Source): Placement | null => {
  switch (source.tag) {
    case "EnemySource":
      {
        const { contents } = source
        if (contents) return props.game.enemies[contents].placement
      }
    case "TreacherySource":
      {
        const { contents } = source
        if (contents) return props.game.treacheries[contents].placement
      }
    default:
  }

  return null
}

watchEffect(() => {
  // determines which tab will be active, it should be the player who is
  // playing the game, but on occasion there will be a forced effect in another
  // players tab and we'd like to direct the player there
  const playersWithForced = ArkhamGame
    .choices(props.game, props.playerId)
    .reduce((acc, c) => {
      if (isForcedAbility(c)) {
        const { source } = c.ability
        const placement = sourceToPlacement(source)
        if (placement?.tag == 'InThreatArea') {
          const investigator = props.game.investigators[placement.contents]
          if (investigator) acc.push(investigator.playerId)
        }
      }
      return acc
    }, [] as string[])

  const playerIds = [...new Set(playersWithForced)]

  if (playerIds.length == 0) {
    const investigator = Object.values(props.players).find(i => i.playerId === props.playerId)
    if (investigator && investigator.id == props.activePlayerId) {
      selectedTab.value = props.playerId
      return
    }
    if (userPicked.value) return
  }

  selectedTab.value = playerIds.length > 0 && !playerIds.includes(props.playerId)
    ? playerIds[0]
    : props.playerId
})
</script>

<template>
  <div class="player-info">
    <ul class='tabs__header'>
      <li v-for='investigator in investigators'
        :key='investigator.name.title'
        @click='selectTab(investigator.playerId)'
        :class='tabClass(investigator)'
      >
        <span v-if="isMobile">{{ getInvestigatorName(investigator.name.title).split(' ')[0] }}</span>
        <span v-else>{{ getInvestigatorName(investigator.name.title) }}</span>
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
  font-size: min(16px, 2vw);
  @media (max-width: 800px) and (orientation: portrait) {
    font-size: min(16px, 3vw);
  }
}

ul.tabs__header > li {
  margin: 0;
  margin-right: 5px;
  cursor: pointer;
  color: white;
  filter: contrast(50%);
  border-radius: 5px 5px 0 0;
  display: inline-flex;
  align-items: center;
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
  background-color: var(--guardian-extra-dark);
}

.tab--Seeker {
  background-color: var(--seeker-extra-dark);
}

.tab--Rogue {
  background-color: var(--rogue-extra-dark);
}

.tab--Mystic {
  background-color: var(--mystic-extra-dark);
}

.tab--Survivor {
  background-color: var(--survivor-extra-dark);
}

.tab--Neutral {
  background-color: var(--neutral-dark);
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
  &:after {
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
  height: 100%;
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
  &:before {
    font-family: "ArkhamIcons";
    content: "\e912";
    font-size: 0.8em;
    margin-left: 5px;
  }
}

.glow-effect {
  box-shadow: inset 0 -10px 20px -10px rgba(0, 255, 0, 0.7); /* Inset shadow for glow effect */
}
</style>
