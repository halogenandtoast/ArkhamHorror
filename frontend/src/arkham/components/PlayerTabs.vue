<script lang="ts" setup>
import { useStorage } from '@vueuse/core'
import { computed, ref, watchEffect, inject } from 'vue';
import type { Ref } from 'vue';
import type { Game } from '@/arkham/types/Game';
import Tab from '@/arkham/components/Tab.vue';
import Player from '@/arkham/components/Player.vue';
import { ArrowPathIcon } from '@heroicons/vue/20/solid';
import * as ArkhamGame from '@/arkham/types/Game';
import { Message, AbilityLabel, type TargetLabel } from '@/arkham/types/Message';
import type { Target } from '@/arkham/types/Target';
import type { Investigator } from '@/arkham/types/Investigator';
import type { TarotCard } from '@/arkham/types/TarotCard';
import type { Placement } from '@/arkham/types/Placement';
import type { Source } from '@/arkham/types/Source';
import { imgsrc } from '@/arkham/helpers';
import { gameLocalStorageKey } from '@/arkham/localStorage';
import { IsMobile } from '@/arkham/isMobile';
import { useSettings } from '@/stores/settings'
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

const storageKey = computed(() => gameLocalStorageKey(props.game.id, 'selected-tab'))
const selectedTab = useStorage<string>(storageKey, props.playerId)
const userPicked = ref(false)

const solo = inject<Ref<boolean>>('solo')
const switchInvestigator = inject<((i: string) => void)>('switchInvestigator')
const hasChoices = (iid: string) => ArkhamGame.choices(props.game, iid).length > 0
const isWaiting = (investigator: Investigator) => props.playerOrder.length > 1 && investigator.playerId in props.game.question
const investigators = computed(() => 
  props.playerOrder.filter(iid => !props.game.investigators[iid]?.eliminated).map(iid => props.players[iid])
)
const inactiveInvestigators = computed(() => props.playerOrder.filter(iid => props.game.investigators[iid]?.eliminated ?? false).map(iid => props.players[iid]))
const lead = computed(() => `url('${imgsrc(`lead-investigator.png`)}')`)
const { isMobile } = IsMobile();
const store = useDbCardStore()

// AI-investigator seats carry an entry in settings.aiPlayers. The seat badge is
// only shown when the dev-only "AI Investigators" flag is enabled.
const settings = useSettings()
function isAiSeat(investigator: Investigator): boolean {
  return settings.aiInvestigatorsEnabled && !!props.game.settings.aiPlayers[investigator.playerId]
}

function tabClass(investigator: Investigator) {
  const pid = investigator.playerId

  const investigatorClass = 
    ['c03006', 'c90087'].includes(investigator.cardCode) && investigator.meta !== 'Neutral' ? (investigator.meta ?? investigator.class) : investigator.class
  return [
    {
      'tab--selected': pid === selectedTab.value,
      'tab--active-player': investigator.id === props.activePlayerId,
      'tab--lead-player': investigator.id === props.game.leadInvestigatorId,
      'tab--has-actions': pid !== props.playerId && hasChoices(investigator.playerId),
      'glow-effect': investigator.id === 'c89001',
    },
    `tab--${investigatorClass}`,
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

const isTargetLabel = (ability: Message): ability is TargetLabel => {
  return ability.tag === "TargetLabel"
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

const targetToPlacement = (target: Target): Placement | null => {
  switch (target.tag) {
    case "EnemyTarget":
      {
        const { contents } = target
        if (typeof contents === 'string') {
          const enemy = props.game.enemies[contents]
          if (enemy) return enemy.placement
        }
      }
    case "TreacheryTarget":
      {
        const { contents } = target
        if (typeof contents === 'string') {
          const treachery = props.game.treacheries[contents]
          if (treachery) return treachery.placement
        }
      }
    default:
  }

  return null
}

watchEffect(() => {
  // determines which tab will be active, it should be the player who is
  // playing the game, but on occasion there will be a forced effect in another
  // players tab and we'd like to direct the player there
  const allChoices = ArkhamGame.choices(props.game, props.playerId)

  const playersWithForced = allChoices
    .reduce((acc, c) => {
      if (isForcedAbility(c)) {
        const { source } = c.ability
        const placement = sourceToPlacement(source)
        if (placement?.tag == 'InThreatArea') {
          const investigator = props.game.investigators[placement.contents]
          if (investigator) acc.push(investigator.playerId)
        }
      } else if (allChoices.length == 1) {
      }
      return acc
    }, [] as string[])

  const allTargets = allChoices.every(isTargetLabel)
  const playersWithTargets = !allTargets ? [] : allChoices
    .reduce((acc, c) => {
      if (isTargetLabel(c)) {
        const placement = targetToPlacement(c.target)
        if (placement?.tag == 'InThreatArea') {
          const investigator = props.game.investigators[placement.contents]
          if (investigator) acc.push(investigator.playerId)
        }
      }
      return acc
    }, [] as string[])

  const playerIds = [...new Set(allTargets ? playersWithTargets : playersWithForced)]

  if (playerIds.length == 0) {
    const investigator = Object.values(props.players).find(i => i.playerId === props.playerId)
    if (investigator && investigator.id == props.activePlayerId) {
      selectedTab.value = props.playerId
      return
    }
    if (userPicked.value) return
  }

  // In true multiplayer each player controls their own view and shouldn't be
  // yanked to another investigator's tab during e.g. a help/commit window where
  // multiple players have choices. We still follow a forced/target effect into
  // another tab when this browser's player is the only player with a choice.
  if (solo?.value !== true) {
    const playersWithChoices = Object.keys(props.game.question).filter(pid => hasChoices(pid))
    const onlyThisPlayerHasChoice =
      playersWithChoices.length === 1 &&
      playersWithChoices[0] === props.playerId

    if (playerIds.length > 0 && !playerIds.includes(props.playerId) && onlyThisPlayerHasChoice) {
      selectedTab.value = playerIds[0]
      return
    }
    if (userPicked.value) return
    selectedTab.value = props.playerId
    return
  }

  selectedTab.value = playerIds.length > 0 && !playerIds.includes(props.playerId)
    ? playerIds[0]
    : props.playerId
})
</script>

<template>
  <div class="player-info">
    <div class="tabs-row">
    <ul class='tabs__header'>
      <li v-for='investigator in investigators'
        :key='investigator.name.title'
        @click='selectTab(investigator.playerId)'
        :class='tabClass(investigator)'
      >
        <span v-if="isMobile">{{ getInvestigatorName(investigator.name.title).split(' ')[0] }}</span>
        <span v-else>{{ getInvestigatorName(investigator.name.title) }}</span>
        <span v-if="isAiSeat(investigator)" class="ai-badge" v-tooltip="'AI controlled'">AI</span>
        <button
          v-if="solo"
          v-tooltip="instructions(investigator)"
          :disabled="investigator.playerId === props.playerId"
          class="switch-investigators"
          @click="selectTabExtended(investigator.playerId)"><font-awesome-icon icon="eye" :class="{ 'fa-icon': hasSwitch(investigator) }" /></button>
        <span
          v-else-if="isWaiting(investigator)"
          class="waiting-indicator"
          v-tooltip="$t('waitingOn.label')"
        ><ArrowPathIcon class="waiting-spinner" aria-hidden="true" /></span>
      </li>
      <li v-for='investigator in inactiveInvestigators'
        :key='investigator.name.title'
        @click='selectTab(investigator.playerId)'
        class="inactive"
        :class='tabClass(investigator)'
      >
        <span>{{ investigator.name.title }}</span>
        <span v-if="isAiSeat(investigator)" class="ai-badge" v-tooltip="'AI controlled'">AI</span>
        <button
          v-if="solo"
          v-tooltip="instructions(investigator)"
          :disabled="investigator.playerId === props.playerId"
          class="switch-investigators"
          @click="selectTabExtended(investigator.playerId)"><font-awesome-icon icon="eye" :class="{ 'fa-icon': hasSwitch(investigator) }" /></button>
        <span
          v-else-if="isWaiting(investigator)"
          class="waiting-indicator"
          v-tooltip="$t('waitingOn.label')"
        ><ArrowPathIcon class="waiting-spinner" aria-hidden="true" /></span>
      </li>
    </ul>
    <slot />
    </div>
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

<style scoped>
.tabs-row {
  display: flex;
  align-items: flex-end;
}

ul.tabs__header {
  flex: 1;
  display: flex;
  list-style: none;
  padding: 0;
  margin: 0;
  user-select: none;
  padding-left: 5px;
  padding-top: 5px;
  font-size: min(16px, 2vw);
  @media (max-width: 800px) and (orientation: portrait) {
    font-size: min(16px, 3vw);
  }
}

ul.tabs__header > li {
  margin: 0;
  margin-right: 3px;
  cursor: pointer;
  color: white;
  opacity: 0.5;
  border-radius: 2px 2px 0 0;
  position: relative;
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
  opacity: 1;
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
  border-radius: 0 2px 0 0;
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

.ai-badge {
  align-self: center;
  margin-right: 5px;
  padding: 1px 5px;
  border-radius: 4px;
  font-size: 0.65em;
  font-weight: bold;
  letter-spacing: 0.08em;
  line-height: 1.4;
  color: #d7e8b0;
  background: rgba(110, 134, 64, 0.45);
  border: 1px solid rgba(110, 134, 64, 0.7);
  text-transform: uppercase;
}

.fa-icon {
  animation: glow 1.5s infinite alternate;
}

.waiting-indicator {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  height: 100%;
  padding: 4px 8px;
  border-left: 1px solid rgba(0, 0, 0, 0.5);
  background-color: rgba(0, 0, 0, 0.5);
  border-radius: 0 2px 0 0;
  color: var(--select);
}

.waiting-spinner {
  width: 0.85em;
  height: 0.85em;
  flex-shrink: 0;
  filter: drop-shadow(0 0 3px color-mix(in srgb, var(--select) 60%, transparent));
  animation: waiting-on-spin 5s linear infinite;
}

@keyframes waiting-on-spin {
  to { transform: rotate(360deg); }
}

@keyframes glow {
  from {
    color: #000; /* Or any other default color */
    text-shadow: 0 0 0px var(--select);
  }
  to {
    color: var(--select); /* Glowing color */
    text-shadow: 0 0 10px var(--select);
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
