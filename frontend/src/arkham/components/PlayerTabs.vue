<script lang="ts" setup>
import { useStorage } from '@vueuse/core'
import { computed, inject, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue';
import type { Ref } from 'vue';
import type { Game } from '@/arkham/types/Game';
import Tab from '@/arkham/components/Tab.vue';
import Player from '@/arkham/components/Player.vue';
import { ArrowPathIcon } from '@heroicons/vue/20/solid';
import * as ArkhamGame from '@/arkham/types/Game';
import type { Investigator } from '@/arkham/types/Investigator';
import type { TarotCard } from '@/arkham/types/TarotCard';
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
const playerInfo = ref<HTMLElement | null>(null)

const solo = inject<Ref<boolean>>('solo')
const spectate = inject<Ref<boolean>>('spectate', ref(false))
const processing = inject<Ref<boolean>>('processing', ref(false))
const uiLock = inject<Ref<boolean>>('uiLock', ref(false))
const switchInvestigator = inject<((i: string) => void)>('switchInvestigator')
const hasChoices = (iid: string) => ArkhamGame.choices(props.game, iid).length > 0
const isWaiting = (investigator: Investigator) => props.playerOrder.length > 1 && investigator.playerId in props.game.question
const investigators = computed(() => 
  props.playerOrder.filter(iid => !props.game.investigators[iid]?.eliminated).map(iid => props.players[iid])
)
const inactiveInvestigators = computed(() => props.playerOrder.filter(iid => props.game.investigators[iid]?.eliminated ?? false).map(iid => props.players[iid]))
const lead = computed(() => `url('${imgsrc(`tokens/lead-investigator.png`)}')`)
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
      'tab--has-actions': pid !== selectedTab.value && hasChoices(pid),
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

type SwitchReason = 'baseline' | 'tab-action' | 'sole-question'
interface SwitchFrame {
  tab: string
  perspective: string
  reason: SwitchReason
}

const switchStack = ref<SwitchFrame[]>([
  { tab: selectedTab.value, perspective: props.playerId, reason: 'baseline' },
])
const pendingPerspective = ref<string | null>(null)

function resetSwitchStack(tab: string, perspective: string) {
  switchStack.value = [{ tab, perspective, reason: 'baseline' }]
}

function selectTab(i: string) {
  selectedTab.value = i
  resetSwitchStack(i, props.playerId)
}

function selectTabExtended(i: string) {
  selectedTab.value = i
  resetSwitchStack(i, i)
  if (solo?.value && props.playerId !== i && switchInvestigator) {
    pendingPerspective.value = i
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

// New actionable controls should use data-game-actionable. The class selectors
// keep existing card/target controls participating while they migrate to the
// shared marker.
const ACTIONABLE_SELECTOR = [
  '[data-game-actionable="true"]',
  '[class*="--can-interact"]',
  '[class*="--can-progress"]',
  '.can-interact',
  '.deck--can-draw',
  '.clue--can-spend',
  '.resource--can-take',
].join(',')

function isAiPlayer(playerId: string) {
  return playerId in props.game.settings.aiPlayers
}

function isEnabledAction(element: Element): element is HTMLElement {
  if (!(element instanceof HTMLElement)) return false
  if (element.matches(':disabled,[aria-disabled="true"]')) return false
  return !element.closest('[inert]')
}

function actionLocations() {
  const scope = playerInfo.value?.closest<HTMLElement>('#scenario') ?? playerInfo.value
  const tabs = new Set<string>()
  let outsideTab = false

  for (const element of scope?.querySelectorAll(ACTIONABLE_SELECTOR) ?? []) {
    if (!isEnabledAction(element)) continue
    const tab = element.closest<HTMLElement>('.tab[data-player-tab]')
    if (!tab) {
      outsideTab = true
      continue
    }
    const playerId = tab.dataset.playerTab
    if (playerId && !isAiPlayer(playerId)) tabs.add(playerId)
  }

  return { tabs, outsideTab }
}

function frameIsStillNeeded(frame: SwitchFrame, tabs: Set<string>) {
  if (frame.reason === 'tab-action') return tabs.has(frame.tab)
  if (frame.reason === 'sole-question') {
    return frame.perspective in props.game.question && !isAiPlayer(frame.perspective)
  }
  return true
}

function applyFrame(frame: SwitchFrame) {
  selectedTab.value = frame.tab
  if (solo?.value === true && props.playerId !== frame.perspective && switchInvestigator) {
    pendingPerspective.value = frame.perspective
    switchInvestigator(frame.perspective)
  }
}

function pushAutomaticFrame(tab: string, perspective: string, reason: Exclude<SwitchReason, 'baseline'>) {
  const current = switchStack.value.at(-1)
  if (current?.tab === tab && current.perspective === perspective) return
  switchStack.value.push({ tab, perspective, reason })
  applyFrame(switchStack.value.at(-1)!)
}

function unwindSwitchStack(tabs: Set<string>) {
  while (switchStack.value.length > 1) {
    const current = switchStack.value.at(-1)!
    if (frameIsStillNeeded(current, tabs)) break
    switchStack.value.pop()
  }
  applyFrame(switchStack.value.at(-1)!)
}

let actionObserver: MutationObserver | null = null
let inspectionFrame: number | null = null
let automaticSwitchCandidate: string | null = null

function scheduleActionInspection() {
  if (inspectionFrame !== null) cancelAnimationFrame(inspectionFrame)
  inspectionFrame = requestAnimationFrame(async () => {
    inspectionFrame = null
    await nextTick()
    inspectActions()
  })
}

function automaticSwitchIsStable(candidate: string) {
  if (automaticSwitchCandidate === candidate) {
    automaticSwitchCandidate = null
    return true
  }

  automaticSwitchCandidate = candidate
  // Vue can briefly remove one tab's controls while replacing a question. Do
  // not route on that intermediate DOM (for example, while Old Book of Lore
  // replaces its investigator target prompt with focused cards).
  scheduleActionInspection()
  return false
}

function inspectActions() {
  if (pendingPerspective.value === props.playerId) pendingPerspective.value = null
  if (spectate.value || processing.value || uiLock.value || pendingPerspective.value !== null) {
    automaticSwitchCandidate = null
    return
  }

  const { tabs, outsideTab } = actionLocations()

  // A unique tab is the only place where the current perspective can act.
  // Controls elsewhere in the scenario deliberately suppress the switch.
  if (!outsideTab && tabs.size === 1) {
    const [onlyTab] = tabs
    if (onlyTab !== selectedTab.value) {
      if (!automaticSwitchIsStable(`tab-action:${onlyTab}:${props.playerId}`)) return
      pushAutomaticFrame(onlyTab, props.playerId, 'tab-action')
      return
    }
  }

  // In multihanded solo, a question belonging solely to another human
  // investigator is enough to change perspective. Its specialized UI may not
  // exist until that perspective is active, so this fallback does not rely on
  // DOM markers.
  if (solo?.value === true && tabs.size === 0 && !outsideTab) {
    const questionPlayers = Object.keys(props.game.question).filter(pid => !isAiPlayer(pid))
    if (questionPlayers.length === 1 && questionPlayers[0] !== props.playerId) {
      const questionPlayer = questionPlayers[0]
      if (!automaticSwitchIsStable(`sole-question:${questionPlayer}`)) return
      pushAutomaticFrame(questionPlayer, questionPlayer, 'sole-question')
      return
    }
  }

  automaticSwitchCandidate = null
  unwindSwitchStack(tabs)
}

onMounted(() => {
  const scope = playerInfo.value?.closest<HTMLElement>('#scenario') ?? playerInfo.value
  if (scope) {
    actionObserver = new MutationObserver(scheduleActionInspection)
    actionObserver.observe(scope, {
      subtree: true,
      childList: true,
      attributes: true,
      attributeFilter: ['class', 'disabled', 'aria-disabled', 'data-game-actionable'],
    })
  }
  scheduleActionInspection()
})

onBeforeUnmount(() => {
  actionObserver?.disconnect()
  if (inspectionFrame !== null) cancelAnimationFrame(inspectionFrame)
})

watch(
  () => [props.playerId, props.game.scenarioSteps, props.game.question, processing.value, uiLock.value] as const,
  scheduleActionInspection,
  { deep: true },
)
</script>

<template>
  <div ref="playerInfo" class="player-info">
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
          @click.stop="selectTabExtended(investigator.playerId)"><font-awesome-icon icon="eye" :class="{ 'fa-icon': hasSwitch(investigator) }" /></button>
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
          @click.stop="selectTabExtended(investigator.playerId)"><font-awesome-icon icon="eye" :class="{ 'fa-icon': hasSwitch(investigator) }" /></button>
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

ul.tabs__header > li.tab--has-actions {
  opacity: 0.85;
  box-shadow:
    inset 0 0 0 1px color-mix(in srgb, var(--select) 70%, transparent),
    0 0 7px color-mix(in srgb, var(--select) 28%, transparent);
  animation: tab-action-pulse 1.8s ease-in-out infinite alternate;
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

@keyframes tab-action-pulse {
  from {
    box-shadow:
      inset 0 0 0 1px color-mix(in srgb, var(--select) 58%, transparent),
      0 0 4px color-mix(in srgb, var(--select) 18%, transparent);
  }
  to {
    box-shadow:
      inset 0 0 0 1px color-mix(in srgb, var(--select) 88%, transparent),
      0 0 9px color-mix(in srgb, var(--select) 36%, transparent);
  }
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
