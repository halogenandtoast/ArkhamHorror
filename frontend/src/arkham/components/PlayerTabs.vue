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
import type { Question } from '@/arkham/types/Question';
import { MessageType } from '@/arkham/types/Message';
import type { TarotCard } from '@/arkham/types/TarotCard';
import { imgsrc, isTypingTarget } from '@/arkham/helpers';
import { gameLocalStorageKey } from '@/arkham/localStorage';
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

type SwitchReason = 'baseline' | 'tab-action' | 'sole-question' | 'covered-question'
interface SwitchFrame {
  tab: string
  perspective: string
  reason: SwitchReason
}

const switchStack = ref<SwitchFrame[]>([
  { tab: selectedTab.value, perspective: props.playerId, reason: 'baseline' },
])
const pendingPerspective = ref<string | null>(null)
let manualSelectionAtStep: number | null = null

function resetSwitchStack(tab: string, perspective: string) {
  switchStack.value = [{ tab, perspective, reason: 'baseline' }]
}

function selectTab(i: string) {
  manualSelectionAtStep = props.game.scenarioSteps
  selectedTab.value = i
  resetSwitchStack(i, props.playerId)
}

// The eye button is the only way to act as another seat, so an explicit perspective
// switch must outrank automatic routing for the current game step -- otherwise a
// declinable fast window on the destination seat is filtered out of
// focusQuestionPlayers() and the sole-question rule immediately routes back to the
// active investigator, stranding that seat's abilities out of reach (#5350).
function selectTabExtended(i: string) {
  manualSelectionAtStep = props.game.scenarioSteps
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
    if (playerId) tabs.add(playerId)
  }

  return { tabs, outsideTab }
}

function humanQuestionPlayers() {
  return Object.keys(props.game.question)
}

// An out-of-turn fast player window: a PlayerWindowChooseOne carrying that seat's own
// Skip Triggers button. It is optional by construction and the engine re-offers it at
// the next player window, so it must not hold the solo perspective on a seat that is
// not taking a turn -- otherwise ending "Ashcan" Pete's turn strands you on Pete while
// Rex, the turn player, waits behind a tab (#5284). Reaction windows decode as
// WindowChooseOne (isPlayerWindow false) and are deliberately NOT covered: those are
// tied to something that just happened and still deserve focus.
function isDeclinableFastWindow(playerId: string) {
  if (!ArkhamGame.activeQuestionIsPlayerWindow(props.game, playerId)) return false
  return ArkhamGame.choices(props.game, playerId)
    .some(choice => choice.tag === MessageType.SKIP_TRIGGERS_BUTTON)
}

// Question seats allowed to claim the perspective. If every seat is a declinable fast
// window there is nothing better to route to, so fall back to the full list rather than
// leaving the only answerable question unreachable.
function focusQuestionPlayers() {
  const players = humanQuestionPlayers()
  const focusable = players.filter(pid => !isDeclinableFastWindow(pid))
  return focusable.length > 0 ? focusable : players
}

function skillTestPlayerId() {
  if (solo?.value !== true) return undefined
  const investigatorId = props.game.skillTest?.investigator
  return investigatorId ? props.game.investigators[investigatorId]?.playerId : undefined
}

function activeInvestigatorPlayerId() {
  return props.game.investigators[props.game.activeInvestigatorId]?.playerId
}

function questionKind(playerId: string) {
  let question: Question | undefined = props.game.question[playerId]
  const tags: string[] = []
  while (question) {
    tags.push(question.tag)
    question = 'question' in question ? question.question : undefined
  }
  return tags.join('/')
}

function playerCanAnswerAllQuestionsFrom(playerId: string, otherPlayerId: string) {
  if (questionKind(playerId) !== questionKind(otherPlayerId)) return false
  const available = new Set(ArkhamGame.choices(props.game, playerId).map(choice => JSON.stringify(choice)))
  return ArkhamGame.choices(props.game, otherPlayerId).every(choice => available.has(JSON.stringify(choice)))
}

function frameIsStillNeeded(frame: SwitchFrame, tabs: Set<string>) {
  const skillTestPlayer = skillTestPlayerId()
  // Once an automatic switch has brought us to the investigator taking the
  // test, transient fast-window questions must not unwind us to an older tab.
  // A sole actionable tab can still add a temporary frame above this one.
  if (frame.reason !== 'baseline' && skillTestPlayer && frame.tab === skillTestPlayer) {
    return true
  }
  if (frame.reason === 'tab-action') return tabs.has(frame.tab)
  if (frame.reason === 'sole-question') {
    const questionPlayers = focusQuestionPlayers()
    return questionPlayers.length === 1 && questionPlayers[0] === frame.perspective
  }
  if (frame.reason === 'covered-question') return false
  return true
}

function applyFrame(frame: SwitchFrame) {
  selectedTab.value = frame.tab
  if (solo?.value === true && props.playerId !== frame.perspective && switchInvestigator) {
    pendingPerspective.value = frame.perspective
    switchInvestigator(frame.perspective)
  }
}

const automaticSwitchStackEnabled = false

function pushAutomaticFrame(tab: string, perspective: string, reason: Exclude<SwitchReason, 'baseline'>) {
  if (!automaticSwitchStackEnabled) {
    // One-way routing: meeting another criterion may switch again, but ending a
    // criterion never restores an earlier tab. The user can always choose one.
    applyFrame({ tab, perspective, reason })
    return
  }

  // Retained stack implementation. This is deliberately unreachable while
  // automaticSwitchStackEnabled is false.
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

const tabbableInvestigators = computed(() => [...investigators.value, ...inactiveInvestigators.value])

function seatShortcutIndex(event: KeyboardEvent): number | null {
  const code = /^(?:Digit|Numpad)([1-4])$/.exec(event.code)
  if (code) return Number(code[1]) - 1
  if (/^[1-4]$/.test(event.key)) return Number(event.key) - 1
  return null
}

function handleSeatShortcut(event: KeyboardEvent) {
  if (event.ctrlKey || event.metaKey || event.altKey || event.repeat) return
  if (isTypingTarget(event.target)) return

  const index = seatShortcutIndex(event)
  if (index === null) return

  const investigator = tabbableInvestigators.value[index]
  if (!investigator) return

  event.preventDefault()
  const pid = investigator.playerId
  if (event.shiftKey && solo?.value && pid !== props.playerId) {
    selectTabExtended(pid)
  } else {
    selectTab(pid)
  }
}

let actionObserver: MutationObserver | null = null
let inspectionFrame: number | null = null
let automaticSwitchCandidate: string | null = null
let automaticSwitchCandidateSince = 0

function scheduleActionInspection() {
  if (inspectionFrame !== null) cancelAnimationFrame(inspectionFrame)
  inspectionFrame = requestAnimationFrame(async () => {
    inspectionFrame = null
    await nextTick()
    inspectActions()
  })
}

function automaticSwitchIsStable(candidate: string) {
  const now = performance.now()
  if (automaticSwitchCandidate === candidate) {
    // Two adjacent animation frames were still short enough to catch the
    // intermediate DOM produced while playing assets such as Flashlight. Wait
    // for the destination to remain unchanged before routing.
    if (now - automaticSwitchCandidateSince >= 120) {
      automaticSwitchCandidate = null
      return true
    }
  } else {
    automaticSwitchCandidate = candidate
    automaticSwitchCandidateSince = now
  }

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

  // Clicking a tab mutates the observed DOM. Keep the user's selection stable
  // for the current game state rather than immediately routing away and back.
  if (manualSelectionAtStep === props.game.scenarioSteps) {
    automaticSwitchCandidate = null
    return
  }
  manualSelectionAtStep = null

  const { tabs, outsideTab } = actionLocations()
  const questionPlayers = focusQuestionPlayers()
  const soleQuestionPlayer = questionPlayers.length === 1 ? questionPlayers[0] : null
  const answerableQuestionPlayers = questionPlayers.filter(pid => ArkhamGame.choices(props.game, pid).length > 0)
  const soleAnswerableQuestionPlayer = questionPlayers.length > 1 && answerableQuestionPlayers.length === 1
    ? answerableQuestionPlayers[0]
    : null
  const skillTestPlayer = skillTestPlayerId()
  const activeQuestionPlayer = activeInvestigatorPlayerId()
  const activePlayerCoversOtherQuestions = solo?.value === true
    && questionPlayers.length > 1
    && activeQuestionPlayer !== undefined
    && questionPlayers.includes(activeQuestionPlayer)
    && questionPlayers.every(pid => pid === activeQuestionPlayer || playerCanAnswerAllQuestionsFrom(activeQuestionPlayer, pid))

  // Some shared prompts create a question for every investigator but only give
  // one of them choices. Route to that investigator instead of leaving an
  // empty version of the prompt in front of the active investigator.
  if (solo?.value === true && soleAnswerableQuestionPlayer) {
    automaticSwitchCandidate = null
    if (selectedTab.value !== soleAnswerableQuestionPlayer || props.playerId !== soleAnswerableQuestionPlayer) {
      // This decision comes from the settled game question rather than
      // transient DOM controls, so it does not need the action stability delay.
      pushAutomaticFrame(soleAnswerableQuestionPlayer, soleAnswerableQuestionPlayer, 'sole-question')
    }
    return
  }

  // Keep the active investigator in view when they can answer every question
  // offered to the other investigators. Switching tabs adds no capability and
  // needlessly interrupts their turn.
  if (activePlayerCoversOtherQuestions && activeQuestionPlayer) {
    if (selectedTab.value !== activeQuestionPlayer || props.playerId !== activeQuestionPlayer) {
      if (!automaticSwitchIsStable(`covered-question:${activeQuestionPlayer}`)) return
      pushAutomaticFrame(activeQuestionPlayer, activeQuestionPlayer, 'covered-question')
    } else {
      automaticSwitchCandidate = null
    }
    return
  }

  // The only control for the sole question can live in another investigator's
  // play area: a forced ability, or a target choice such as Correlate All Its
  // Contents placing a charge on an asset controlled by an investigator at your
  // location (#5495). The sole-question rule below would otherwise keep the
  // question owner's own -- empty -- tab in front of them. Keep their
  // perspective while showing the only tab where the control can be selected.
  if (solo?.value === true && soleQuestionPlayer && tabs.size === 1 && !tabs.has(soleQuestionPlayer)) {
    const [actionTab] = tabs
    if (selectedTab.value !== actionTab || props.playerId !== soleQuestionPlayer) {
      if (!automaticSwitchIsStable(`action-tab:${actionTab}:${soleQuestionPlayer}`)) return
      pushAutomaticFrame(actionTab, soleQuestionPlayer, 'sole-question')
    } else {
      automaticSwitchCandidate = null
    }
    return
  }

  // A sole question owns the tab even if Vue has left stale actionable controls
  // on another tab. During a skill test, however, another investigator's fast
  // window does not pull focus away from the test taker unless that
  // investigator's tab is the sole place with an actionable control.
  //
  // Only a *declinable* window may be held back that way. game.skillTest stays
  // populated after the test resolves, while the consequences of the result are
  // still resolving -- an Arcane Barrier leave cost that fails can discard the
  // location, move everyone off it, and hand each investigator in turn a forced
  // ability, all with the failed test still open. A forced ability or reaction
  // cannot be declined and is the only thing that can advance the game, so it
  // has to claim the perspective even then; otherwise the sole answerable
  // question sits behind a tab with no control rendered anywhere on screen.
  const skillTestHoldsFocus =
    !!skillTestPlayer
    && soleQuestionPlayer !== skillTestPlayer
    && isDeclinableFastWindow(soleQuestionPlayer as string)
  if (solo?.value === true && soleQuestionPlayer && !skillTestHoldsFocus) {
    if (selectedTab.value !== soleQuestionPlayer || props.playerId !== soleQuestionPlayer) {
      if (!automaticSwitchIsStable(`sole-question:${soleQuestionPlayer}`)) return
      pushAutomaticFrame(soleQuestionPlayer, soleQuestionPlayer, 'sole-question')
    } else {
      automaticSwitchCandidate = null
    }
    return
  }

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

  automaticSwitchCandidate = null
  if (automaticSwitchStackEnabled) unwindSwitchStack(tabs)
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
  document.addEventListener('keydown', handleSeatShortcut)
})

onBeforeUnmount(() => {
  document.removeEventListener('keydown', handleSeatShortcut)
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
