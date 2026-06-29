<script lang="ts" setup>
import {
  computed,
  nextTick,
  onMounted,
  onUnmounted,
  provide,
  ref,
  shallowRef,
  watch,
} from 'vue'
import { onBeforeRouteLeave, useRoute, useRouter } from 'vue-router'
import { useI18n } from 'vue-i18n'
import confetti from '@/effects/confetti'
import { useWebSocket, useResizeObserver } from '@vueuse/core'
import { MenuItem } from '@headlessui/vue'
import {
  AdjustmentsHorizontalIcon,
  ArrowPathIcon,
  ArrowsRightLeftIcon,
  ArrowUturnLeftIcon,
  BackwardIcon,
  BeakerIcon,
  BoltIcon,
  BugAntIcon,
  ClockIcon,
  DocumentArrowDownIcon,
  DocumentTextIcon,
  EyeIcon,
  ExclamationTriangleIcon,
  FlagIcon,
  RectangleStackIcon,
} from '@heroicons/vue/20/solid'
import { LottieAnimation } from 'lottie-web-vue'
import * as JsonDecoder from 'ts.data.json'
import processingJSON from '@/assets/processing.json'
import api from '@/api'
import {
  fetchGame,
  buildWebsocketUrl,
  undoChoice,
  undoScenarioChoice,
  undoAction,
  undoTurn,
  undoPhase,
  undoRound,
  markEventReady,
  eventTimeUp,
} from '@/arkham/api'
import * as Api from '@/arkham/api'
import { useCardStore } from '@/stores/cards'
import { useUserStore } from '@/stores/user'
import { useEventStore } from '@/arkham/stores/event'
import { useEventTimer } from '@/arkham/composables/useEventTimer'
import { awaitingOrganizer, type SharedEventState } from '@/arkham/types/EpicEvent'
import { useMenu } from '@/composable/menu'
import useEmitter from '@/composable/useEmitter'
import { useDebug } from '@/arkham/debug'
import { useAi } from '@/arkham/ai'
import { useSettings } from '@/stores/settings'
import { imgsrc } from '@/arkham/helpers'
import { handleEmbeddedI18n } from '@/arkham/i18n'
import { getGameLocalStorageItem, setGameLocalStorageItem } from '@/arkham/localStorage'
import * as Arkham from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import {
  choicesByPlayerKey,
  choicesSourceByPlayerKey,
  choicesTooltipByPlayerKey,
} from '@/arkham/composables/useGameChoices'
import { buildGameIndexes, gameIndexesKey } from '@/arkham/composables/useGameIndexes'
import { Card, cardDecoder, toCardContents } from '@/arkham/types/Card'
import * as Message from '@/arkham/types/Message'
import { type Question } from '@/arkham/types/Question'
import type { Source } from '@/arkham/types/Source'
import { TarotCard, tarotCardDecoder, tarotCardImage } from '@/arkham/types/TarotCard'
import Campaign from '@/arkham/components/Campaign.vue'
import CampaignLog from '@/arkham/components/CampaignLog.vue'
import CampaignSettings from '@/arkham/components/CampaignSettings.vue'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import CardView from '@/arkham/components/Card.vue'
import MultiplayerLobby from '@/arkham/components/MultiplayerLobby.vue'
import GameLog from '@/arkham/components/GameLog.vue'
import HistoryPanel from '@/arkham/components/HistoryPanel.vue'
import ScenarioSettings from '@/arkham/components/ScenarioSettings.vue'
import Settings from '@/arkham/components/Settings.vue'
import OrganizerBar from '@/arkham/components/OrganizerBar.vue'
import PlayerEventBar from '@/arkham/components/PlayerEventBar.vue'
import EventStartBarrier from '@/arkham/components/EventStartBarrier.vue'
import EventActAdvanceBarrier from '@/arkham/components/EventActAdvanceBarrier.vue'
import StandaloneScenario from '@/arkham/components/StandaloneScenario.vue'
import AiControlPanel from '@/arkham/components/AiControlPanel.vue'
import AiQuestionsPanel from '@/arkham/components/AiQuestionsPanel.vue'
import Draggable from '@/components/Draggable.vue'
import Menu from '@/components/Menu.vue'
import Prompt from '@/components/Prompt.vue'

interface GameCard {
  title: string
  card: Card
}

interface GameCardOnly {
  player: string
  title: string
  card: Card
}

// TODO: contents should not be string
type ServerResult =
  | { tag: 'GameError'; contents: string }
  | { tag: 'GameMessage'; contents: string }
  | { tag: 'GameTarot'; contents: string }
  | { tag: 'GameCard'; contents: string }
  | { tag: 'GameCardOnly'; contents: string }
  | { tag: 'GameUpdate'; contents: string }
  | { tag: 'GameShowDiscard'; contents: string }
  | { tag: 'GameShowUnder'; contents: string }
  | { tag: 'GameUI'; contents: string }
  | { tag: 'GameAudio'; contents: string }
  | { tag: 'SharedStateUpdate'; contents: SharedEventState }

export interface Props {
  gameId: string
  spectate?: boolean
}

const props = withDefaults(defineProps<Props>(), { spectate: false })

const debug = useDebug()
const ai = useAi()
const settings = useSettings()
// AI-investigator UI/driver is gated on the dev-only "AI Investigators" settings
// flag (Settings → danger zone). The flag is itself `isDevBuild() && stored`, so
// it is never enabled in production and defaults OFF in dev until toggled on.
const aiDevEnabled = computed(() => settings.aiInvestigatorsEnabled)
const emitter = useEmitter()
const router = useRouter()
const route = useRoute()
const store = useCardStore()
const userStore = useUserStore()
const eventStore = useEventStore()
const { addEntry, menuItems } = useMenu()

// "Epic Multiplayer": a group's game can be entered two ways — via the dashboard's
// per-group links (which carry an ?event=<id> query param) OR via the plain
// join / "take a seat" path (which does NOT). To engage the event on EITHER path
// we resolve the event id from the URL first, then fall back to the `eventId` the
// game-fetch response now carries (resolved server-side). Everything that needs to
// know "which event is this game a group of" keys off `resolvedEventId`; only true
// navigation/links keep using the raw `eventQueryId`.
const eventQueryId = computed(() => {
  const q = route.query.event
  return typeof q === 'string' && q !== '' ? q : null
})

// Set from the fetchGame payload's `eventId` (null for ordinary games). Lets a
// group game engage its event even when the URL is missing ?event.
const gamePayloadEventId = ref<string | null>(null)

const resolvedEventId = computed(() => eventQueryId.value ?? gamePayloadEventId.value)

const organizerEventId = computed(() => {
  const eid = resolvedEventId.value
  if (!eid) return null
  const ev = eventStore.event
  return ev && ev.id === eid && ev.role === 'organizer' ? eid : null
})

// Player-facing counterpart: any seated MEMBER (not the organizer) of an Epic
// event may switch to / spectate the sibling groups. NOT gated on the local dev
// flag — invited players don't have it set, but their game is still part of the
// event server-side. Engages purely on the loaded event containing this gameId
// (mirrors organizerEventId). Events can only be CREATED with the dev flag, so
// there are no event games in production regardless. Organizer keeps OrganizerBar.
const playerEventId = computed(() => {
  const eid = resolvedEventId.value
  if (!eid) return null
  const ev = eventStore.event
  if (!ev || ev.id !== eid || ev.role === 'organizer') return null
  return ev.groups.some((g) => g.gameId === props.gameId) ? eid : null
})

watch(
  resolvedEventId,
  (eid) => {
    if (!eid) return
    if (eventStore.event?.id === eid) return
    eventStore.load(eid).catch((e) => console.error(e))
  },
  { immediate: true },
)

// "Epic Multiplayer" time limit. The event id this game view actively
// PARTICIPATES in for the timer: a seated player (or an organizer playing a
// seat), never the organizer's spectate/non-playing view. NOT gated on the local
// dev flag (invited players don't have it) — engages purely on the loaded event
// containing this game as a group, so ordinary games never engage.
const timerEventId = computed(() => {
  if (props.spectate) return null
  const eid = resolvedEventId.value
  if (!eid) return null
  const ev = eventStore.event
  if (!ev || ev.id !== eid) return null
  return ev.groups.some((g) => g.gameId === props.gameId) ? eid : null
})

const { hasTimeLimit, barrierPending, timerStartedAt, timeUp } = useEventTimer()

// Whether an epic bar (organizer or player) is mounted above the board.
const hasEventBar = computed(() => !!organizerEventId.value || !!playerEventId.value)

// Reserve the epic bar's height in the board layout. `.game-main` is sized off a
// hardcoded `calc(100vh - 80px)`; the bar adds height ABOVE it, so without this
// the board's bottom (player area) is pushed past the viewport and clipped.
// Measured (not a fixed constant) so it stays correct if the bar wraps/changes,
// and it defaults to 0 for ordinary, non-event games — no layout shift for them.
const epicBarRef = ref<HTMLElement | null>(null)
const epicBarHeight = ref(0)
useResizeObserver(epicBarRef, () => {
  epicBarHeight.value = epicBarRef.value?.offsetHeight ?? 0
})
watch(hasEventBar, (present) => {
  if (!present) epicBarHeight.value = 0
})

const preloaded = new Set<string>()
const preloading = new Set<string>()
let mouseX = 0
let mouseY = 0
let focusLightObserver: MutationObserver | null = null
let focusLightAnimationFrame: number | null = null
const flashlightX = ref(0)
const flashlightY = ref(0)
const focusLightX = ref(-1000)
const focusLightY = ref(-1000)

store.fetchCards()

interface PlayabilityInfo {
  cardId: string
  cardCode: string
  checks: [string, string | null][]
}

const game = shallowRef<Arkham.Game | null>(null)

// "Ready to play": the group has reached the first investigation phase of an
// active, started scenario. Cleanest signal we have off the existing game state.
const reachedInvestigation = computed(() => {
  const g = game.value
  return (
    !!g &&
    g.gameState.tag === 'IsActive' &&
    !!g.scenario?.started &&
    g.phase === 'InvestigationPhase'
  )
})

// Show the blocking start-barrier overlay only once this group has finished its own
// setup (reached investigation) and is waiting on the other groups. Gating on
// reachedInvestigation is essential: blocking the board during deck selection /
// mulligan would stop the player from ever reaching investigation -> deadlock.
const showStartBarrier = computed(
  () => !!timerEventId.value && barrierPending.value && reachedInvestigation.value,
)

// Stage (1/2/3) of the act currently in play for this group, fed to the epic bars'
// shared-pool readout. The Blob has a single act deck, so the lone act in
// `game.acts` is the current one; its sequence number is the stage.
const currentActStage = computed<number | null>(() => {
  const acts = game.value ? Object.values(game.value.acts) : []
  return acts.length > 0 ? acts[0].sequence.number : null
})

// Park an actively-playing member of this event's group behind a wait overlay while
// the shared act advance for their current stage awaits the organizer's allocation.
// Lifts as soon as the `awaiting-organizer:<stage>` gate clears (the backend pushes
// the cleared shared state over the ws), surfacing the group's parked "Continue"
// question for the player to click — mirrors EventStartBarrier's release.
const showActAdvanceWait = computed(
  () =>
    !!timerEventId.value &&
    currentActStage.value !== null &&
    awaitingOrganizer(eventStore.sharedState, currentActStage.value) > 0,
)

// Mark this group ready at the start barrier exactly once per load. Guarded with a
// local flag (the endpoint is idempotent server-side regardless). Immediate so a
// reconnect mid-investigation still signals readiness.
let markedReady = false
watch(
  [timerEventId, reachedInvestigation, barrierPending],
  () => {
    if (markedReady) return
    const eid = timerEventId.value
    if (!eid) return
    if (!hasTimeLimit.value) return
    if (timerStartedAt.value !== 0) return
    if (!reachedInvestigation.value) return
    markedReady = true
    markEventReady(eid).catch((e) => console.error(e))
  },
  { immediate: true },
)

// When the countdown hits 0, force the time-up resolution once. Fires from any
// loaded event game (player or spectating organizer); the endpoint is idempotent
// across clients.
let timeUpFired = false
watch(
  timeUp,
  (up) => {
    if (!up || timeUpFired) return
    const eid = resolvedEventId.value
    if (!eid || !hasTimeLimit.value) return
    timeUpFired = true
    eventTimeUp(eid).catch((e) => console.error(e))
  },
  { immediate: true },
)

const gameCard = ref<GameCard | null>(null)
const showTheSilenceModal = ref(false)
const playabilityInfo = ref<PlayabilityInfo | null>(null)
const gameLog = shallowRef<readonly string[]>(Object.freeze([]))
const playerId = ref<string | null>(null)
const ready = ref(false)
const resultQueue = ref<any>([])
const showLog = ref(false)
const showShortcuts = ref(false)
const isMobileViewport = () =>
  typeof window !== 'undefined' && window.matchMedia('(max-width: 800px)').matches
const showSidebar = ref(
  isMobileViewport() ? false : JSON.parse(getGameLocalStorageItem(props.gameId, 'showSidebar') ?? 'true'),
)
const socketError = ref(false)
const error = ref<string | null>(null)
const solo = ref(false)
const soundsDisabled = ref(localStorage.getItem('arkhamSoundsDisabled') === 'true')
const showOtherPlayersHands = ref(getGameLocalStorageItem(props.gameId, 'showOtherPlayersHands') === 'true')
watch(showOtherPlayersHands, (v) => {
  setGameLocalStorageItem(props.gameId, 'showOtherPlayersHands', v ? 'true' : 'false')
})
const tarotCards = ref<TarotCard[]>([])
const uiLock = ref<boolean>(false)
const showSettings = ref(false)
const showHistory = ref(false)
const processing = ref(false)
const oldQuestion = ref<Record<string, Question> | null>(null)
const skipAllPending = ref<Set<string>>(new Set())
const { t } = useI18n()

const format = (str: string) => {
  return handleEmbeddedI18n(str, t)
}

function handleSettingChange(event: Event) {
  const detail = (event as CustomEvent<{ key?: string; value?: string }>).detail
  if (detail?.key === 'arkhamSoundsDisabled') {
    soundsDisabled.value = detail.value === 'true'
  }
}

function updateGameLog(nextLog: readonly string[]) {
  const currentLog = gameLog.value
  if (
    currentLog.length === nextLog.length &&
    currentLog[0] === nextLog[0] &&
    currentLog[currentLog.length - 1] === nextLog[nextLog.length - 1]
  ) {
    return
  }

  gameLog.value = Object.freeze([...nextLog])
}

addEntry({
  id: 'viewSettings',
  icon: AdjustmentsHorizontalIcon,
  content: t('gameBar.viewSettings'),
  shortcut: 'S',
  nested: 'view',
  action: () => (showSettings.value = !showSettings.value),
})

addEntry({
  id: 'viewHistory',
  icon: ClockIcon,
  content: t('gameBar.viewHistory'),
  shortcut: 'H',
  nested: 'view',
  action: () => (showHistory.value = !showHistory.value),
})

// Computed
const cards = computed(() => store.cards)
const choicesByPlayer = computed(() => {
  const currentGame = game.value
  if (!currentGame) return new Map<string, readonly Message.Message[]>()

  return new Map(
    Object.keys(currentGame.question).map((pid) => [pid, ArkhamGame.choices(currentGame, pid)]),
  )
})
const choicesSourceByPlayer = computed(() => {
  const currentGame = game.value
  if (!currentGame) return new Map<string, Source | null>()

  return new Map(
    Object.keys(currentGame.question).map((pid) => [pid, ArkhamGame.choicesSource(currentGame, pid)]),
  )
})
const choicesTooltipByPlayer = computed(() => {
  const currentGame = game.value
  if (!currentGame) return new Map<string, string | null>()

  return new Map(
    Object.keys(currentGame.question).map((pid) => [pid, ArkhamGame.choicesTooltip(currentGame, pid)]),
  )
})
const gameIndexes = computed(() => buildGameIndexes(game.value))
const choices = computed(() => {
  if (!playerId.value) return []
  return choicesByPlayer.value.get(playerId.value) ?? []
})
const gameOver = computed(() => game.value?.gameState.tag === 'IsOver')
const question = computed(() => (playerId.value ? game.value?.question[playerId.value] : null))

function questionTag(q: Question | null | undefined): string | null {
  if (!q) return null
  if (q.tag === 'QuestionLabel') return q.question.tag
  return q.tag
}

const isActualScenarioView = computed(() => {
  const g = game.value
  if (!g?.scenario) return false
  if (g.gameState.tag !== 'IsActive' && g.gameState.tag !== 'IsOver') return false
  if (!g.scenario.started || g.scenario.campaignStep) return false
  if (Object.entries(g.investigators).length === 0) return false

  const activeQuestionTag = questionTag(question.value)
  return activeQuestionTag !== 'ChooseUpgradeDeck'
    && activeQuestionTag !== 'ChooseDeck'
    && activeQuestionTag !== 'PickScenarioSettings'
    && activeQuestionTag !== 'PickCampaignSettings'
    && activeQuestionTag !== 'ContinueCampaign'
})

const realityAcidLightOverride = ref<boolean | null>(null)
const realityAcidLightMetaActive = computed(() => {
  const scenario = game.value?.scenario
  return scenario?.id === 'c85001' && scenario.meta?.lightActive === true
})

const realityAcidLightActive = computed(() => realityAcidLightOverride.value ?? realityAcidLightMetaActive.value)

watch(realityAcidLightMetaActive, () => {
  realityAcidLightOverride.value = null
})

watch(question, async () => {
  await nextTick()
  updateFocusLight()
})

const realityAcidLightDevoured = computed(() => {
  const scenario = game.value?.scenario
  if (scenario?.id !== 'c85001') return false
  return realityAcidLightMetaActive.value || scenario.meta?.lightDevoured === true || realityAcidLightOverride.value !== null
})

const toggleRealityAcidLight = () => {
  const gameId = game.value?.id
  if (!gameId) return
  const active = !realityAcidLightActive.value
  realityAcidLightOverride.value = active
  debug.send(gameId, {
    tag: 'ScenarioSpecific',
    contents: ['blobSetLightActive', active],
  })
}

const activePlayerId = computed(() => game.value?.activePlayerId ?? null)

function activePlayerBelongsToCurrentPlayer(g: Arkham.Game, currentPlayerId: string) {
  if (g.activePlayerId === currentPlayerId) return true
  return Object.values(g.investigators).some(
    (investigator) => investigator.id === g.activePlayerId && investigator.playerId === currentPlayerId,
  )
}

watch(activePlayerId, (newActivePlayerId, oldActivePlayerId) => {
  if (!newActivePlayerId || !oldActivePlayerId || newActivePlayerId === oldActivePlayerId) return
  if (props.spectate || solo.value) return
  if (!game.value || game.value.playerCount < 2 || !playerId.value) return
  if (!activePlayerBelongsToCurrentPlayer(game.value, playerId.value)) return

  playAudioFile('turnIndicator.ogg')
})

// --- "AI asks questions" fetch trigger (dev-only) ----------------------------
// On a genuine old->new turn-start edge where the new active seat is an AI seat,
// pull the AI's pending questions and merge them into the store. Gated on the
// dev flag; guarded to the turn-start edge so it never refetch-spams. AI-target
// questions are auto-resolved here; human-target ones render in AiQuestionsPanel.
watch(activePlayerId, (newActivePlayerId, oldActivePlayerId) => {
  if (!aiDevEnabled.value || props.spectate) return
  if (!newActivePlayerId || !oldActivePlayerId || newActivePlayerId === oldActivePlayerId) return
  const g = game.value
  if (!g) return
  if (!isInvestigatorTurn(g)) return
  if (!(newActivePlayerId in g.settings.aiPlayers)) return

  Api.fetchAiQuestions(g.id)
    .then((qs) => {
      ai.mergeQuestions(qs, g.scenarioSteps)
      resolveAiTargetQuestions()
    })
    .catch((e) => console.error(e))
})

// A skill test opening is another moment an AI can offer help: committing a card
// to the performer's test (offerCommit). Fetch when a test opens, regardless of
// whose turn it is, so an AI can offer to boost a (human or AI) performer.
watch(() => (game.value?.skillTest ?? null) !== null, (hasTest, hadTest) => {
  if (!hasTest || hadTest) return
  if (!aiDevEnabled.value || props.spectate) return
  const g = game.value
  if (!g) return
  if (Object.keys(g.settings.aiPlayers).length === 0) return

  Api.fetchAiQuestions(g.id)
    .then((qs) => {
      ai.mergeQuestions(qs, g.scenarioSteps)
      resolveAiTargetQuestions()
    })
    .catch((e) => console.error(e))
})

// Auto-resolve any AI-target question that carries a precomputed answer: replay
// its chosen option's RAW config Messages over the debug channel and drop it from
// the store so it never renders. Human-target questions are left for the panel.
function resolveAiTargetQuestions() {
  const g = game.value
  if (!g) return
  for (const q of [...ai.questions]) {
    if (!q.toIsAi || q.aiAnswer === null) continue
    const option = q.options[q.aiAnswer]
    if (option) {
      for (const message of option.messages) debug.send(g.id, message)
    }
    ai.dismissQuestion(q.id)
  }
}

type SkipTriggerEntry = { playerId: string; choiceIdx: number; investigatorId: string }

function skipTriggerEntries(g: Arkham.Game): SkipTriggerEntry[] {
  const result: SkipTriggerEntry[] = []
  for (const pid of Object.keys(g.question)) {
    const cs = ArkhamGame.choices(g, pid)
    const idx = cs.findIndex((c) => c.tag === Message.MessageType.SKIP_TRIGGERS_BUTTON)
    const choice = idx === -1 ? null : cs[idx]
    if (choice?.tag === Message.MessageType.SKIP_TRIGGERS_BUTTON) {
      result.push({ playerId: pid, choiceIdx: idx, investigatorId: choice.investigatorId })
    }
  }
  return result
}

function investigatorBelongsToPlayer(g: Arkham.Game, investigatorId: string, targetPlayerId: string) {
  return g.investigators[investigatorId]?.playerId === targetPlayerId
}

function isInvestigatorTurn(g: Arkham.Game) {
  return g.phaseStep?.tag === 'InvestigationPhaseStep'
    && [
      'NextInvestigatorsTurnBeginsStep',
      'NextInvestigatorsTurnBeginsWindow',
      'InvestigatorTakesActionStep',
      'InvestigatorsTurnEndsStep',
    ].includes(g.phaseStep.contents)
}

function canCurrentPlayerSkipAllWindows(g: Arkham.Game, currentPlayerId: string) {
  if (solo.value) return true

  if (g.skillTest) {
    return investigatorBelongsToPlayer(g, g.skillTest.investigator, currentPlayerId)
  }

  if (isInvestigatorTurn(g)) {
    return investigatorBelongsToPlayer(g, g.activeInvestigatorId, currentPlayerId)
  }

  return true
}

function authorizedSkipTriggerEntries(g: Arkham.Game): SkipTriggerEntry[] {
  if (!playerId.value) return []
  if (!canCurrentPlayerSkipAllWindows(g, playerId.value)) return []
  return skipTriggerEntries(g)
}

const skipAllAvailable = computed(() => {
  if (!game.value) return false
  if (skipAllPending.value.size > 0) return true

  const entries = authorizedSkipTriggerEntries(game.value)
  const distinct = new Set(entries.map((entry) => entry.playerId))
  if (distinct.size > 1) return true
  // The authorized player (e.g. the skill-test owner) may be waiting on a
  // single other player's fast trigger with no window of their own to skip;
  // let them skip that lone window too. Solo keeps the stricter rule.
  return !solo.value && distinct.size === 1 && !distinct.has(playerId.value ?? '')
})

const skipAllInProgress = computed(() => skipAllPending.value.size > 0)

function setGameQuestion(question: Record<string, Question>) {
  if (!game.value) return
  game.value = { ...game.value, question }
}

const websocketUrl = computed(() => {
  const spectatePrefix = props.spectate ? '/spectate' : ''
  return buildWebsocketUrl(`/api/v1/arkham/games/${props.gameId}${spectatePrefix}`, userStore.token)
})

watch(
  // Also react to `spectate`: the same Game.vue instance is reused when an
  // organizer toggles between the Spectate (organizer) and Game (play-my-seat)
  // routes for one gameId, so we must re-fetch in the new mode to pick up the
  // player's seat/question (or drop them when spectating again).
  () => [props.gameId, props.spectate] as const,
  async (newVals, oldVals) => {
    const [newId] = newVals
    if (!newId) return
    if (oldVals && newId === oldVals[0] && newVals[1] === oldVals[1]) return
    await fetchGame(props.gameId, props.spectate).then(
      async ({ game: newGame, playerId: newPlayerId, multiplayerMode, eventId }) => {
        preloadImages(newGame)
        ;(window as Window & { g?: Arkham.Game }).g = newGame
        game.value = newGame
        solo.value = multiplayerMode === 'Solo'
        // Engage the Epic event this game belongs to even when the URL lacks
        // ?event (e.g. entered via the join / take-a-seat path).
        gamePayloadEventId.value = eventId
        updateGameLog(newGame.log)
        playerId.value = newPlayerId
        ready.value = true
      },
    )
  },
  { immediate: true },
)

// Local Decoders
const gameCardDecoder = JsonDecoder.object<GameCard>(
  {
    title: JsonDecoder.string(),
    card: cardDecoder,
  },
  'GameCard',
)

const gameCardOnlyDecoder = JsonDecoder.object<GameCardOnly>(
  {
    player: JsonDecoder.string(),
    title: JsonDecoder.string(),
    card: cardDecoder,
  },
  'GameCard',
)

// Socket Handling
const onError = () => {
  processing.value = false
  if (game.value && oldQuestion.value) {
    setGameQuestion(oldQuestion.value)
  }
  socketError.value = true
}
const onConnected = () => {
  socketError.value = false
  processing.value = false
}

const onMessage = (_ws: WebSocket, event: MessageEvent) => {
  const result = JSON.parse(event.data)
  handleResult(result)
  oldQuestion.value = null
}

let qHead = 0
const qPush = (x: any) => {
  resultQueue.value.push(x)
}
const qPop = () => {
  if (qHead >= resultQueue.value.length) {
    resultQueue.value = []
    qHead = 0
    return undefined
  }
  return resultQueue.value[qHead++]
}
let decoding = false
let pendingUpdate: string | null = null

function scheduleApplyUpdate(payload: string) {
  if (decoding) {
    pendingUpdate = payload
    return
  }
  decoding = true
  Arkham.gameDecoder
    .decodePromise(payload)
    .then((updatedGame) => {
      const locked = uiLock.value
      // Behind a revelation: refresh the board but keep the question hidden so the
      // player can't act until they dismiss it. On unlock the queued GameUpdate is
      // replayed (locked === false) and restores the real question + side effects.
      game.value = locked ? { ...updatedGame, question: {} } : updatedGame
      updateGameLog(updatedGame.log)
      preloadImages(updatedGame)
      if (!locked) {
        if (solo.value === true) {
          if (Object.keys(game.value.question).length == 1) {
            playerId.value = Object.keys(game.value.question)[0]
          } else if (game.value.activePlayerId !== playerId.value) {
            if (playerId.value && Object.keys(game.value.question).includes(playerId.value)) {
              playerId.value = game.value.activePlayerId
            } else {
              playerId.value = Object.keys(game.value.question)[0]
            }
          } else if (playerId.value && !Object.keys(game.value.question).includes(playerId.value)) {
            playerId.value = Object.keys(game.value.question)[0]
          }
        }
        continueSkipAll()
      }
    })
    .finally(() => {
      decoding = false
      if (pendingUpdate) {
        const p = pendingUpdate
        pendingUpdate = null
        scheduleApplyUpdate(p)
      }
    })
}

function playAudioFile(fileName: string) {
  if (soundsDisabled.value) return
  // Only allow simple filenames from the server; audio files live under public/audio.
  if (!/^[a-zA-Z0-9_.-]+\.(ogg|mp3|wav)$/i.test(fileName)) return

  const audio = new Audio(`/audio/${fileName}`)
  audio.play().catch((error) => console.warn(`Unable to play audio file: ${fileName}`, error))
}

function continueSkipAll() {
  if (skipAllPending.value.size === 0) return
  if (!game.value) return
  const next = authorizedSkipTriggerEntries(game.value).find((e) => skipAllPending.value.has(e.playerId))
  if (!next) {
    skipAllPending.value = new Set()
    return
  }
  sendSkipFor(next.playerId, next.choiceIdx)
}

function sendSkipFor(targetPlayerId: string, choiceIdx: number) {
  if (!game.value || props.spectate) return
  oldQuestion.value = game.value.question
  const questionVersion = game.value.scenarioSteps
  setGameQuestion({})
  processing.value = true
  send(
    JSON.stringify({
      tag: 'Answer',
      contents: { choice: choiceIdx, playerId: targetPlayerId, questionVersion },
    }),
  )
}

function skipAllTriggers() {
  if (!game.value || props.spectate) return
  if (skipAllPending.value.size > 0) {
    if (!processing.value) continueSkipAll()
    return
  }

  const entries = authorizedSkipTriggerEntries(game.value)
  if (entries.length === 0) return
  skipAllPending.value = new Set(entries.map((e) => e.playerId))
  const first = entries[0]
  sendSkipFor(first.playerId, first.choiceIdx)
}

const { send, close } = useWebSocket(websocketUrl, {
  autoReconnect: true,
  onError,
  onConnected,
  onMessage,
})

// --- AI-investigator driver (dev-only) ---------------------------------------
// For each parked question belonging to an enabled AI seat, schedule (after that
// seat's response delay) an `AiAnswer` over this same websocket; the backend
// computes and applies the AI's move. Manual override always works: the creator
// clicking a normal choice for an AI seat (solo mode lets one tab answer any
// seat) just resolves it via the existing `choose` path.

// Setup/lobby questions the AI must never touch (it has no decision model for
// these). Tags are read after unwrapping QuestionLabel/PayCostQuestion/QuestionWithSource.
const AI_SETUP_DENYLIST = new Set<string>([
  'ChooseDeck',
  'ChooseUpgradeDeck',
  'PickScenarioSettings',
  'PickCampaignSettings',
  'PickCampaignSpecific',
  'PickScenarioSpecific',
  'ContinueCampaign',
  'PickDestiny',
])

// Pending scheduled sends, keyed by playerId; tracks the questionVersion the send
// was armed for so a question change cancels/reschedules instead of firing stale.
const aiScheduled = new Map<string, { version: number; timer: ReturnType<typeof setTimeout> }>()
// The (playerId -> questionVersion) we last actually sent an AiAnswer for. Drives
// the loop-guard: if the same (seat, version) is still pending after our send, the
// AI couldn't resolve it, so we stop and hand it to the human.
const aiSentVersion = new Map<string, number>()
// Reactive set of AI seats currently "stuck" (handed back to the human creator).
const aiStuckSeats = ref<Set<string>>(new Set())

// All configured AI seats (used to mount the dev panel); the driver further
// filters to enabled seats.
const aiSeatIds = computed(() =>
  game.value ? Object.keys(game.value.settings.aiPlayers) : [],
)

function innerQuestionTag(q: Question | undefined): string | null {
  let cur: Question | undefined = q
  while (
    cur &&
    (cur.tag === 'QuestionLabel' || cur.tag === 'PayCostQuestion' || cur.tag === 'QuestionWithSource')
  ) {
    cur = 'question' in cur ? cur.question : undefined
  }
  return cur ? cur.tag : null
}

function enabledAiSeats(g: Arkham.Game): string[] {
  const seats = g.settings.aiPlayers
  return Object.keys(seats).filter((pid) => seats[pid]?.aiEnabled)
}

// The investigator id seated at an AI playerId (AI seats map to an investigator
// via investigator.playerId), or null if that seat isn't seated yet.
function aiSeatInvestigatorId(g: Arkham.Game, pid: string): string | null {
  for (const investigator of Object.values(g.investigators)) {
    if (investigator.playerId === pid) return investigator.id
  }
  return null
}

// A skill-test ASSIST commit window for an AI seat: there is an active skill
// test, the seat has a parked question, and the seat is NOT the performer (the
// performer's own AI commit window is driven normally by the backend). The
// backend's AiAnswer driver loops on these assist windows, so we leave them
// parked and surface the dev "Request assist" button instead (AiControlPanel).
function isAiAssistWindow(g: Arkham.Game, pid: string): boolean {
  if (!g.skillTest) return false
  if (!(pid in g.question)) return false
  const invId = aiSeatInvestigatorId(g, pid)
  return invId !== null && invId !== g.skillTest.investigator
}

function cancelAiTimer(pid: string) {
  const sched = aiScheduled.get(pid)
  if (sched) {
    clearTimeout(sched.timer)
    aiScheduled.delete(pid)
  }
}

function cancelAllAiTimers() {
  for (const { timer } of aiScheduled.values()) clearTimeout(timer)
  aiScheduled.clear()
}

function setAiStuck(pid: string, stuck: boolean) {
  if (stuck === aiStuckSeats.value.has(pid)) return
  const next = new Set(aiStuckSeats.value)
  if (stuck) next.add(pid)
  else next.delete(pid)
  aiStuckSeats.value = next
}

function driveAi() {
  // Flag off (or spectating): stand down and clear any armed sends.
  if (!aiDevEnabled.value || props.spectate) {
    cancelAllAiTimers()
    return
  }
  const g = game.value
  if (!g) {
    cancelAllAiTimers()
    return
  }

  // Master kill-switch off, or not in active play (setup/lobby/over): stand down.
  if (!ai.enabled || g.gameState.tag !== 'IsActive') {
    cancelAllAiTimers()
    return
  }

  const seats = enabledAiSeats(g)
  if (seats.length === 0) {
    cancelAllAiTimers()
    return
  }

  const version = g.scenarioSteps

  // Drop scheduled sends for seats no longer pending / no longer AI-enabled.
  for (const pid of [...aiScheduled.keys()]) {
    if (!(pid in g.question) || !seats.includes(pid)) cancelAiTimer(pid)
  }
  // Clear stale stuck flags once a seat's question clears or its version advances.
  for (const pid of [...aiStuckSeats.value]) {
    if (!(pid in g.question) || aiSentVersion.get(pid) !== version) setAiStuck(pid, false)
  }

  for (const pid of seats) {
    const q = g.question[pid]
    if (!q) continue

    const tag = innerQuestionTag(q)
    if (tag && AI_SETUP_DENYLIST.has(tag)) continue

    // Skill-test ASSIST window: the backend AiAnswer driver loops on a teammate
    // AI's commit window during another investigator's test. Never auto-answer
    // it and never mark it "stuck" — leave it parked for the human / the dev
    // "Request assist" button. Cancel any send already armed before the test.
    if (isAiAssistWindow(g, pid)) {
      cancelAiTimer(pid)
      continue
    }

    // Loop-guard: we already auto-answered this exact (seat, version) and it is
    // STILL pending -> the AI couldn't resolve this question shape. Mark the seat
    // stuck and stop auto-answering it; the human creator answers it manually.
    // Normal auto-answering resumes once the version advances.
    if (aiSentVersion.get(pid) === version) {
      setAiStuck(pid, true)
      cancelAiTimer(pid)
      continue
    }
    setAiStuck(pid, false)

    const existing = aiScheduled.get(pid)
    if (existing) {
      if (existing.version === version) continue // already armed for this version
      cancelAiTimer(pid) // version moved on -> reschedule against the current one
    }

    const delay = g.settings.aiPlayers[pid]?.aiResponseDelayMs ?? 1500
    const timer = setTimeout(() => {
      aiScheduled.delete(pid)
      const cur = game.value
      // Re-validate at fire time so a question change/clear, a state change, a
      // disabled seat, or a paused master switch cancels the stale send.
      if (!cur || !ai.enabled || props.spectate) return
      if (cur.gameState.tag !== 'IsActive') return
      if (cur.scenarioSteps !== version) return
      if (!(pid in cur.question)) return
      if (!enabledAiSeats(cur).includes(pid)) return
      // A skill test that opened after this send was armed turns the seat's
      // question into an assist window; don't fire AiAnswer into it (it loops).
      if (isAiAssistWindow(cur, pid)) return
      aiSentVersion.set(pid, version)
      send(JSON.stringify({ tag: 'AiAnswer', playerId: pid }))
    }, Math.max(0, delay))
    aiScheduled.set(pid, { version, timer })
  }
}

// Re-evaluate whenever the game updates (every server push reassigns game.value)
// and whenever the client master switch flips.
watch(game, () => {
  // Drop "AI asks questions" entries that predate the current game state (undo,
  // or advancing past the window they belonged to).
  if (game.value) ai.clearStale(game.value.scenarioSteps)
  driveAi()
})
watch(() => ai.enabled, () => driveAi())
// Toggling the dev "AI Investigators" flag mid-session stands the driver down /
// brings it back up immediately (the AiControlPanel mount is reactive on its own).
watch(aiDevEnabled, (enabled) => {
  if (!enabled) ai.clearQuestions()
  driveAi()
})
const handleResult = (result: ServerResult) => {
  processing.value = false
  switch (result.tag) {
    case 'GameError':
      if (props.spectate) return
      error.value = result.contents
      if (game.value && oldQuestion.value) {
        setGameQuestion(oldQuestion.value)
      }
      return
    case 'GameMessage':
      gameLog.value = Object.freeze([...gameLog.value, localize(result.contents)])
      return
    case 'GameShowDiscard':
      emitter.emit('showDiscards', result.contents)
      return
    case 'GameShowUnder':
      emitter.emit('showUnder', result.contents)
      return
    case 'GameAudio':
      playAudioFile(result.contents)
      return
    case 'GameUI':
      if (result.contents.startsWith('theSilence:')) {
        if (props.spectate) return
        const targetPlayer = result.contents.slice('theSilence:'.length)
        if (!(solo.value === true || targetPlayer === playerId.value)) return
        if (uiLock.value) {
          qPush(result)
          return
        }
        document.dispatchEvent(new CustomEvent('arkham:clear-card-overlay'))
        showTheSilenceModal.value = true
        uiLock.value = true
        return
      }
      switch (result.contents) {
        case 'confetti': {
          setTimeout(() => {
            var count = 500
            var defaults = {
              origin: { y: 0.7 },
            }

            function fire(particleRatio: number, opts: Parameters<typeof confetti>[0]) {
              confetti({
                ...defaults,
                ...opts,
                particleCount: Math.floor(count * particleRatio),
              })
            }

            fire(0.25, {
              spread: 26,
              startVelocity: 55,
            })
          }, 500)
        }
        default:
          return
      }
    case 'GameTarot':
      if (props.spectate) return
      if (uiLock.value) {
        qPush(result)
        return
      }

      uiLock.value = true
      JsonDecoder.array(tarotCardDecoder, 'tarotCards')
        .decodePromise(result.contents)
        .then((r) => {
          tarotCards.value = r
        })
        .catch((e) => {
          console.error(e)
          uiLock.value = false
        })
      return

    case 'GameCard':
      if (props.spectate) return
      if (uiLock.value) {
        qPush(result)
        return
      }

      uiLock.value = true
      gameCardDecoder
        .decodePromise(result as any)
        .then((r) => {
          gameCard.value = r
        })
        .catch((e) => {
          console.error(e)
          uiLock.value = false
        })
      return

    case 'GameCardOnly':
      if (props.spectate) return
      if (uiLock.value) {
        qPush(result)
        return
      }

      uiLock.value = true
      gameCardOnlyDecoder
        .decodePromise(result as any)
        .then((r) => {
          // if it isn't for us, immediately unlock and continue draining
          if (!(solo.value === true || r.player === playerId.value)) {
            uiLock.value = false
            return
          }
          gameCard.value = r
        })
        .catch((e) => {
          console.error(e)
          uiLock.value = false
        })
      return
    case 'SharedStateUpdate':
      // "Epic Multiplayer" shared-state feed riding on this group's game ws.
      // Forward it to the event store so the organizer bar's shared counters stay
      // live; harmless no-op for ordinary games that never receive this tag.
      eventStore.applySharedState(result.contents)
      return
    case 'GameUpdate':
      // Flush the latest state onto the board even while a revelation/modal holds
      // the UI lock, so the table behind it reflects the current situation instead
      // of freezing on the pre-revelation state (issue #4817). Keep it queued so
      // the pending question is only restored once every revelation is dismissed.
      if (uiLock.value) qPush(result)
      scheduleApplyUpdate(result.contents)
      return
  }
}

watch(uiLock, async () => {
  if (uiLock.value) return
  // drain result queue
  for (;;) {
    const r = qPop()
    if (!r) break
    handleResult(r)
    if (uiLock.value) break
  }
})

const confirmingUndoScenario = ref(false)

const actionMap = computed<Map<string, () => void>>(() => {
  const map = new Map<string, () => void>()
  for (const item of menuItems.value) {
    if (item.shortcut) map.set(item.shortcut, item.action)
  }
  return map
})

const canUndoScenario = computed(() => {
  if (!game.value) return false
  return game.value.scenarioSteps > 1
})

const canUndoBoundary = (boundary: number | null): boolean => {
  if (!game.value) return false
  if (boundary === null) return false
  return game.value.scenarioSteps > boundary
}

const canUndoAction = computed(() => canUndoBoundary(game.value?.undoActionStep ?? null))
const canUndoTurn = computed(() => canUndoBoundary(game.value?.undoTurnStep ?? null))
const canUndoPhase = computed(() => canUndoBoundary(game.value?.undoPhaseStep ?? null))
const canUndoRound = computed(() => canUndoBoundary(game.value?.undoRoundStep ?? null))

// Chord state for U + <key> shortcuts (T/R/P/S/A)
const undoChordArmed = ref(false)
let undoChordTimer: number | null = null
const UNDO_CHORD_TIMEOUT_MS = 1500

const armUndoChord = () => {
  undoChordArmed.value = true
  if (undoChordTimer) clearTimeout(undoChordTimer)
  undoChordTimer = window.setTimeout(() => {
    undoChordArmed.value = false
    undoChordTimer = null
  }, UNDO_CHORD_TIMEOUT_MS)
}

const clearUndoChord = () => {
  undoChordArmed.value = false
  if (undoChordTimer) {
    clearTimeout(undoChordTimer)
    undoChordTimer = null
  }
}

// --- Konami Code support ---
const KONAMI_SEQ = [
  'ArrowUp',
  'ArrowUp',
  'ArrowDown',
  'ArrowDown',
  'ArrowLeft',
  'ArrowRight',
  'ArrowLeft',
  'ArrowRight',
  'b',
  'a',
] as const

let konamiIndex = 0
let konamiTimer: number | null = null
const KONAMI_TIMEOUT_MS = 5000 // reset if user pauses too long

const onKonami = () => {
  if (!game.value) return
  debug.send(game.value.id, { tag: 'KonamiCode', contents: playerId.value })
}

const feedKonami = (rawKey: string): boolean => {
  const key = rawKey.length === 1 ? rawKey.toLowerCase() : rawKey

  // match current step
  if (key === KONAMI_SEQ[konamiIndex]) {
    konamiIndex++
    if (konamiIndex === KONAMI_SEQ.length) {
      // success!
      konamiIndex = 0
      if (konamiTimer) {
        clearTimeout(konamiTimer)
        konamiTimer = null
      }
      onKonami()
      return true
    }
    // keep a rolling timeout while the user is entering
    if (konamiTimer) clearTimeout(konamiTimer)
    konamiTimer = window.setTimeout(() => {
      konamiIndex = 0
      konamiTimer = null
    }, KONAMI_TIMEOUT_MS)
    return false
  }

  // mismatch: allow overlap if this key is the first symbol of the sequence
  if (key === KONAMI_SEQ[0]) {
    konamiIndex = 1
    if (konamiTimer) clearTimeout(konamiTimer)
    konamiTimer = window.setTimeout(() => {
      konamiIndex = 0
      konamiTimer = null
    }, KONAMI_TIMEOUT_MS)
  } else {
    konamiIndex = 0
    if (konamiTimer) {
      clearTimeout(konamiTimer)
      konamiTimer = null
    }
  }

  return false
}

// Keyboard Shortcuts
const handleKeyPress = (event: KeyboardEvent) => {
  if (filingBug.value) return
  if (event.ctrlKey) return
  if (event.metaKey) return
  if (event.altKey) return

  if (feedKonami(event.key)) return

  // Chord: when U is armed, the next key chooses the undo level
  if (undoChordArmed.value) {
    const k = event.key.toLowerCase()
    if (k === 'a' && canUndoAction.value) {
      clearUndoChord()
      undoActionStart()
      return
    }
    if (k === 't' && canUndoTurn.value) {
      clearUndoChord()
      undoTurnStart()
      return
    }
    if (k === 'p' && canUndoPhase.value) {
      clearUndoChord()
      undoPhaseStart()
      return
    }
    if (k === 'r' && canUndoRound.value) {
      clearUndoChord()
      undoRoundStart()
      return
    }
    if (k === 's' && canUndoScenario.value) {
      clearUndoChord()
      confirmingUndoScenario.value = true
      return
    }
    // Pressing U again while armed = single undo (re-pressing the prefix)
    if (k === 'u') {
      clearUndoChord()
      undo()
      return
    }
    // Any other key cancels the chord and falls through
    clearUndoChord()
  }

  if (event.key === 'u') {
    undo()
    return
  }

  if (event.key === 'U') {
    armUndoChord()
    return
  }

  if (event.key === 'D') {
    debug.toggle()
    return
  }

  if (event.key === '?') {
    showShortcuts.value = !showShortcuts.value
    return
  }

  if (event.key === ' ' || event.code === 'Space') {
    event.preventDefault()

    const skipTriggers = choices.value.findIndex(
      (c) => c.tag === Message.MessageType.SKIP_TRIGGERS_BUTTON,
    )
    if (skipTriggers !== -1) {
      choose(skipTriggers)
      return
    }

    const doneCommitting = choices.value.findIndex((c) => {
      if (c.tag === Message.MessageType.START_SKILL_TEST_BUTTON) return true
      if (c.tag !== Message.MessageType.LABEL && c.tag !== Message.MessageType.DONE) return false
      return c.label === '$label.doneCommitting' || c.label.endsWith('doneCommitting')
    })
    if (doneCommitting !== -1) {
      choose(doneCommitting)
      return
    }

    const validIndices = choices.value
      .map((c, i) =>
        ![Message.MessageType.INVALID_LABEL, Message.MessageType.INFO].includes(c.tag) ? i : -1,
      )
      .filter((i) => i !== -1)

    if (validIndices.length === 1) {
      choose(validIndices[0])
      return
    }

    if (choices.value.length === 1) {
      choose(0)
      return
    }
    return
  }

  if (event.key === 'd') {
    const draw = choices.value.findIndex((c) => {
      if (c.tag !== Message.MessageType.COMPONENT_LABEL) return false
      if (c.component.tag !== 'InvestigatorDeckComponent') return false
      if (!playerId.value) return false
      return game.value?.investigators[c.component.investigatorId]?.playerId === playerId.value
    })
    if (draw !== -1) {
      choose(draw)
    } else {
      const drawEncounter = choices.value.findIndex((c) => {
        if (c.tag !== Message.MessageType.TARGET_LABEL) return false
        return c.target.tag === 'EncounterDeckTarget'
      })

      if (drawEncounter !== -1) choose(drawEncounter)
    }
    return
  }

  if (event.key === 'r') {
    const resource = choices.value.findIndex((c) => {
      if (c.tag !== Message.MessageType.COMPONENT_LABEL) return false
      if (c.component.tag !== 'InvestigatorComponent') return false
      if (c.component.tokenType !== 'ResourceToken') return false
      if (!playerId.value) return false
      return game.value?.investigators[c.component.investigatorId]?.playerId === playerId.value
    })
    if (resource !== -1) choose(resource)
    return
  }

  if (event.key === 'e') {
    if (!game.value || !playerId.value) return
    const elementUnderMouse = document.elementFromPoint(mouseX, mouseY)
    if (debug.active && elementUnderMouse) {
      const dataId = elementUnderMouse.getAttribute('data-id')
      if (dataId && game.value.assets[dataId]) {
        const exhausted = elementUnderMouse.classList.contains('exhausted')
        if (exhausted) {
          debug.send(game.value.id, {
            tag: 'Ready',
            contents: { tag: 'AssetTarget', contents: dataId },
          })
        } else {
          debug.send(game.value.id, {
            tag: 'Exhaust',
            contents: { tag: 'AssetTarget', contents: dataId },
          })
        }
        return
      }
    }
    const endTurn = choices.value.findIndex((c) => {
      if (c.tag !== Message.MessageType.END_TURN_BUTTON) return false
      return game.value?.investigators[c.investigatorId]?.playerId === playerId.value
    })
    if (endTurn !== -1) choose(endTurn)
    return
  }

  actionMap.value.get(event.key)?.()
}

// Sidebar
const toggleSidebar = function () {
  showSidebar.value = !showSidebar.value
  if (!isMobileViewport()) {
    setGameLocalStorageItem(props.gameId, 'showSidebar', JSON.stringify(showSidebar.value))
  }
}

// Undo
const undoLock = ref(false)
async function undo() {
  processing.value = true
  const oldQuestion = game.value?.question
  if (game.value) setGameQuestion({})
  resultQueue.value = []
  gameCard.value = null
  tarotCards.value = []
  uiLock.value = false
  if (undoLock.value) return
  undoLock.value = true
  try {
    await undoChoice(props.gameId, debug.active)
  } catch (e) {
    processing.value = false
    if (game.value && oldQuestion) setGameQuestion(oldQuestion)
    console.log(e)
  }
  undoLock.value = false
}

async function undoScenario() {
  confirmingUndoScenario.value = false
  processing.value = true
  if (game.value) setGameQuestion({})
  resultQueue.value = []
  gameCard.value = null
  tarotCards.value = []
  uiLock.value = false
  undoScenarioChoice(props.gameId)
}

async function undoBoundary(call: (gameId: string) => Promise<void>) {
  if (undoLock.value) return
  processing.value = true
  const oldQuestion = game.value?.question
  if (game.value) setGameQuestion({})
  resultQueue.value = []
  gameCard.value = null
  tarotCards.value = []
  uiLock.value = false
  undoLock.value = true
  try {
    await call(props.gameId)
  } catch (e) {
    processing.value = false
    if (game.value && oldQuestion) setGameQuestion(oldQuestion)
    console.log(e)
  }
  undoLock.value = false
}

const undoActionStart = () => undoBoundary(undoAction)
const undoTurnStart = () => undoBoundary(undoTurn)
const undoPhaseStart = () => undoBoundary(undoPhase)
const undoRoundStart = () => undoBoundary(undoRound)

const filingBug = ref(false)
const submittingBug = ref(false)
const bugTitle = ref('')
const bugDescription = ref('')

function fileBugFromError() {
  bugDescription.value = error.value ?? ''
  error.value = null
  filingBug.value = true
}

async function fileBug() {
  submittingBug.value = true
  filingBug.value = false
  Api.fileBug(props.gameId)
    .then((response) => {
      const title = encodeURIComponent(bugTitle.value)
      const body = encodeURIComponent(
        `${bugDescription.value}\n\ngame: ${window.location.href}\nfile: ${response.data}`,
      )
      window.open(
        `https://github.com/halogenandtoast/ArkhamHorror/issues/new?labels=bug&title=${title}&body=${body}&assignee=halogenandtoast&projects=halogenandtoast/2`,
        '_blank',
      )
      submittingBug.value = false
    })
    .catch(() => {
      alert(t('gameBar.bugSubmittingFail'))
      submittingBug.value = false
    })
}

const continueUI = () => {
  gameCard.value = null
  showTheSilenceModal.value = false
  tarotCards.value = []
  uiLock.value = false
}

function preloadImages(game: Arkham.Game): void {
  void loadAllImages(game).catch((e: unknown) => {
    console.error(e)
  })
}

async function loadAllImages(game: Arkham.Game): Promise<void> {
  const pending: string[] = []
  for (const card of Object.values(game.cards)) {
    const { cardCode, isFlipped } = toCardContents(card)
    const url = imgsrc(`cards/${cardCode.replace(/^c/, '')}${isFlipped ? 'b' : ''}.avif`)
    if (!preloaded.has(url) && !preloading.has(url)) pending.push(url)
  }
  if (pending.length === 0) return
  pending.forEach((url) => preloading.add(url))

  await Promise.all(
    pending.map(
      (url) =>
        new Promise<void>((resolve) => {
          const img = new Image()
          img.onload = () => {
            preloaded.add(url)
            preloading.delete(url)
            resolve()
          }
          img.onerror = () => {
            preloaded.add(url)
            preloading.delete(url)
            console.warn(`Could not preload ${url}`)
            resolve()
          }
          img.src = url
        }),
    ),
  )
}

// Callbacks
async function choose(idx: number) {
  if (idx !== -1 && game.value && !props.spectate) {
    oldQuestion.value = game.value.question
    const questionVersion = game.value.scenarioSteps
    setGameQuestion({})
    processing.value = true
    send(
      JSON.stringify({
        tag: 'Answer',
        contents: { choice: idx, playerId: playerId.value, questionVersion },
      }),
    )
  }
}

async function chooseDeck(deckId: string): Promise<void> {
  if (game.value && !props.spectate) {
    oldQuestion.value = game.value.question
    setGameQuestion({})
    processing.value = true
    send(JSON.stringify({ tag: 'DeckAnswer', deckId, playerId: playerId.value }))
  }
}

async function chooseDeckList(deckList: object): Promise<void> {
  if (game.value && !props.spectate) {
    oldQuestion.value = game.value.question
    setGameQuestion({})
    processing.value = true
    send(JSON.stringify({ tag: 'DeckListAnswer', deckList, playerId: playerId.value }))
  }
}

async function choosePaymentAmounts(amounts: Record<string, number>): Promise<void> {
  if (game.value && !props.spectate) {
    oldQuestion.value = game.value.question
    const questionVersion = game.value.scenarioSteps
    setGameQuestion({})
    processing.value = true
    send(
      JSON.stringify({
        tag: 'PaymentAmountsAnswer',
        contents: { amounts, questionVersion, playerId: playerId.value },
      }),
    )
  }
}

async function scenarioSpecificAnswer(key: string, value: unknown): Promise<void> {
  if (game.value && !props.spectate) {
    oldQuestion.value = game.value.question
    setGameQuestion({})
    processing.value = true
    send(JSON.stringify({ tag: 'ScenarioSpecificAnswer', contents: [key, value] }))
  }
}

async function chooseAmounts(amounts: Record<string, number>): Promise<void> {
  if (game.value && !props.spectate) {
    oldQuestion.value = game.value.question
    const questionVersion = game.value.scenarioSteps
    setGameQuestion({})
    processing.value = true
    send(
      JSON.stringify({
        tag: 'AmountsAnswer',
        contents: { amounts, questionVersion, playerId: playerId.value },
      }),
    )
  }
}

function localize(str: string): string {
  if (str.startsWith('$')) {
    return t(str.slice(1))
  }
  return str
}

async function update(state: Arkham.Game) {
  game.value = state
}

function switchInvestigator(newPlayerId: string) {
  playerId.value = newPlayerId
}
type ExportType = 'basic' | 'full' | 'scenario'
function debugExport(exportType: ExportType) {
  const isFullExport = exportType === 'full'
  api
    .get(
      `arkham/games/${props.gameId}/${isFullExport ? 'full-' : exportType == 'scenario' ? 'scenario-' : ''}export`,
      { responseType: 'blob', params: isFullExport ? { gzip: true } : undefined },
    )
    .then((resp) => {
      const url = window.URL.createObjectURL(resp.data)
      const a = document.createElement('a')
      a.style.display = 'none'
      a.href = url
      // the filename you want
      a.download = isFullExport ? 'arkham-debug.json.gz' : 'arkham-debug.json'
      document.body.appendChild(a)
      a.click()
      window.URL.revokeObjectURL(url)
    })
    .catch((e) => {
      console.log(e)
      alert(t('game.unableToDownloadExport'))
    })
}

// provides
provide(choicesByPlayerKey, choicesByPlayer)
provide(choicesSourceByPlayerKey, choicesSourceByPlayer)
provide(choicesTooltipByPlayerKey, choicesTooltipByPlayer)
provide(gameIndexesKey, gameIndexes)
provide('chooseDeck', chooseDeck)
provide('chooseDeckList', chooseDeckList)
provide('send', send)
provide('choosePaymentAmounts', choosePaymentAmounts)
provide('chooseAmounts', chooseAmounts)
provide('scenarioSpecificAnswer', scenarioSpecificAnswer)
provide('switchInvestigator', switchInvestigator)
provide('solo', solo)
provide('skipAllTriggers', skipAllTriggers)
provide('skipAllAvailable', skipAllAvailable)
provide('skipAllInProgress', skipAllInProgress)
provide('showOtherPlayersHands', showOtherPlayersHands)

function updateFocusLight() {
  const highlighted = [...document.querySelectorAll<HTMLElement>(
    '.source-highlight, .ability-target, .card-frame-inner.highlighted, .cards-under-indicator--highlighted',
  )].find((el) => {
    if (el.closest('.scenario-cards')) return false
    const rect = el.getBoundingClientRect()
    return rect.width > 0 && rect.height > 0 && rect.bottom >= 0 && rect.right >= 0
      && rect.top <= window.innerHeight && rect.left <= window.innerWidth
  })

  if (!highlighted) {
    focusLightX.value = -1000
    focusLightY.value = -1000
    return
  }

  const rect = highlighted.getBoundingClientRect()
  focusLightX.value = rect.left + rect.width / 2
  focusLightY.value = rect.top + rect.height / 2
}

function scheduleFocusLightUpdate() {
  if (focusLightAnimationFrame !== null) return
  focusLightAnimationFrame = requestAnimationFrame(() => {
    focusLightAnimationFrame = null
    updateFocusLight()
  })
}

const onMove = (event: MouseEvent) => {
  mouseX = event.clientX
  mouseY = event.clientY
  flashlightX.value = event.clientX
  flashlightY.value = event.clientY
  scheduleFocusLightUpdate()
}

// callbacks
const onPlayabilityResult = (result: any) => {
  if (!debug.active) return
  playabilityInfo.value = {
    cardId: result.cardId,
    cardCode: result.cardCode,
    checks: result.checks,
  }
}
emitter.on('playabilityResult', onPlayabilityResult)

onMounted(() => {
  flashlightX.value = window.innerWidth / 2
  flashlightY.value = window.innerHeight / 2
  ;(window as any).sendDebug = async (msg: any) => {
    if (game.value) await debug.send(game.value.id, msg)
  }
  ;(window as any).undo = undo
  ;(window as any).debugChoose = choose
  document.addEventListener('mousemove', onMove, { passive: true })
  focusLightObserver = new MutationObserver(scheduleFocusLightUpdate)
  focusLightObserver.observe(document.body, { attributes: true, attributeFilter: ['class'], subtree: true })
  scheduleFocusLightUpdate()
  document.addEventListener('keydown', handleKeyPress)
  window.addEventListener('arkham-setting-change', handleSettingChange)
})

onBeforeRouteLeave(() => close())
onUnmounted(() => {
  document.removeEventListener('keydown', handleKeyPress)
  document.removeEventListener('mousemove', onMove)
  focusLightObserver?.disconnect()
  focusLightObserver = null
  if (focusLightAnimationFrame !== null) cancelAnimationFrame(focusLightAnimationFrame)
  window.removeEventListener('arkham-setting-change', handleSettingChange)
  cancelAllAiTimers()
  delete (window as any).sendDebug
  delete (window as any).undo
  delete (window as any).debugChoose
  emitter.off('playabilityResult', onPlayabilityResult)
  close()
})
</script>

<template>
  <div v-if="submittingBug" class="column page-container">
    <div class="page-content column">
      <h2 class="title">{{ $t('gameBar.bugSubmittingTitle') }}</h2>
      <section class="box">
        {{ $t('gameBar.bugSubmittingContent') }}
      </section>
    </div>
  </div>
  <div id="game" v-else-if="ready && game && playerId" :style="{ '--epic-bar-height': epicBarHeight + 'px' }">
    <AiControlPanel
      v-if="aiDevEnabled && game && aiSeatIds.length > 0"
      :game="game"
      :stuck-seats="aiStuckSeats"
    />
    <AiQuestionsPanel
      v-if="aiDevEnabled && game && aiSeatIds.length > 0"
      :game="game"
    />
    <dialog v-if="error" class="error-dialog">
      <h2>{{ $t('error') }}</h2>
      <p class="error-message">{{ error }}</p>
      <p>{{ $t('errorContent') }}</p>
      <div class="buttons">
        <button @click="fileBugFromError">
          <ExclamationTriangleIcon aria-hidden="true" /> {{ $t('fileBug') }}
        </button>
        <button @click="error = null">{{ $t('close') }}</button>
      </div>
    </dialog>
    <div v-if="processing" class="processing">
      <LottieAnimation
        :animation-data="processingJSON"
        :auto-play="true"
        :loop="true"
        :speed="1"
        ref="anim"
      />
    </div>
    <CardOverlay />
    <div
      v-if="realityAcidLightActive"
      class="reality-acid-flashlight"
      :style="{ '--flashlight-x': `${flashlightX}px`, '--flashlight-y': `${flashlightY}px` }"
      aria-hidden="true"
    ></div>
    <div
      v-if="realityAcidLightActive"
      class="reality-acid-focus-light"
      :style="{ '--focus-light-x': `${focusLightX}px`, '--focus-light-y': `${focusLightY}px` }"
      aria-hidden="true"
    ></div>
    <Draggable v-if="showShortcuts">
      <div class="shortcuts-modal">
        <div class="shortcuts-header">
          <h2 class="shortcuts-title">{{ $t('gameBar.shortcutsTitle') }}</h2>
        </div>

        <div class="shortcuts-body">
          <section class="shortcuts-section">
            <h3 class="section-title">{{ $t('game.shortcutSection.game') }}</h3>
            <div class="shortcut-list">
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('gameBar.shortcutSkipTriggers') }}</div>
                <div class="shortcut-keys"><kbd> </kbd></div>
              </div>
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('gameBar.shortcutEndTurn') }}</div>
                <div class="shortcut-keys"><kbd>e</kbd></div>
              </div>
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('gameBar.shortcutDraw') }}</div>
                <div class="shortcut-keys"><kbd>d</kbd></div>
              </div>
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('gameBar.shortcutTakeResources') }}</div>
                <div class="shortcut-keys"><kbd>r</kbd></div>
              </div>
            </div>
          </section>

          <section class="shortcuts-section">
            <h3 class="section-title">{{ $t('game.shortcutSection.undo') }}</h3>
            <div class="shortcut-list">
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('gameBar.shortcutUndo') }}</div>
                <div class="shortcut-keys"><kbd>u</kbd></div>
              </div>
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('game.shortcutUndoActionStart') }}</div>
                <div class="shortcut-keys">
                  <kbd>U</kbd><span class="chord-arrow">+</span><kbd>A</kbd>
                </div>
              </div>
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('game.shortcutUndoTurnStart') }}</div>
                <div class="shortcut-keys">
                  <kbd>U</kbd><span class="chord-arrow">+</span><kbd>T</kbd>
                </div>
              </div>
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('game.shortcutUndoPhaseStart') }}</div>
                <div class="shortcut-keys">
                  <kbd>U</kbd><span class="chord-arrow">+</span><kbd>P</kbd>
                </div>
              </div>
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('game.shortcutUndoRoundStart') }}</div>
                <div class="shortcut-keys">
                  <kbd>U</kbd><span class="chord-arrow">+</span><kbd>R</kbd>
                </div>
              </div>
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('gameBar.shortcutRestartScenario') }}</div>
                <div class="shortcut-keys">
                  <kbd>U</kbd><span class="chord-arrow">+</span><kbd>S</kbd>
                </div>
              </div>
            </div>
          </section>

          <section class="shortcuts-section">
            <h3 class="section-title">{{ $t('game.shortcutSection.view') }}</h3>
            <div class="shortcut-list">
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('gameBar.shortcutShowOrHideShortcuts') }}</div>
                <div class="shortcut-keys"><kbd>?</kbd></div>
              </div>
              <div class="shortcut-row">
                <div class="shortcut-name">{{ $t('gameBar.shortcutToggleDebug') }}</div>
                <div class="shortcut-keys"><kbd>D</kbd></div>
              </div>
              <template v-for="item in menuItems" :key="item.id">
                <div v-if="item.shortcut" class="shortcut-row">
                  <div class="shortcut-name">{{ item.content }}</div>
                  <div class="shortcut-keys">
                    <kbd>{{ item.shortcut }}</kbd>
                  </div>
                </div>
              </template>
            </div>
          </section>
        </div>

        <button class="shortcuts-footer" @click="showShortcuts = false">{{ $t('close') }}</button>
      </div>
    </Draggable>
    <Draggable v-if="filingBug">
      <template #handle>
        <header>
          <h2>{{ $t('gameBar.fileABug') }}</h2>
        </header>
      </template>
      <form @submit.prevent="fileBug" class="column bug-form box">
        <p>{{ $t('gameBar.fileBugPart1') }}</p>
        <p class="info">{{ $t('gameBar.fileBugPart2') }}</p>
        <p class="warning">{{ $t('gameBar.fileBugPart3') }}</p>
        <input
          required
          type="text"
          v-model="bugTitle"
          v-bind:placeholder="$t('gameBar.bugTitleholder')"
        />
        <textarea
          required
          v-model="bugDescription"
          v-bind:placeholder="$t('gameBar.bugDescriptionholder')"
        ></textarea>
        <div class="buttons">
          <button type="submit">{{ $t('submit') }}</button>
          <button @click="filingBug = false">{{ $t('cancel') }}</button>
        </div>
      </form>
    </Draggable>
    <div v-if="socketError" class="socketWarning">
      <!-- frontend/src/locales/en/gameBoard/base.json -->
      <p>{{ $t('outOfSyncHint') }}</p>
    </div>
    <div class="game-bar">
      <div class="game-bar-item">
        <div>
          <button @click="showLog = !showLog">
            <DocumentTextIcon aria-hidden="true" />
            {{ showLog ? $t('gameBar.closeLog') : $t('gameBar.viewLog') }}
          </button>
        </div>
      </div>
      <div>
        <Menu>
          <EyeIcon aria-hidden="true" />
          {{ $t('gameBar.view') }}
          <template #items>
            <MenuItem v-slot="{ active }">
              <button :class="{ active }" @click="showShortcuts = !showShortcuts">
                <BoltIcon aria-hidden="true" /> {{ $t('gameBar.shortcuts') }}
                <span class="shortcut">?</span>
              </button>
            </MenuItem>
            <template v-for="item in menuItems" :key="item.id">
              <MenuItem v-if="item.nested === 'view'" v-slot="{ active }">
                <button :class="{ active }" @click="item.action">
                  <component v-if="item.icon" v-bind:is="item.icon"></component>
                  {{ item.content }}
                  <span v-if="item.shortcut" class="shortcut">{{ item.shortcut }}</span>
                </button>
              </MenuItem>
            </template>
          </template>
        </Menu>
      </div>
      <div>
        <Menu>
          <BeakerIcon aria-hidden="true" />
          {{ $t('gameBar.debug') }}
          <template #items>
            <MenuItem v-slot="{ active }">
              <button :class="{ active }" @click="debug.toggle">
                <BugAntIcon aria-hidden="true" /> {{ $t('gameBar.toggleDebug') }}
                <span class="shortcut">D</span>
              </button>
            </MenuItem>
            <MenuItem v-slot="{ active }">
              <button :class="{ active }" @click="debugExport('basic')">
                <DocumentArrowDownIcon aria-hidden="true" /> {{ $t('gameBar.debugExport') }}
              </button>
            </MenuItem>
            <MenuItem v-if="userStore.isAdmin" v-slot="{ active }">
              <button :class="{ active }" @click="debugExport('scenario')">
                <DocumentArrowDownIcon aria-hidden="true" /> {{ $t('gameBar.debugExportScenario') }}
              </button>
            </MenuItem>
            <MenuItem v-if="userStore.isAdmin" v-slot="{ active }">
              <button :class="{ active }" @click="debugExport('full')">
                <DocumentArrowDownIcon aria-hidden="true" /> {{ $t('gameBar.debugExportFull') }}
              </button>
            </MenuItem>
          </template>
        </Menu>
      </div>
      <div>
        <Menu>
          <BackwardIcon aria-hidden="true" />
          {{ $t('gameBar.undo') }}
          <template #items>
            <MenuItem v-slot="{ active }">
              <button :class="{ active }" @click="undo">
                <BackwardIcon aria-hidden="true" /> {{ $t('gameBar.undo') }}
                <span class="shortcut">u</span>
              </button>
            </MenuItem>
            <div
              v-if="canUndoAction || canUndoTurn || canUndoPhase || canUndoRound || canUndoScenario"
              class="undo-jump-group"
              :class="{ armed: undoChordArmed }"
            >
              <div class="undo-jump-header">
                <span>{{ $t('game.undoTo') }}</span>
                <span class="chord-prefix"><kbd>U</kbd> + <span class="chord-hint">…</span></span>
              </div>
              <MenuItem v-if="canUndoAction" v-slot="{ active }">
                <button class="undo-jump scope-action" :class="{ active }" @click="undoActionStart">
                  <ArrowUturnLeftIcon aria-hidden="true" />
                  <span class="undo-jump-label">{{ $t('game.startOfAction') }}</span>
                  <kbd class="chord-key">A</kbd>
                </button>
              </MenuItem>
              <MenuItem v-if="canUndoTurn" v-slot="{ active }">
                <button class="undo-jump scope-turn" :class="{ active }" @click="undoTurnStart">
                  <ClockIcon aria-hidden="true" />
                  <span class="undo-jump-label">{{ $t('game.startOfTurn') }}</span>
                  <kbd class="chord-key">T</kbd>
                </button>
              </MenuItem>
              <MenuItem v-if="canUndoPhase" v-slot="{ active }">
                <button class="undo-jump scope-phase" :class="{ active }" @click="undoPhaseStart">
                  <RectangleStackIcon aria-hidden="true" />
                  <span class="undo-jump-label">{{ $t('game.startOfPhase') }}</span>
                  <kbd class="chord-key">P</kbd>
                </button>
              </MenuItem>
              <MenuItem v-if="canUndoRound" v-slot="{ active }">
                <button class="undo-jump scope-round" :class="{ active }" @click="undoRoundStart">
                  <ArrowPathIcon aria-hidden="true" />
                  <span class="undo-jump-label">{{ $t('game.startOfRound') }}</span>
                  <kbd class="chord-key">R</kbd>
                </button>
              </MenuItem>
              <MenuItem v-if="canUndoScenario" v-slot="{ active }">
                <button
                  class="undo-jump scope-scenario"
                  :class="{ active }"
                  @click="confirmingUndoScenario = true"
                >
                  <FlagIcon aria-hidden="true" />
                  <span class="undo-jump-label">{{ $t('gameBar.restartScenario') }}</span>
                  <kbd class="chord-key">S</kbd>
                </button>
              </MenuItem>
            </div>
          </template>
        </Menu>
      </div>
      <div>
        <button @click="filingBug = true">
          <ExclamationTriangleIcon aria-hidden="true" /> {{ $t('fileBug') }}
        </button>
      </div>
      <div v-for="item in menuItems" :key="item.id">
        <template v-if="item.nested === null || item.nested === undefined">
          <button @click="item.action">
            <component v-if="item.icon" v-bind:is="item.icon"></component>
            {{ item.content }}
          </button>
        </template>
      </div>
      <div class="right">
        <button v-if="isActualScenarioView" @click="toggleSidebar">
          <ArrowsRightLeftIcon aria-hidden="true" /> {{ $t('gameBar.toggleSidebar') }}
        </button>
      </div>
    </div>
    <div v-if="hasEventBar" ref="epicBarRef" class="epic-bar-slot">
      <OrganizerBar
        v-if="organizerEventId"
        :event-id="organizerEventId"
        :current-game-id="gameId"
        :spectate="spectate"
        :current-act-stage="currentActStage"
      />
      <PlayerEventBar
        v-else-if="playerEventId"
        :event-id="playerEventId"
        :current-game-id="gameId"
        :spectate="spectate"
        :current-act-stage="currentActStage"
      />
    </div>
    <EventStartBarrier v-if="showStartBarrier" />
    <EventActAdvanceBarrier v-if="showActAdvanceWait" />
    <MultiplayerLobby
      v-if="game.gameState.tag === 'IsPending'"
      :game-id="gameId"
      :game="game"
      :player-id="playerId"
    />
    <template v-else>
      <Draggable v-if="showSettings">
        <Settings
          :game="game"
          :playerId="playerId"
          :solo="solo"
          v-model:showOtherPlayersHands="showOtherPlayersHands"
          :closeSettings="() => (showSettings = false)"
        />
      </Draggable>
      <CampaignLog
        v-if="showLog && game !== null"
        :game="game"
        :cards="cards"
        :playerId="playerId"
      >
        <template #header-leading>
          <button class="back-button" @click="showLog = false">
            <font-awesome-icon icon="arrow-left" class="back-icon" />
            <span>{{ $t('back') }}</span>
          </button>
        </template>
      </CampaignLog>
      <div v-else class="game-main">
        <div v-if="showTheSilenceModal" class="the-silence-modal-backdrop">
          <div class="the-silence-modal" role="dialog" aria-modal="true" aria-labelledby="the-silence-modal-title">
            <img class="the-silence-modal__agenda no-overlay" :src="imgsrc('cards/10652.avif')" alt="The Silence" />
            <div class="the-silence-modal__body">
              <h2 id="the-silence-modal-title">The Silence</h2>
              <p>If you look at the Cosmic Emissary enemy for more than 15 seconds at a time, you are <strong>driven insane</strong>.</p>
              <div class="the-silence-modal__actions">
                <button type="button" class="the-silence-modal__confirm" @click="continueUI">{{ $t('ok') }}</button>
              </div>
            </div>
          </div>
        </div>
        <div v-else-if="gameCard" class="revelation">
          <div class="revelation-container">
            <h2>{{ format(gameCard.title) }}</h2>
            <div class="revelation-card-container">
              <div class="revelation-card">
                <CardView :game="game" :card="gameCard.card" :playerId="playerId" />
                <img
                  v-if="gameCard.card.tag === 'PlayerCard'"
                  :src="imgsrc('player_back.jpg')"
                  class="card back"
                />
                <img v-else :src="imgsrc('back.png')" class="card back" />
              </div>
              <button @click="continueUI">{{ $t('ok') }}</button>
            </div>
          </div>
        </div>
        <HistoryPanel
          v-if="showHistory && game && playerId"
          :game="game"
          :playerId="playerId"
          @close="showHistory = false"
        />
        <div
          v-if="playabilityInfo && debug.active"
          class="debug-modal-overlay"
          @click.self="playabilityInfo = null"
        >
          <div class="debug-playability-modal">
            <h3>{{ $t('game.playabilityChecks') }}</h3>
            <div class="debug-playability-content">
              <img
                class="debug-card-image"
                :src="imgsrc(`cards/${playabilityInfo.cardCode.replace('c', '')}.avif`)"
              />
              <ul class="playability-checks">
                <li
                  v-for="[name, detail] in playabilityInfo.checks"
                  :key="name"
                  :class="detail === null ? 'check-passed' : 'check-failed'"
                >
                  <span class="check-icon">{{ detail === null ? '✓' : '✗' }}</span>
                  <span class="check-name">{{ name }}</span>
                  <span v-if="detail !== null" class="check-detail">{{ detail }}</span>
                </li>
              </ul>
            </div>
            <button @click="playabilityInfo = null">{{ $t('close') }}</button>
          </div>
        </div>
        <div v-if="tarotCards.length > 0" class="revelation">
          <div class="revelation-container">
            <div class="revelation-card-container">
              <div class="tarot-cards">
                <div v-for="(tarotCard, idx) in tarotCards" :key="idx" class="tarot-card">
                  <div class="card-container">
                    <img
                      :src="imgsrc(`tarot/${tarotCardImage(tarotCard)}`)"
                      class="tarot"
                      :class="tarotCard.facing"
                    />
                  </div>
                  <img :src="imgsrc('tarot/back.jpg')" class="card back" />
                </div>
              </div>
              <button @click="continueUI">{{ $t('ok') }}</button>
            </div>
          </div>
        </div>
        <CampaignSettings
          v-if="game.campaign && !gameOver && question && question.tag === 'PickCampaignSettings'"
          :game="game"
          :campaign="game.campaign"
          :playerId="playerId"
        />
        <Campaign
          v-else-if="game.campaign"
          :game="game"
          :gameLog="gameLog"
          :playerId="playerId"
          :campaign="game.campaign"
          :realityAcidLightDevoured="realityAcidLightDevoured"
          :realityAcidLightActive="realityAcidLightActive"
          @choose="choose"
          @update="update"
          @toggleRealityAcidLight="toggleRealityAcidLight"
        />
        <ScenarioSettings
          v-else-if="
            game.scenario && !gameOver && question && question.tag === 'PickScenarioSettings'
          "
          :game="game"
          :scenario="game.scenario"
          :playerId="playerId"
        />
        <StandaloneScenario
          v-else-if="game.scenario && !gameOver"
          :game="game"
          :playerId="playerId"
          :realityAcidLightDevoured="realityAcidLightDevoured"
          :realityAcidLightActive="realityAcidLightActive"
          @choose="choose"
          @update="update"
          @toggleRealityAcidLight="toggleRealityAcidLight"
        />
        <div
          class="sidebar"
          :class="{ 'sidebar--empty-log': gameLog.length === 0 }"
          v-if="
            showSidebar &&
            isActualScenarioView
          "
        >
          <GameLog :game="game" :gameLog="gameLog" @undo="undo" />
        </div>
        <div class="game-over" v-if="gameOver">
          <p>{{ $t('gameOver') }}</p>
          <button
            class="replay-button"
            @click="router.push({ name: 'ReplayGame', params: { gameId } })"
          >
            {{ $t('watchReplay') }}
          </button>
          <CampaignLog v-if="game !== null" :game="game" :cards="cards" :playerId="playerId" />
        </div>
        <div
          v-if="showSidebar && isActualScenarioView"
          class="sidebar-backdrop"
          @click="toggleSidebar"
          aria-hidden="true"
        ></div>
      </div>
    </template>
    <Prompt
      v-if="confirmingUndoScenario"
      prompt="$game.areYouSureUndoScenario"
      :yes="undoScenario"
      :no="() => confirmingUndoScenario = false"
    />
  </div>
</template>

<style lang="scss" scoped>
.back-button {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  padding: 8px 16px;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.05);
  border: 1px solid rgba(255, 255, 255, 0.1);
  color: rgba(255, 255, 255, 0.7);
  font-family: teutonic, sans-serif;
  font-size: 0.95em;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  text-decoration: none;
  cursor: pointer;
  transition: background 0.15s, border-color 0.15s, color 0.15s;

  .back-icon {
    font-size: 0.85em;
    transition: transform 0.15s;
  }

  &:hover {
    background: rgba(255, 255, 255, 0.1);
    border-color: rgba(255, 255, 255, 0.2);
    color: #f0f0f0;

    .back-icon {
      transform: translateX(-3px);
    }
  }
}

.reality-acid-flashlight {
  --flashlight-x: 50vw;
  --flashlight-y: 50vh;
  position: fixed;
  inset: 0;
  z-index: var(--z-index-9998);
  pointer-events: none;
  background: radial-gradient(
    circle 330px at var(--flashlight-x) var(--flashlight-y),
    rgba(0, 0, 0, 0) 0 52%,
    rgba(0, 0, 0, 0.12) 68%,
    rgba(0, 0, 0, 0.82) 100%
  );
}

.reality-acid-focus-light {
  --focus-light-x: -1000px;
  --focus-light-y: -1000px;
  position: fixed;
  inset: 0;
  z-index: calc(var(--z-index-9998) + 1);
  pointer-events: none;
  background: radial-gradient(
    circle 205px at var(--focus-light-x) var(--focus-light-y),
    rgba(255, 248, 190, 0.72) 0 18%,
    rgba(255, 230, 128, 0.42) 46%,
    rgba(255, 226, 120, 0) 76%
  );
  mix-blend-mode: screen;
  opacity: 0.95;
}

.action {
  border: 5px solid var(--select);
  border-radius: 15px;
}

.undo-jump-group {
  background: rgba(0, 0, 0, 0.22);
  box-shadow: inset 0 1px 2px rgba(0, 0, 0, 0.25);
  border-bottom-left-radius: 5px;
  border-bottom-right-radius: 5px;
  overflow: hidden;
  transition:
    box-shadow 0.2s ease,
    background 0.2s ease;

  &.armed {
    background: rgba(0, 0, 0, 0.35);
    box-shadow:
      inset 0 1px 2px rgba(0, 0, 0, 0.3),
      0 0 0 1px rgba(127, 184, 212, 0.6),
      0 0 12px rgba(127, 184, 212, 0.35);
  }
}

.game-bar div .undo-jump-header {
  display: flex;
}

.undo-jump-header {
  font-size: 10px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  color: rgba(255, 255, 255, 0.55);
  padding: 8px 10px 6px 10px;
  user-select: none;
  pointer-events: none;
  align-items: center;
  gap: 8px;
}

.chord-prefix {
  display: inline-flex;
  align-items: center;
  gap: 4px;
  margin-left: auto;
  letter-spacing: normal;
  text-transform: none;

  kbd {
    font-family: inherit;
    font-size: inherit;
    font-weight: bold;
    padding: 2px 5px;
    border-radius: 4px;
    background-color: var(--box-background);
    border: 1px solid var(--title);
    color: white;
    line-height: 1;
  }

  .chord-hint {
    opacity: 0.6;
  }
}

.undo-jump-group.armed .chord-prefix kbd {
  background-color: var(--box-border);
}

.chord-key {
  font-family: inherit;
  font-size: inherit;
  font-weight: bold;
  margin-left: auto;
  padding: 2px 5px;
  border-radius: 4px;
  background-color: var(--box-background);
  border: 1px solid var(--title);
  color: white;
  line-height: 1;
}

.undo-jump:hover .chord-key,
.undo-jump.active .chord-key,
.undo-jump-group.armed .chord-key {
  background-color: var(--box-border);
}

.undo-jump {
  position: relative;
  width: 100%;
  padding: 5px 10px 5px 18px !important;
  background: rgba(0, 0, 0, 0.4);

  &::before {
    content: '';
    position: absolute;
    left: 6px;
    top: 6px;
    bottom: 6px;
    width: 2px;
    border-radius: 2px;
    background: var(--undo-scope);
    opacity: 0.55;
    transition:
      opacity 0.15s ease,
      transform 0.15s ease;
  }

  svg {
    color: var(--undo-scope);
  }

  &.scope-action {
    --undo-scope: #7fb8d4;
  }
  &.scope-turn {
    --undo-scope: #6cc28d;
  }
  &.scope-phase {
    --undo-scope: #e0b256;
  }
  &.scope-round {
    --undo-scope: #c97aa8;
  }
  &.scope-scenario {
    --undo-scope: #d96a6a;
  }

  &:hover {
    background: rgba(0, 0, 0, 0.6);
  }

  &:hover::before,
  &.active::before {
    opacity: 1;
    transform: scaleX(1.5);
  }
}

#game {
  width: 100vw;
  display: flex;
  flex-direction: column;
  flex: 1;
  overflow: hidden;
  &:has(.scroll-container) {
    overflow: auto;
  }
}

/* Epic Multiplayer bar lives in normal flow above the board; reserve its measured
   height so the board's player area stays within the viewport. Defaults to 0 for
   ordinary games. */
.epic-bar-slot {
  flex: 0 0 auto;
}

.game-main {
  width: 100vw;
  height: calc(100vh - 80px - var(--epic-bar-height, 0px));
  display: flex;
  flex: 1;
}

.socketWarning {
  backdrop-filter: blur(3px);
  background-color: rgba(0, 0, 0, 0.8);
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  display: flex;
  z-index: var(--z-index-100);

  justify-content: center;
  align-items: center;
  justify-self: center;
  align-self: center;

  p {
    padding: 10px;
    background: #fff;
    border-radius: 4px;
  }
}

.sidebar {
  height: 100%;
  width: 25vw;
  max-width: 300px;
  display: flex;
  flex-direction: column;
  background: #d0d9dc;

  @media (max-width: 800px) {
    position: fixed;
    top: 0;
    right: 0;
    height: 100vh;
    height: 100dvh;
    width: min(85vw, 360px);
    max-width: none;
    z-index: var(--z-index-200);
    box-shadow: -2px 0 16px rgba(0, 0, 0, 0.45);
    animation: sidebar-slide-in 0.18s ease-out;
  }

  @media (prefers-color-scheme: dark) {
    background: #1c1c1c;
  }
}

.sidebar--empty-log {
  pointer-events: none;
}

.sidebar-backdrop {
  display: none;

  @media (max-width: 800px) {
    display: block;
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.5);
    z-index: var(--z-index-199);
    animation: sidebar-fade-in 0.18s ease-out;
  }
}

@keyframes sidebar-slide-in {
  from {
    transform: translateX(100%);
  }
  to {
    transform: translateX(0);
  }
}

@keyframes sidebar-fade-in {
  from {
    opacity: 0;
  }
  to {
    opacity: 1;
  }
}

#invite {
  background-color: #15192c;
  color: white;
  width: 800px;
  margin: 0 auto;
  margin-top: 20px;
  border-radius: 5px;
  text-align: center;
  p {
    margin: 0;
    padding: 0;
    margin-bottom: 20px;
    font-size: 1.3em;
  }
  @media (max-width: 800px) and (orientation: portrait) {
    width: 100%;
  }
}

.invite-container {
  margin-top: 50px;
  h2 {
    color: #656a84;
    margin-left: 10px;
    text-transform: uppercase;
    padding: 0;
    margin: 0;
  }
}

header {
  display: flex;
  flex-direction: column;
}

.invite-link {
  flex: 1;
  input {
    font-size: 1.3em;
    width: 60%;
    border-right: 0;
    border-radius: 3px 0 0 3px;
    padding: 5px;
  }
  button {
    font-size: 1.3em;
    border-radius: 0 3px 3px 0;
    padding: 5px 10px;
    position: relative;

    &:before {
      content: '';
      display: none;
      position: absolute;
      z-index: var(--z-index-9998);
      top: 35px;
      left: 15px;
      width: 0;
      height: 0;

      border-left: 5px solid transparent;
      border-right: 5px solid transparent;
      border-bottom: 5px solid rgba(0, 0, 0, 0.72);
    }

    &:after {
      content: 'Copied!';
      display: none;
      position: absolute;
      z-index: var(--z-index-9999);
      top: var(--nav-height);
      left: -37px;
      width: 114px;
      height: 36px;

      color: #fff;
      font-size: 10px;
      line-height: 36px;
      text-align: center;

      background: rgba(0, 0, 0, 0.72);
      border-radius: 3px;
    }

    &:active,
    &:focus {
      outline: none;

      &:hover {
        background-color: #eee;

        &:before,
        &:after {
          display: block;
        }
      }
    }
  }
}

.button-link {
  display: block;
  width: 100%;
  text-decoration: none;
  button {
    display: block;
    width: 100%;
  }
}

header {
  font-family: Teutonic;
  font-size: 2em;
  text-align: center;
}

.game-over {
  flex-grow: 1;
  display: flex;
  flex-direction: column;
  align-items: center;

  p {
    text-transform: uppercase;
    background: rgba(0, 0, 0, 0.5);
    width: 100%;
    padding: 10px 20px;
    color: white;
    text-align: center;
  }
}

@keyframes revelation {
  0% {
    opacity: 0;
    transform: scale(0);
  }

  65% {
    transform: scale(1.3);
  }

  100% {
    opacity: 1;
    transform: scale(1);
  }
}

@keyframes anim {
  0%,
  100% {
    border-radius: 30% 70% 70% 30% / 30% 52% 48% 70%;
    /*box-shadow: 10px -2vmin 4vmin LightPink inset, 10px -4vmin 4vmin MediumPurple inset, 10px -2vmin 7vmin purple inset;*/
  }

  10% {
    border-radius: 50% 50% 20% 80% / 25% 80% 20% 75%;
  }

  20% {
    border-radius: 67% 33% 47% 53% / 37% 20% 80% 63%;
  }

  30% {
    border-radius: 39% 61% 47% 53% / 37% 40% 60% 63%;
    /*box-shadow: 20px -4vmin 8vmin hotpink inset, -1vmin -2vmin 6vmin LightPink inset, -1vmin -2vmin 4vmin MediumPurple inset, 1vmin 4vmin 8vmin purple inset;*/
  }

  40% {
    border-radius: 39% 61% 82% 18% / 74% 40% 60% 26%;
  }

  50% {
    border-radius: 100%;
    /*box-shadow: 40px 4vmin 16vmin hotpink inset, 40px 2vmin 5vmin LightPink inset, 40px 4vmin 4vmin MediumPurple inset, 40px 6vmin 8vmin purple inset;*/
  }

  60% {
    border-radius: 50% 50% 53% 47% / 72% 69% 31% 28%;
  }

  70% {
    border-radius: 50% 50% 53% 47% / 26% 22% 78% 74%;
    /*box-shadow: 1vmin 1vmin 8vmin LightPink inset, 2vmin -1vmin 4vmin MediumPurple inset, -1vmin -1vmin 16vmin purple inset;*/
  }

  80% {
    border-radius: 50% 50% 53% 47% / 26% 69% 31% 74%;
  }

  90% {
    border-radius: 20% 80% 20% 80% / 20% 80% 20% 80%;
  }
}

@property --gradient-angle {
  syntax: '<angle>';
  initial-value: 0deg;
  inherits: false;
}

@keyframes rotation {
  0% {
    --gradient-angle: 360deg;
  }
  100% {
    --gradient-angle: 0deg;
  }
}

@keyframes glow {
  0% {
    filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
      drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue) drop-shadow(0 0 7vmin MediumOrchid);
  }
  50% {
    filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
      drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue) drop-shadow(0 0 7vmin Black);
  }
  100% {
    filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
      drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue) drop-shadow(0 0 7vmin MediumOrchid);
  }
}

.the-silence-modal-backdrop {
  position: fixed;
  inset: 0;
  z-index: var(--z-index-30000);
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 24px;
  background: rgba(0, 0, 0, 0.65);
}

.the-silence-modal {
  display: flex;
  gap: 18px;
  max-width: min(760px, 100%);
  padding: 18px;
  border: 1px solid rgba(79, 224, 214, 0.65);
  border-radius: 14px;
  background: linear-gradient(135deg, rgba(5, 29, 35, 0.98), rgba(12, 75, 82, 0.98));
  box-shadow: 0 18px 50px rgba(0, 0, 0, 0.7), 0 0 28px rgba(79, 224, 214, 0.38);
  color: #d8fffb;
}

.the-silence-modal__agenda {
  width: min(280px, 34vw);
  border-radius: 12px;
  box-shadow: 0 8px 24px rgba(0, 0, 0, 0.55);
}

.the-silence-modal__body {
  display: flex;
  flex-direction: column;
  justify-content: center;
  max-width: 360px;
  font-family: Arial, sans-serif;
  text-align: left;
}

.the-silence-modal__body h2 {
  margin: 0 0 10px;
  font-family: Teutonic, Georgia, serif;
  font-size: 1.7rem;
  color: #bffff8;
}

.the-silence-modal__body p {
  margin: 0;
  line-height: 1.45;
}

.the-silence-modal__actions {
  display: flex;
  justify-content: flex-end;
  gap: 10px;
  margin-top: 18px;
}

.the-silence-modal__actions button {
  padding: 8px 14px;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 8px;
  color: white;
  cursor: pointer;
}

.the-silence-modal__confirm {
  background: rgba(12, 112, 119, 0.95);
  box-shadow: 0 0 12px rgba(79, 224, 214, 0.28);
}

@media (max-width: 650px) {
  .the-silence-modal {
    flex-direction: column;
    align-items: center;
  }

  .the-silence-modal__agenda {
    width: min(280px, 72vw);
  }
}

.revelation {
  position: absolute;
  transform: all 0.5s;
  z-index: var(--z-index-1000);
  color: white;
  text-align: center;
  margin: auto;
  inset: 0;
  width: fit-content;
  height: fit-content;
  display: grid;
  /* glow effect */
  filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
    drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue) drop-shadow(0 0 7vmin MediumOrchid);
  animation:
    revelation 0.3s ease-in-out,
    glow 4s cubic-bezier(0.55, 0.085, 0.68, 0.53) infinite;

  button {
    width: 100%;
    border: 0;
    padding: 10px;
    text-transform: uppercase;
    background-color: var(--button-2);
    font-weight: bold;
    color: #eee;
    font: Arial, sans-serif;
    &:hover {
      background-color: #311b3e;
    }

    i {
      font-style: normal;
    }

    .card {
      border-radius: 15px;
    }
  }

  h2 {
    font-family: Teutonic;
    text-transform: uppercase;
    margin: 0;
    padding: 0;
    font-size: 2.5em;
  }

  :deep(.card) {
    animation: revelation 0.6s ease-in-out;
    width: 300px !important;
    aspect-ratio: var(--card-ratio);
    overflow: hidden;
    border-radius: 15px;
  }
}

.revelation-container {
  display: flex;
  flex-direction: column;
  gap: 10px;
  align-items: center;
  align-self: center;
  align-content: center;
  justify-content: center;
  justify-items: center;
  justify-self: center;
}

@keyframes flip-back {
  0% {
    opacity: 1;
    transform: rotateY(0deg);
  }

  49% {
    opacity: 1;
  }

  50% {
    opacity: 0;
  }

  100% {
    transform: rotateY(-180deg);
    opacity: 0;
  }
}

@keyframes flip-front {
  0% {
    transform: rotateY(180deg);
    opacity: 0;
  }

  49% {
    opacity: 0;
  }

  50% {
    opacity: 1;
  }

  100% {
    opacity: 1;
    transform: rotateY(0deg);
  }
}

.revelation-card-container {
  display: flex;
  flex-direction: column;
  width: fit-content;
  height: fit-content;
  gap: 10px;

  .the-silence-card {
    width: 300px;
    aspect-ratio: var(--card-aspect);

    .the-silence-card-image {
      animation: none !important;
      width: 300px !important;
      aspect-ratio: var(--card-ratio);
      border-radius: 15px;
    }
  }

  .tarot-cards {
    gap: 15px;
    display: flex;
    flex-direction: row;
    width: fit-content;
    height: fit-content;
    &:deep(img) {
      border-radius: 10px;
    }
  }

  .revelation-card {
    position: relative;
    width: 300px;
    aspect-ratio: var(--card-aspect);
    perspective: 1000px;
    &:nth-child(1) {
      animation-delay: 0.3s;
    }

    &:nth-child(2) {
      animation-delay: 0.6s;
    }

    &:nth-child(3) {
      animation-delay: 0.9s;
    }

    :deep(.card-container) {
      transform: rotateY(-180deg);
      transform-style: preserve-3d;
      position: absolute;
      top: 0;
      left: 0;
      backface-visibility: hidden;
      animation: flip-front 0.3s linear;
      animation-fill-mode: forwards;
      animation-delay: inherit;
    }

    .card-container {
      opacity: 0;
      transform-style: preserve-3d;
    }

    .card.back {
      transform-style: preserve-3d;
      position: absolute;
      top: 0;
      left: 0;
      backface-visibility: hidden;
      animation: flip-back 0.3s linear;
      animation-fill-mode: forwards;
      animation-delay: inherit;
    }
  }

  .tarot {
    width: 300px;
    aspect-ratio: 8/14;
  }

  .tarot-card:nth-child(1) {
    animation-delay: 0.3s;
  }

  .tarot-card:nth-child(2) {
    animation-delay: 0.6s;
  }

  .tarot-card:nth-child(3) {
    animation-delay: 0.9s;
  }

  .tarot-card {
    position: relative;
    width: 300px;
    padding-bottom: 15px;
    aspect-ratio: 8/14;
    perspective: 1000px;
    .Reversed {
      transform: rotateZ(180deg);
    }
    .card-container {
      transform: rotateY(-180deg);
      transform-style: preserve-3d;
      position: absolute;
      top: 0;
      left: 0;
      backface-visibility: hidden;
      animation: flip-front 0.3s linear;
      animation-fill-mode: forwards;
      animation-delay: inherit;
    }

    img {
      pointer-events: none;
    }

    .card-container {
      opacity: 0;
      transform-style: preserve-3d;
      pointer-events: none;
      img {
        pointer-events: none;
      }
    }

    .card.back {
      transform-style: preserve-3d;
      position: absolute;
      top: 0;
      left: 0;
      backface-visibility: hidden;
      animation: flip-back 0.3s linear;
      animation-fill-mode: forwards;
      animation-delay: inherit;
    }
  }
}

.full-width {
  flex: 1;
  padding-bottom: 10px;
}

.game-bar {
  display: flex;
  margin: 0;
  padding: 0;
  background: var(--background-mid);
  div {
    &.right {
      margin-left: auto;
    }
    display: inline;
    transition: 0.3s;
    height: 100%;
    a {
      display: inline;
    }
    button {
      background: none;
      border: 0;
      display: inline;
      padding: 5px 10px;
      display: flex;
      gap: 5px;
      height: 100%;
      align-items: center;
      svg {
        width: 15px;
      }
      &:hover {
        background: rgba(0, 0, 0, 0.4);
      }
      height: 100%;
    }
  }
  justify-content: flex-start;
}

.game-bar-item.active,
.game-bar-item:hover {
  background: rgba(0, 0, 0, 0.21);
  color: var(--title);
}

.shortcuts-modal {
  display: flex;
  flex-direction: column;
  width: 100%;
  max-height: 75vh;
  background: var(--background);
  color: var(--text);
}

.shortcuts-header {
  flex-shrink: 0;
  padding: 8px 16px;
  background: var(--background-dark);
  border-bottom: 1px solid var(--box-border);
}

.shortcuts-title {
  margin: 0;
  font-family: Teutonic, serif;
  font-size: 20px;
  color: var(--text);
  text-transform: none;
}

.shortcuts-body {
  flex: 1 1 auto;
  min-height: 0;
  overflow-y: auto;
  padding: 18px 24px;
  display: flex;
  flex-direction: column;
  gap: 20px;
}

.shortcuts-section {
  display: flex;
  flex-direction: column;

  .section-title {
    margin: 0 0 10px;
    padding-bottom: 6px;
    font-family: Teutonic, serif;
    font-size: 13px;
    letter-spacing: 0.12em;
    text-transform: uppercase;
    color: var(--title);
    border-bottom: 1px solid var(--box-border);
  }
}

.shortcut-list {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
  gap: 6px;
}

.shortcut-row {
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 16px;
  align-items: center;
  padding: 10px 14px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
}

.shortcut-row:hover {
  background: var(--background-mid);
}

.shortcut-name {
  font-size: 14px;
  color: var(--text);
  min-width: 0;
}

.shortcut-keys {
  display: flex;
  align-items: center;
  gap: 6px;
  flex-shrink: 0;

  kbd {
    font-family: inherit;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    min-width: 26px;
    padding: 4px 8px;
    font-size: 12px;
    font-weight: 700;
    border-radius: 4px;
    background: var(--background-dark);
    border: 1px solid var(--box-border);
    color: var(--text);
    line-height: 1;
  }

  .chord-arrow {
    opacity: 0.5;
    font-size: 12px;
  }
}

.shortcuts-footer {
  flex-shrink: 0;
  width: 100%;
  padding: 8px 16px;
  border: none;
  border-top: 1px solid var(--box-border);
  background: var(--button-2);
  color: var(--text);
  font-family: Teutonic, serif;
  font-size: 14px;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  cursor: pointer;
  text-align: center;
}

.shortcuts-footer:hover {
  background: var(--button-2-highlight);
}

@media (max-width: 700px) {
  .shortcuts-header,
  .shortcuts-footer {
    padding-left: 16px;
    padding-right: 16px;
  }
  .shortcuts-body {
    padding: 14px 16px;
  }
}

.shortcut {
  margin-left: auto;
  border: 1px solid var(--title);
  background-color: var(--box-background);
  padding: 2px 5px;
  border-radius: 4px;
}

button:hover .shortcut {
  background-color: var(--box-border);
}

.bug-form {
  padding: 10px;
  font-size: 1.2em;
  input,
  textarea,
  button {
    font-size: 1.2em;
    padding: 5px 10px;
  }
}

.error-dialog {
  backdrop-filter: blur(3px);
  background-color: rgba(0, 0, 0, 0.8);
  position: absolute;
  padding: 0;
  padding-block: 10px;
  width: 50%;
  display: flex;
  z-index: var(--z-index-100);
  display: flex;
  flex-direction: column;
  border: 0;
  border-radius: 10px;
  top: 50%;

  p {
    padding: 10px;
    margin: 0;
  }

  h2 {
    font-family: Teutonic;
    font-size: 2em;
  }

  button {
    background: none;
    border: 0;
    display: inline;
    padding: 5px 10px;
    display: flex;
    gap: 5px;
    height: 100%;
    align-items: center;
    svg {
      width: 15px;
    }
    &:hover {
      background: rgba(0, 0, 0, 0.4);
    }
    height: 100%;
  }

  justify-content: center;
  align-items: center;
  justify-self: center;
  align-self: center;

  .error-message {
    max-height: 50vh;
    overflow: auto;
    padding-inline: 20px;
  }
}
.loader {
  z-index: var(--z-index-1000);
  position: absolute;
  top: 50px;
  left: 20px;
  width: 60px;
  aspect-ratio: 1;
  display: flex;
  color: #d0d0d099;
  border: 4px solid;
  box-sizing: border-box;
  border-radius: 50%;
  background:
    radial-gradient(circle 5px, currentColor 95%, #0000),
    linear-gradient(currentColor 50%, #0000 0) 50%/4px 60% no-repeat;
  animation: l1 30s infinite linear;
}
.loader:before {
  content: '';
  flex: 1;
  background: linear-gradient(currentColor 50%, #0000 0) 50%/4px 80% no-repeat;
  animation: inherit;
}
@keyframes l1 {
  100% {
    transform: rotate(1turn);
  }
}

.processing {
  z-index: var(--z-index-1000);
  position: absolute;
  top: 5px;
  left: 00px;
  width: 80px;
  filter: invert(48%) sepia(32%) saturate(393%) hue-rotate(37deg) brightness(92%) contrast(89%);
  aspect-ratio: 1;
}

.replay-button {
  padding: 10px;
  width: 100%;
  font-size: 1.2em;
  border: 0;
  background-color: var(--spooky-green);
  &:hover {
    background-color: var(--spooky-green-dark);
  }
}

.warning {
  background-color: var(--survivor-extra-dark);
  padding: 10px;
}

.info {
  background-color: var(--seeker-extra-dark);
  padding: 10px;
}

dialog {
  width: 400px;
  max-width: 90vw;
  padding: 20px;
  border-radius: 10px;
  background-color: var(--background);
  color: var(--title);
  font-size: 1.2em;
  margin: 0 auto;

  position: absolute;
  top: 50%;
  transform: translateY(-50%);

  &::backdrop {
    backdrop-filter: blur(3px);
    background-color: rgba(0, 0, 0, 0.8);
  }

  .buttons {
    display: flex;
    justify-content: flex-end;
    gap: 10px;
    margin-top: 10px;

    button {
      padding: 5px 10px;
      font-size: 1.2em;
    }
  }
}

.debug-modal-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.7);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: var(--z-index-1000);
}

.debug-playability-modal {
  background: #1a1a2e;
  border: 1px solid var(--button-highlight);
  border-radius: 8px;
  padding: 1.5rem;
  min-width: 300px;
  max-width: 700px;
  color: #eee;

  h3 {
    margin: 0 0 1rem;
    font-size: 1.1rem;
    color: #adf;
  }

  button {
    margin-top: 1rem;
  }
}

.debug-playability-content {
  display: flex;
  gap: 1.5rem;
  align-items: flex-start;
}

.debug-card-image {
  width: 150px;
  border-radius: 6px;
  flex-shrink: 0;
}

.playability-checks {
  list-style: none;
  padding: 0;
  margin: 0;
  flex: 1;

  li {
    padding: 0.3rem 0;
    display: flex;
    align-items: baseline;
    gap: 0.5rem;
    flex-wrap: wrap;
  }
}

.check-name {
  font-weight: 500;
}
.check-detail {
  font-size: 0.85rem;
  opacity: 0.8;
  font-style: italic;
}
.check-passed {
  color: #4f4;
}
.check-failed {
  color: #f44;
}
.check-icon {
  font-weight: bold;
  width: 1rem;
  flex-shrink: 0;
}
</style>
