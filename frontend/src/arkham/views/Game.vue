<script lang="ts" setup>

import { computed, onMounted, onUnmounted, provide, ref, shallowRef, useTemplateRef, watch } from 'vue'
import { onBeforeRouteLeave, useRouter } from 'vue-router'
import { useI18n } from 'vue-i18n'
import confetti   from '@/effects/confetti'
import { useWebSocket } from '@vueuse/core'
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
import { fetchGame, undoChoice, undoScenarioChoice, undoAction, undoTurn, undoPhase, undoRound } from '@/arkham/api'
import * as Api from '@/arkham/api'
import { useCardStore } from '@/stores/cards'
import { useUserStore } from '@/stores/user'
import { useMenu } from '@/composeable/menu'
import useEmitter from '@/composeable/useEmitter'
import { useDebug } from '@/arkham/debug'
import { imgsrc } from '@/arkham/helpers'
import { handleEmbeddedI18n } from '@/arkham/i18n'
import * as Arkham from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import { Card, cardDecoder, toCardContents } from '@/arkham/types/Card'
import * as Message from '@/arkham/types/Message'
import { type Question } from '@/arkham/types/Question'
import { TarotCard, tarotCardDecoder, tarotCardImage } from '@/arkham/types/TarotCard'
import Campaign from '@/arkham/components/Campaign.vue'
import CampaignLog from '@/arkham/components/CampaignLog.vue'
import CampaignSettings from '@/arkham/components/CampaignSettings.vue'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import CardView from '@/arkham/components/Card.vue'
import MultiplayerLobby from '@/arkham/components/MultiplayerLobby.vue'
import GameLog from '@/arkham/components/GameLog.vue'
import ScenarioSettings from '@/arkham/components/ScenarioSettings.vue'
import Settings from '@/arkham/components/Settings.vue'
import StandaloneScenario from '@/arkham/components/StandaloneScenario.vue'
import Draggable from '@/components/Draggable.vue'
import Menu from '@/components/Menu.vue'

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
  | { tag: "GameError"; contents: string }
  | { tag: "GameMessage"; contents: string }
  | { tag: "GameTarot"; contents: string }
  | { tag: "GameCard"; contents: string }
  | { tag: "GameCardOnly"; contents: string }
  | { tag: "GameUpdate"; contents: string }
  | { tag: "GameShowDiscard"; contents: string }
  | { tag: "GameShowUnder"; contents: string }
  | { tag: "GameUI"; contents: string }

export interface Props {
  gameId: string
  spectate?: boolean
}

const props = withDefaults(defineProps<Props>(), { spectate: false })

const debug = useDebug()
const emitter = useEmitter()
const router = useRouter()
const store = useCardStore()
const userStore = useUserStore()
const { addEntry, menuItems } = useMenu()
const preloaded = new Set<string>()
let mouseX = 0;
let mouseY = 0;

store.fetchCards()

interface PlayabilityInfo {
  cardId: string
  cardCode: string
  checks: [string, string | null][]
}

const game = ref<Arkham.Game | null>(null)
const gameCard = ref<GameCard | null>(null)
const playabilityInfo = ref<PlayabilityInfo | null>(null)
const gameLog = shallowRef<readonly string[]>(Object.freeze([]))
const playerId = ref<string | null>(null)
const ready = ref(false)
const resultQueue = ref<any>([])
const showLog = ref(false);
const showShortcuts = ref(false)
const showSidebar = ref(JSON.parse(localStorage.getItem("showSidebar")??'true'))
const socketError = ref(false)
const error = ref<string | null>(null)
const solo = ref(false)
const showOtherPlayersHands = ref(localStorage.getItem("showOtherPlayersHands") === "true")
watch(showOtherPlayersHands, (v) => {
  localStorage.setItem("showOtherPlayersHands", v ? "true" : "false")
})
const tarotCards = ref<TarotCard[]>([])
const uiLock = ref<boolean>(false)
const showSettings = ref(false)
const processing = ref(false)
const oldQuestion = ref<Record<string, Question> | null>(null)
const skipAllPending = ref<Set<string>>(new Set())
const { t } = useI18n();

const format = (str: string) => {
  return handleEmbeddedI18n(str, t)
}

addEntry({
  id: "viewSettings",
  icon: AdjustmentsHorizontalIcon,
  content: t('gameBar.viewSettings'),
  shortcut: "S",
  nested: 'view',
  action: () => showSettings.value = !showSettings.value
})

// Computed
const cards = computed(() => store.cards)
const choices = computed(() => {
  if (!game.value || !playerId.value) return []
  return ArkhamGame.choices(game.value, playerId.value)
})
const gameOver = computed(() => game.value?.gameState.tag === "IsOver")
const question = computed(() => playerId.value ? game.value?.question[playerId.value] : null)

function skipTriggerEntries(g: Arkham.Game): { playerId: string, choiceIdx: number }[] {
  const result: { playerId: string, choiceIdx: number }[] = []
  for (const pid of Object.keys(g.question)) {
    const cs = ArkhamGame.choices(g, pid)
    const idx = cs.findIndex((c) => c.tag === Message.MessageType.SKIP_TRIGGERS_BUTTON)
    if (idx !== -1) result.push({ playerId: pid, choiceIdx: idx })
  }
  return result
}

const skipAllAvailable = computed(() => {
  if (!solo.value || !game.value) return false
  return skipTriggerEntries(game.value).length > 1
})
const websocketUrl = computed(() => {
  const spectatePrefix = props.spectate ? "/spectate" : ""
  return `${baseURL}/api/v1/arkham/games/${props.gameId}${spectatePrefix}?token=${userStore.token}`.
    replace(/https/, 'wss').
    replace(/http/, 'ws')
})

watch(
  () => props.gameId,
  async (newV, oldV) => {
    if (!newV) return
    if (newV === oldV) return
    await fetchGame(props.gameId, props.spectate).then(async ({ game: newGame, playerId: newPlayerId, multiplayerMode}) => {
      try { await loadAllImages(newGame) } catch (e) { console.error(e) }
      window.g = newGame
      game.value = newGame
      solo.value = multiplayerMode === "Solo"
      gameLog.value = Object.freeze(newGame.log)
      playerId.value = newPlayerId
      ready.value = true
    })
  }, { immediate: true }
)

// Local Decoders
const gameCardDecoder = JsonDecoder.object<GameCard>(
  {
    title: JsonDecoder.string(),
    card: cardDecoder
  },
  'GameCard'
);

const gameCardOnlyDecoder = JsonDecoder.object<GameCardOnly>(
  {
    player: JsonDecoder.string(),
    title: JsonDecoder.string(),
    card: cardDecoder
  },
  'GameCard'
);

const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`

// Socket Handling
const onError = () => {
  processing.value = false
  if (game.value && oldQuestion.value) {
    game.value.question = oldQuestion.value
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
const qPush = (x:any)=>{ resultQueue.value.push(x) }
const qPop = ()=> {
  if (qHead >= resultQueue.value.length) { resultQueue.value = []; qHead = 0; return undefined }
  return resultQueue.value[qHead++]
}
let decoding=false
let pendingUpdate: string | null = null

function scheduleApplyUpdate(payload:string){
  if (decoding){ pendingUpdate = payload; return }
  decoding = true
  Arkham.gameDecoder.decodePromise(payload).then(async updatedGame=>{
    await loadAllImages(updatedGame)
    game.value = updatedGame
    gameLog.value = Object.freeze([...updatedGame.log])
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
  }).finally(()=>{
    decoding=false
    if (pendingUpdate){ const p = pendingUpdate; pendingUpdate = null; scheduleApplyUpdate(p) }
  })
}

function continueSkipAll() {
  if (skipAllPending.value.size === 0) return
  if (!game.value) return
  const next = skipTriggerEntries(game.value).find((e) => skipAllPending.value.has(e.playerId))
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
  game.value.question = {}
  processing.value = true
  send(JSON.stringify({tag: 'Answer', contents: { choice: choiceIdx, playerId: targetPlayerId, questionVersion }}))
}

function skipAllTriggers() {
  if (!game.value || props.spectate) return
  const entries = skipTriggerEntries(game.value)
  if (entries.length === 0) return
  skipAllPending.value = new Set(entries.map((e) => e.playerId))
  const first = entries[0]
  sendSkipFor(first.playerId, first.choiceIdx)
}

const { send, close } = useWebSocket(websocketUrl, { autoReconnect: true, onError, onConnected, onMessage })
const handleResult = (result: ServerResult) => {
  processing.value = false
  switch(result.tag) {
    case "GameError":
      if (props.spectate) return
      error.value = result.contents
      if(game.value && oldQuestion.value) {
        game.value.question = oldQuestion.value
      }
      return
    case "GameMessage":
      gameLog.value = Object.freeze([...gameLog.value, localize(result.contents)])
      return
    case "GameShowDiscard":
      emitter.emit('showDiscards', result.contents)
      return
    case "GameShowUnder":
      emitter.emit('showUnder', result.contents)
      return
    case "GameUI":
      switch (result.contents) {
        case "confetti": {
          setTimeout(() => {
            var count = 500;
            var defaults = {
              origin: { y: 0.7 }
            };

            function fire(particleRatio, opts) {
              confetti({
                ...defaults,
                ...opts,
                particleCount: Math.floor(count * particleRatio)
              });
            }

            fire(0.25, {
              spread: 26,
              startVelocity: 55,
            });
          }, 500)
        }
        default: return
      }
    case "GameTarot":
      if (props.spectate) return
      if (uiLock.value) { qPush(result); return }

      uiLock.value = true
      JsonDecoder.array(tarotCardDecoder, 'tarotCards')
        .decodePromise(result.contents)
        .then((r) => { tarotCards.value = r })
        .catch((e) => {
          console.error(e)
          uiLock.value = false
        })
      return

    case "GameCard":
      if (props.spectate) return
      if (uiLock.value) { qPush(result); return }

      uiLock.value = true
      gameCardDecoder
        .decodePromise(result as any)
        .then((r) => { gameCard.value = r })
        .catch((e) => {
          console.error(e)
          uiLock.value = false
        })
      return

    case "GameCardOnly":
      if (props.spectate) return
      if (uiLock.value) { qPush(result); return }

      uiLock.value = true
      gameCardOnlyDecoder
        .decodePromise(result as any)
        .then((r) => {
          // if it isn't for us, immediately unlock and continue draining
          console.log(solo.value, r.player, playerId.value)
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
    case "GameUpdate":
      if (uiLock.value) {
        qPush(result)
        if (game.value) game.value.question = {}
      } else {
        scheduleApplyUpdate(result.contents)
      }
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

const undoScenarioDialog = useTemplateRef<HTMLDialogElement>('undoScenarioDialog')

const actionMap = computed<Map<string, () => void>>(() => {
  const map = new Map<string, () => void>()
  for( const item of menuItems.value) {
    if (item.shortcut) map.set(item.shortcut, item.action)
  }
  return map
})

const canUndoScenario = computed(() => {
  if(!game.value) return false
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
  if (undoChordTimer) { clearTimeout(undoChordTimer); undoChordTimer = null }
}

// --- Konami Code support ---
const KONAMI_SEQ = [
  'ArrowUp','ArrowUp','ArrowDown','ArrowDown',
  'ArrowLeft','ArrowRight','ArrowLeft','ArrowRight','b','a',
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
      if (konamiTimer) { clearTimeout(konamiTimer); konamiTimer = null }
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
    if (konamiTimer) { clearTimeout(konamiTimer); konamiTimer = null }
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
    if (k === 'a' && canUndoAction.value) { clearUndoChord(); undoActionStart(); return }
    if (k === 't' && canUndoTurn.value)   { clearUndoChord(); undoTurnStart();   return }
    if (k === 'p' && canUndoPhase.value)  { clearUndoChord(); undoPhaseStart();  return }
    if (k === 'r' && canUndoRound.value)  { clearUndoChord(); undoRoundStart();  return }
    if (k === 's' && canUndoScenario.value) {
      clearUndoChord()
      undoScenarioDialog.value?.showModal()
      return
    }
    // Pressing U again while armed = single undo (re-pressing the prefix)
    if (k === 'u') { clearUndoChord(); undo(); return }
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

  if (event.key === ' ') {
    const skipTriggers = choices.value.findIndex((c) => c.tag === Message.MessageType.SKIP_TRIGGERS_BUTTON)
    if (skipTriggers !== -1) choose(skipTriggers)
    const validIndices = choices.value
      .map((c, i) => (![Message.MessageType.INVALID_LABEL, Message.MessageType.INFO].includes(c.tag) ? i : -1))
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
      if (c.component.tag !== "InvestigatorDeckComponent") return false
      if (!playerId.value) return false
      return game.value?.investigators[c.component.investigatorId]?.playerId === playerId.value
    })
    if (draw !== -1) {
      choose(draw)
    } else {
      const drawEncounter = choices.value.findIndex((c) => {
        if (c.tag !== Message.MessageType.TARGET_LABEL) return false
        return c.target.tag === "EncounterDeckTarget"
      })

      if (drawEncounter !== -1) choose(drawEncounter)
    }
    return
  }

  if (event.key === 'r') {
    const resource = choices.value.findIndex((c) => {
      if (c.tag !== Message.MessageType.COMPONENT_LABEL) return false
      if (c.component.tag !== "InvestigatorComponent") return false
      if (c.component.tokenType !== "ResourceToken") return false
      if (!playerId.value) return false
      return game.value?.investigators[c.component.investigatorId]?.playerId === playerId.value
    })
    if (resource !== -1) choose(resource)
    return
  }

  if (event.key === 'e') {
    if(!game.value || !playerId.value) return
    const elementUnderMouse = document.elementFromPoint(mouseX, mouseY);
    if (debug.active && elementUnderMouse) {
      const dataId = elementUnderMouse.getAttribute('data-id')
      if (dataId && game.value.assets[dataId]) {
        const exhausted = elementUnderMouse.classList.contains('exhausted')
        if (exhausted) {
          debug.send(game.value.id, { tag: 'Ready', contents: { tag: 'AssetTarget', contents: dataId } })
        } else {
          debug.send(game.value.id, { tag: 'Exhaust', contents: { tag: 'AssetTarget', contents: dataId } })
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
  localStorage.setItem("showSidebar", JSON.stringify(showSidebar.value))
}

// Undo
const undoLock = ref(false)
async function undo() {
  processing.value = true
  const oldQuestion = game.value?.question
  if (game.value) game.value.question = {}
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
    if (game.value && oldQuestion) game.value.question = oldQuestion
    console.log(e)
  }
  undoLock.value = false
}

async function undoScenario() {
  undoScenarioDialog.value?.close()
  processing.value = true
  if (game.value) game.value.question = {}
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
  if (game.value) game.value.question = {}
  resultQueue.value = []
  gameCard.value = null
  tarotCards.value = []
  uiLock.value = false
  undoLock.value = true
  try {
    await call(props.gameId)
  } catch (e) {
    processing.value = false
    if (game.value && oldQuestion) game.value.question = oldQuestion
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
const bugTitle = ref("")
const bugDescription = ref("")

async function fileBug() {
  submittingBug.value = true
  filingBug.value = false
  Api.fileBug(props.gameId).then((response) => {
    const title = encodeURIComponent(bugTitle.value)
    const body = encodeURIComponent(`${bugDescription.value}\n\ngame: ${window.location.href}\nfile: ${response.data}`)
    window.open(`https://github.com/halogenandtoast/ArkhamHorror/issues/new?labels=bug&title=${title}&body=${body}&assignee=halogenandtoast&projects=halogenandtoast/2`, '_blank')
    submittingBug.value = false
  }).catch(() => {
    alert(t('gameBar.bugSubmittingFail'))
    submittingBug.value = false
  })
}

const continueUI = () => {
  gameCard.value = null
  tarotCards.value = []
  uiLock.value = false
}


async function loadAllImages(game:Arkham.Game):Promise<void>{
  const pending: string[] = []
  for (const card of Object.values(game.cards)) {
    const {cardCode, isFlipped} = toCardContents(card)
    const url = imgsrc(`cards/${cardCode.replace(/^c/,'')}${isFlipped?'b':''}.avif`)
    if (!preloaded.has(url)) pending.push(url)
  }
  if (pending.length === 0) return

  await Promise.all(pending.map(url => new Promise<void>((resolve, reject)=>{
    const img = new Image()
    img.onload = () => { preloaded.add(url); resolve() }
    img.onerror = () => reject(`Could not load ${url}`)
    img.src = url
  })))
}

// Callbacks
async function choose(idx: number) {
  if (idx !== -1 && game.value && !props.spectate) {
    oldQuestion.value = game.value.question
    const questionVersion = game.value.scenarioSteps
    game.value.question = {}
    processing.value = true
    send(JSON.stringify({tag: 'Answer', contents: { choice: idx , playerId: playerId.value, questionVersion } }))
  }
}

async function chooseDeck(deckId: string): Promise<void> {
  if(game.value && !props.spectate) {
    oldQuestion.value = game.value.question
    game.value.question = {}
    processing.value = true
    send(JSON.stringify({tag: 'DeckAnswer', deckId, playerId: playerId.value}))
  }
}

async function chooseDeckList(deckList: object): Promise<void> {
  if(game.value && !props.spectate) {
    oldQuestion.value = game.value.question
    game.value.question = {}
    processing.value = true
    send(JSON.stringify({tag: 'DeckListAnswer', deckList, playerId: playerId.value}))
  }
}

async function choosePaymentAmounts(amounts: Record<string, number>): Promise<void> {
  if(game.value && !props.spectate) {
    oldQuestion.value = game.value.question
    const questionVersion = game.value.scenarioSteps
    game.value.question = {}
    processing.value = true
    send(JSON.stringify({tag: 'PaymentAmountsAnswer', contents: { amounts, questionVersion, playerId: playerId.value } }))
  }
}

async function chooseAmounts(amounts: Record<string, number>): Promise<void> {
  if(game.value && !props.spectate) {
    oldQuestion.value = game.value.question
    const questionVersion = game.value.scenarioSteps
    game.value.question = {}
    processing.value = true
    send(JSON.stringify({tag: 'AmountsAnswer', contents: { amounts, questionVersion, playerId: playerId.value } }))
  }
}

function localize(str: string): string {
  if (str.startsWith("$")) {
    return t(str.slice(1))
  }
  return str
}

async function update(state: Arkham.Game) { game.value = state }

function switchInvestigator (newPlayerId: string) { playerId.value = newPlayerId }
type ExportType = 'basic' | 'full' | 'scenario'
function debugExport (exportType: ExportType) {
  api.get(`arkham/games/${props.gameId}/${exportType == 'full' ? "full-" : (exportType == 'scenario' ? "scenario-" : "")}export`, { responseType: 'blob' })
  .then(resp => {
    const url = window.URL.createObjectURL(resp.data)
    const a = document.createElement('a')
    a.style.display = 'none'
    a.href = url
    // the filename you want
    a.download = 'arkham-debug.json'
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
provide('chooseDeck', chooseDeck)
provide('chooseDeckList', chooseDeckList)
provide('send', send)
provide('choosePaymentAmounts', choosePaymentAmounts)
provide('chooseAmounts', chooseAmounts)
provide('switchInvestigator', switchInvestigator)
provide('solo', solo)
provide('skipAllTriggers', skipAllTriggers)
provide('skipAllAvailable', skipAllAvailable)
provide('showOtherPlayersHands', showOtherPlayersHands)

const onMove = (event: MouseEvent) => {
  mouseX = event.clientX;
  mouseY = event.clientY;
}

// callbacks
const onPlayabilityResult = (result: any) => {
  if (!debug.active) return
  playabilityInfo.value = { cardId: result.cardId, cardCode: result.cardCode, checks: result.checks }
}
emitter.on('playabilityResult', onPlayabilityResult)

onMounted(() => {
  (window as any).sendDebug = async (msg: any) => { if (game.value) await debug.send(game.value.id, msg) }
  ; (window as any).undo = undo
  ; (window as any).debugChoose = choose
  document.addEventListener('mousemove', onMove, { passive: true })
  document.addEventListener('keydown', handleKeyPress)
  for (var key in localStorage){
    if (key.startsWith('selected-tab:')) localStorage.removeItem(key)
  }
})

onBeforeRouteLeave(() => close())
onUnmounted(() => {
  document.removeEventListener('keydown', handleKeyPress)
  document.removeEventListener('mousemove', onMove)
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
  <div id="game" v-else-if="ready && game && playerId">
    <dialog v-if="error" class="error-dialog">
      <h2>{{$t('error')}}</h2>
      <p class="error-message">{{error}}</p>
      <p>{{$t('errorContent')}}</p>
      <div class="buttons">
        <button @click="bugDescription = error ?? ''; error = null; filingBug = true"><ExclamationTriangleIcon aria-hidden="true" /> {{$t('fileBug')}}</button>
        <button @click="error = null">{{$t('close')}}</button>
      </div>
    </dialog>
    <div v-if="processing" class="processing">
      <LottieAnimation
        :animation-data="processingJSON"
        :auto-play="true"
        :loop="true"
        :speed="1"
        ref="anim" />
    </div>
    <CardOverlay />
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
                  <div class="shortcut-keys"><kbd>{{ item.shortcut }}</kbd></div>
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
       <input required type="text" v-model="bugTitle" v-bind:placeholder="$t('gameBar.bugTitleholder')" />
       <textarea required v-model="bugDescription" v-bind:placeholder="$t('gameBar.bugDescriptionholder')"></textarea>
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
          <button @click="router.push({name: 'CampaignLog', params: { gameId }})"><DocumentTextIcon aria-hidden="true" /> {{ showLog ? $t('gameBar.closeLog') : $t('gameBar.viewLog') }}</button>
        </div>
      </div>
      <div>
        <Menu>
          <EyeIcon aria-hidden="true" />
          {{ $t('gameBar.view') }}
          <template #items>
            <MenuItem v-slot="{ active }">
              <button :class="{ active }" @click="showShortcuts = !showShortcuts">
                <BoltIcon aria-hidden="true" /> {{ $t('gameBar.shortcuts') }} <span class="shortcut">?</span>
              </button>
            </MenuItem>
            <template v-for="item in menuItems" :key="item.id">
              <MenuItem v-if="item.nested === 'view'" v-slot="{ active }">
                <button :class="{ active }" @click="item.action">
                  <component v-if="item.icon" v-bind:is="item.icon"></component>
                  {{item.content}}
                  <span v-if="item.shortcut" class="shortcut">{{item.shortcut}}</span>
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
              <button :class="{ active }" @click="debug.toggle"><BugAntIcon aria-hidden="true" /> {{ $t('gameBar.toggleDebug') }} <span class="shortcut">D</span></button>
            </MenuItem>
            <MenuItem v-slot="{ active }">
              <button :class="{ active }" @click="debugExport('basic')"><DocumentArrowDownIcon aria-hidden="true" /> {{ $t('gameBar.debugExport') }} </button>
            </MenuItem>
            <MenuItem v-if="userStore.isAdmin" v-slot="{ active }">
              <button :class="{ active }" @click="debugExport('scenario')"><DocumentArrowDownIcon aria-hidden="true" /> {{ $t('gameBar.debugExportScenario') }} </button>
            </MenuItem>
            <MenuItem v-if="userStore.isAdmin" v-slot="{ active }">
              <button :class="{ active }" @click="debugExport('full')"><DocumentArrowDownIcon aria-hidden="true" /> {{ $t('gameBar.debugExportFull') }} </button>
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
              <button :class="{ active }" @click="undo"><BackwardIcon aria-hidden="true" /> {{ $t('gameBar.undo') }} <span class='shortcut'>u</span></button>
            </MenuItem>
            <div v-if="canUndoAction || canUndoTurn || canUndoPhase || canUndoRound || canUndoScenario" class="undo-jump-group" :class="{ armed: undoChordArmed }">
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
                <button class="undo-jump scope-scenario" :class="{ active }" @click="undoScenarioDialog && undoScenarioDialog.showModal()">
                  <FlagIcon aria-hidden="true" />
                  <span class="undo-jump-label">{{ $t('gameBar.restartScenario') }}</span>
                  <kbd class="chord-key">S</kbd>
                </button>
              </MenuItem>
            </div>
          </template>
        </Menu>
      </div>
      <div><button @click="filingBug = true"><ExclamationTriangleIcon aria-hidden="true" /> {{ $t('fileBug') }} </button></div>
      <div v-for="item in menuItems" :key="item.id">
        <template v-if="item.nested === null || item.nested === undefined">
          <button @click="item.action">
            <component v-if="item.icon" v-bind:is="item.icon"></component>
            {{item.content}}
          </button>
        </template>
      </div>
      <div class="right">
        <button @click="toggleSidebar"><ArrowsRightLeftIcon aria-hidden="true" /> {{ $t('gameBar.toggleSidebar') }} </button>
      </div>
    </div>
    <MultiplayerLobby
      v-if="game.gameState.tag === 'IsPending'"
      :game-id="gameId"
      :game="game"
      :player-id="playerId"
    />
    <template v-else>
      <Draggable v-if="showSettings">
      <Settings :game="game" :playerId="playerId" :solo="solo" v-model:showOtherPlayersHands="showOtherPlayersHands" :closeSettings="() => showSettings = false" />
      </Draggable>
      <CampaignLog v-if="showLog && game !== null" :game="game" :cards="cards" :playerId="playerId" />
      <div v-else class="game-main">
        <div v-if="gameCard" class="revelation">
          <div class="revelation-container">
            <h2>{{format(gameCard.title)}}</h2>
            <div class="revelation-card-container">
              <div class="revelation-card">
                <CardView :game="game" :card="gameCard.card" :playerId="playerId" />
                <img v-if="gameCard.card.tag === 'PlayerCard'" :src="imgsrc('player_back.jpg')" class="card back" />
                <img v-else :src="imgsrc('back.png')" class="card back" />
              </div>
              <button @click="continueUI">{{ $t('ok') }}</button>
            </div>
          </div>
        </div>
        <div v-if="playabilityInfo && debug.active" class="debug-modal-overlay" @click.self="playabilityInfo = null">
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
                <div
                    v-for="(tarotCard, idx) in tarotCards"
                    :key="idx"
                    class="tarot-card"
                  >
                  <div class="card-container">
                    <img :src="imgsrc(`tarot/${tarotCardImage(tarotCard)}`)" class="tarot" :class="tarotCard.facing" />
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
          @choose="choose"
          @update="update"
        />
        <ScenarioSettings
          v-else-if="game.scenario && !gameOver && question && question.tag === 'PickScenarioSettings'"
          :game="game"
          :scenario="game.scenario"
          :playerId="playerId"
        />
        <StandaloneScenario
          v-else-if="game.scenario && !gameOver"
          :game="game"
          :playerId="playerId"
          @choose="choose"
          @update="update"
        />
        <div class="sidebar" v-if="showSidebar && game.scenario !== null && (game.gameState.tag === 'IsActive' || game.gameState.tag === 'IsOver')">
          <GameLog :game="game" :gameLog="gameLog" @undo="undo" />
        </div>
        <div class="game-over" v-if="gameOver">
          <p>{{ $t('gameOver') }}</p>
          <button class="replay-button" @click="router.push({name: 'ReplayGame', params: { gameId }})">{{ $t('watchReplay') }}</button>
          <CampaignLog v-if="game !== null" :game="game" :cards="cards" :playerId="playerId" />
        </div>
        <div class="sidebar" v-if="showSidebar && game.scenario === null">
          <GameLog :game="game" :gameLog="gameLog" @undo="undo" />
        </div>
      </div>
    </template>
    <dialog id="undoScenarioDialog" ref="undoScenarioDialog">
      <p>{{ $t('game.areYouSureUndoScenario') }}</p>
      <div class="buttons">
        <button @click="undoScenario()">{{ $t('Yes') }}</button>
        <button @click="undoScenarioDialog?.close()">{{ $t('No') }}</button>
      </div>
    </dialog>
  </div>
</template>

<style lang="scss" scoped>
.action { border: 5px solid var(--select); border-radius: 15px; }

.undo-jump-group {
  background: rgba(0, 0, 0, 0.22);
  box-shadow: inset 0 1px 2px rgba(0, 0, 0, 0.25);
  border-bottom-left-radius: 5px;
  border-bottom-right-radius: 5px;
  overflow: hidden;
  transition: box-shadow 0.2s ease, background 0.2s ease;

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
    transition: opacity 0.15s ease, transform 0.15s ease;
  }

  svg {
    color: var(--undo-scope);
  }

  &.scope-action   { --undo-scope: #7fb8d4; }
  &.scope-turn     { --undo-scope: #6cc28d; }
  &.scope-phase    { --undo-scope: #e0b256; }
  &.scope-round    { --undo-scope: #c97aa8; }
  &.scope-scenario { --undo-scope: #d96a6a; }

  &:hover {
    background: rgba(0, 0, 0, 0.6);
  }

  &:hover::before, &.active::before {
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

.game-main {
  width: 100vw;
  height: calc(100vh - 80px);
  display: flex;
  flex: 1;
}

.socketWarning  {
  backdrop-filter: blur(3px);
  background-color: rgba(0,0,0,0.8);
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  display: flex;
  z-index: 100;

  justify-content: center;
  align-items: center;
  justify-self: center;
  align-self: center;

  p {
    padding: 10px;
    background: #FFF;
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
    display: none;
  }

  @media (prefers-color-scheme: dark) {
    background: #1C1C1C;
  }
}

#invite {
  background-color: #15192C;
  color: white;
  width: 800px;
  margin: 0 auto;
  margin-top: 20px;
  border-radius: 5px;
  text-align: center;
  p { margin: 0; padding: 0; margin-bottom: 20px; font-size: 1.3em; }
  @media (max-width: 800px) and (orientation: portrait){
    width: 100%;
  }
}

.invite-container {
  margin-top: 50px;
  h2 {
    color: #656A84;
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
        z-index: 9998;
        top: 35px;
        left: 15px;
        width: 0;
        height: 0;

        border-left: 5px solid transparent;
        border-right: 5px solid transparent;
        border-bottom: 5px solid rgba(0,0,0,.72);
    }

    &:after {
      content: 'Copied!';
      display: none;
      position: absolute;
      z-index: 9999;
      top: var(--nav-height);
      left: -37px;
      width: 114px;
      height: 36px;

      color: #fff;
      font-size: 10px;
      line-height: 36px;
      text-align: center;

      background: rgba(0,0,0,.72);
      border-radius: 3px;
    }

    &:active, &:focus {
      outline: none;

      &:hover {
        background-color: #eee;

        &:before, &:after {
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
  syntax: "<angle>";
  initial-value: 0deg;
  inherits: false;
}

@keyframes rotation {
  0% { --gradient-angle: 360deg; }
  100% { --gradient-angle: 0deg; }
}

@keyframes glow {
  0% {
    filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
      drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue)
      drop-shadow(0 0 7vmin MediumOrchid);
  }
  50% {
    filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
      drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue)
      drop-shadow(0 0 7vmin Black);
  }
  100% {
    filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
      drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue)
      drop-shadow(0 0 7vmin MediumOrchid);
  }
}

.revelation {
  position: absolute;
  transform: all 0.5s;
  z-index: 1000;
  color: white;
  text-align:center;
  margin: auto;
  inset: 0;
  width: fit-content;
  height: fit-content;
  display: grid;
  /* glow effect */
  filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
		drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue)
		drop-shadow(0 0 7vmin MediumOrchid);
  animation: revelation 0.3s ease-in-out, glow 4s cubic-bezier(0.550, 0.085, 0.680, 0.530) infinite;

  button {
    width: 100%;
    border: 0;
    padding: 10px;
    text-transform: uppercase;
    background-color: #532e61;
    font-weight: bold;
    color: #EEE;
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
        background: rgba(0,0,0,0.4);
      }
      height: 100%;
    }
  }
  justify-content: flex-start;
}

.game-bar-item.active, .game-bar-item:hover {
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
  input, textarea, button {
    font-size: 1.2em;
    padding: 5px 10px;
  }

}

.error-dialog {
  backdrop-filter: blur(3px);
  background-color: rgba(0,0,0,0.8);
  position: absolute;
  padding: 0;
  padding-block: 10px;
  width: 50%;
  display: flex;
  z-index: 100;
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
      background: rgba(0,0,0,0.4);
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
  z-index: 1000;
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
    radial-gradient(circle 5px, currentColor 95%,#0000),
    linear-gradient(currentColor 50%,#0000 0) 50%/4px 60% no-repeat;
  animation: l1 30s infinite linear;
}
.loader:before {
  content: "";
  flex: 1;
  background:linear-gradient(currentColor 50%,#0000 0) 50%/4px 80% no-repeat;
  animation: inherit;
}
@keyframes l1 {
  100% {transform: rotate(1turn)}
}

.processing {
  z-index: 1000;
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
    background-color: rgba(0,0,0,0.8);
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
  background: rgba(0,0,0,0.7);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.debug-playability-modal {
  background: #1a1a2e;
  border: 1px solid #444;
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

.check-name { font-weight: 500; }
.check-detail { font-size: 0.85rem; opacity: 0.8; font-style: italic; }
.check-passed { color: #4f4; }
.check-failed { color: #f44; }
.check-icon { font-weight: bold; width: 1rem; flex-shrink: 0; }
</style>
