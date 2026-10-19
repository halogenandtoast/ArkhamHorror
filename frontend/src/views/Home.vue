<script lang="ts" setup>
import { ref, computed, Ref } from 'vue';
import { useUserStore } from '@/stores/user';
import { useCardStore } from '@/stores/cards';
import { useRouter } from 'vue-router';
import { deleteEvent, deleteGame, fetchGames, fetchEvents, fetchNotifications, fetchSpectatableGameIds } from '@/arkham/api';
import { cullGameLocalStorage, removeGameLocalStorage } from '@/arkham/localStorage';
import { isDevBuild } from '@/arkham/displayRules';
import type { BrokenGameDetails, GameDetails } from '@/arkham/types/Game';
import type { EventListEntry } from '@/arkham/types/EpicEvent';
import type { AppNotification } from '@/arkham/api';
import GameRow from '@/arkham/components/GameRow.vue';
import EventRow from '@/arkham/components/EventRow.vue';
import ImportGame from '@/arkham/components/ImportGame.vue';
import PrimaryButton from '@/components/PrimaryButton.vue';
import SegmentedToggle from '@/components/SegmentedToggle.vue';
import { storeToRefs } from 'pinia';
import { useI18n } from 'vue-i18n';

const { t } = useI18n()
const router = useRouter()
const store = useUserStore()
const cardStore = useCardStore()
const { currentUser } = storeToRefs(store)
const games: Ref<GameDetails[]> = ref([])
const brokenGames: Ref<BrokenGameDetails[]> = ref([])
const events: Ref<EventListEntry[]> = ref([])
const notifications: Ref<AppNotification[]> = ref([])

const dismissedNotifications = JSON.parse(localStorage.getItem('dismissedNotifications') ?? "[]")

const activeGames = computed(() => games.value.filter(g => g.gameState.tag !== 'IsOver'))
const finishedGames = computed(() => games.value.filter(g => g.gameState.tag === 'IsOver'))

type Tab = 'active' | 'finished'
type OutcomeFilter = 'all' | 'won' | 'lost'

/* Null until the player picks a tab, so that until they do we can land them on
 * whichever tab actually has something in it -- someone whose games are all
 * finished should not arrive at an empty page. */
const chosenTab = ref<Tab | null>(null)
const tab = computed<Tab>(() => {
  if (chosenTab.value) return chosenTab.value
  if (activeGames.value.length > 0 || events.value.length > 0 || brokenGames.value.length > 0) return 'active'
  return finishedGames.value.length > 0 ? 'finished' : 'active'
})

const outcomeFilter = ref<OutcomeFilter>('all')
const wonGames = computed(() => finishedGames.value.filter(g => g.outcome?.result === 'Won'))
const lostGames = computed(() => finishedGames.value.filter(g => g.outcome?.result === 'Lost'))

/* A finished game can still have no verdict -- a standalone, or a campaign that
 * ended in a way we cannot read -- so Won and Lost need not add up to All. */
const shownFinishedGames = computed(() => {
  switch (outcomeFilter.value) {
    case 'won': return wonGames.value
    case 'lost': return lostGames.value
    default: return finishedGames.value
  }
})

const tabOptions = computed(() => [
  { value: 'active' as Tab, label: t('activeGames'), count: activeGames.value.length + events.value.length },
  { value: 'finished' as Tab, label: t('finishedGames'), count: finishedGames.value.length },
])

const outcomeOptions = computed(() => [
  { value: 'all' as OutcomeFilter, label: t('home.allOutcomes'), count: finishedGames.value.length },
  { value: 'won' as OutcomeFilter, label: t('gameRow.won'), count: wonGames.value.length },
  { value: 'lost' as OutcomeFilter, label: t('gameRow.lost'), count: lostGames.value.length },
])

fetchGames().then((result) => {
  const availableGames = result.filter((g) => g.tag === 'game') as GameDetails[]
  cullGameLocalStorage(availableGames)
  games.value = availableGames
  /* One request for every custom investigator on the page, rather than each row
   * buying the whole of its game's card set to read a portrait off it. */
  cardStore.ensureCustomCardPortraits(
    availableGames.flatMap((game) => [
      ...game.investigators.map((investigator) => investigator.id),
      ...Object.values(game.otherInvestigators).map((investigator) => investigator.id),
    ]),
  )
  // Only dev builds get the broken-game rows: in production an unloadable game
  // is noise a player cannot act on beyond deleting it.
  brokenGames.value = isDevBuild() ? (result.filter((g) => g.tag === 'error') as BrokenGameDetails[]) : []
})

// Epic Multiplayer events surface as a single entry each, inline with regular
// games (group games are hidden from fetchGames by the backend). A user who is
// both organizer and player of an event gets duplicate membership rows; collapse
// to one entry, preferring the organizer role.
fetchEvents().then((result) => {
  const byId = new Map<string, EventListEntry>()
  for (const entry of result) {
    const existing = byId.get(entry.id)
    if (!existing || entry.role === 'organizer') byId.set(entry.id, entry)
  }
  events.value = [...byId.values()]
})

fetchNotifications().then((result) => notifications.value = result.filter((n: AppNotification) => !dismissedNotifications.includes(n.id)))

/* How many games are open to spectators right now, for the badge on the Watch
 * button. The digest is the ids-only probe the watch page polls, so this is a
 * handful of uuids rather than a game apiece -- but it is read once on mount
 * and left alone: the number is an invitation to click through, and the page
 * behind the button is the one that keeps itself current. A failure just leaves
 * the button unbadged. */
const liveGameCount = ref(0)
fetchSpectatableGameIds()
  .then((ids) => liveGameCount.value = ids.length)
  .catch(() => liveGameCount.value = 0)

async function deleteGameEvent(game: GameDetails) {
  deleteGame(game.id).then(() => {
    removeGameLocalStorage(game.id)
    games.value = games.value.filter((g) => g.id !== game.id);
  });
}

async function deleteBrokenGame(game: BrokenGameDetails) {
  deleteGame(game.id).then(() => {
    removeGameLocalStorage(game.id)
    brokenGames.value = brokenGames.value.filter((g) => g.id !== game.id)
  })
}

async function deleteEpicEvent(event: EventListEntry) {
  deleteEvent(event.id).then(() => {
    events.value = events.value.filter((e) => e.id !== event.id)
  })
}

const showImportGame = ref(false)
const importGameRef = ref<any>(null)
const importGameSelected = computed(() => !!importGameRef.value?.selectedFile)
const importGameCanSubmit = computed(() => importGameRef.value?.canSubmit ?? false)
const importGameLoading = computed(() => importGameRef.value?.loading ?? false)
const submitImportGame = () => importGameRef.value?.submit()

const goToNewGame = () => router.push({ path: '/new-game' })

const toggleImportGame = () => {
  showImportGame.value = !showImportGame.value
}

const dismissNotification = (notification: AppNotification) => {
  localStorage.setItem('dismissedNotifications', JSON.stringify([notification.id, ...dismissedNotifications]))
  notifications.value = notifications.value.filter(n => n.id !== notification.id)
}
</script>

<template>
  <div class="page-container">
    <div class="home page-content">
      <div class="notification" v-for="notification in notifications" :key="notification.id">
        <p v-html="notification.body"></p>
        <a @click.prevent="dismissNotification(notification)" href="#">{{ $t('home.dismiss') }}</a>
      </div>

      <div class="container">
        <section>
          <header class="main-header">
            <div class="title-group">
              <h2>{{$t('home.yourGames')}}</h2>
              <SegmentedToggle
                class="tabs"
                :model-value="tab"
                :options="tabOptions"
                :label="$t('home.tabsLabel')"
                @update:model-value="chosenTab = $event as Tab"
              />
            </div>
            <div class="header-actions">
              <!-- Other people's games are a page of their own: they are not
                   yours, and the only thing to do with one is watch it. -->
              <RouterLink class="secondary-cta watch-cta" to="/watch">
                <font-awesome-icon icon="eye" />
                {{ $t('home.watchGames') }}
                <span
                  v-if="liveGameCount > 0"
                  class="watch-count"
                  :aria-label="$t('home.watchGamesAvailable', { count: liveGameCount }, { plural: liveGameCount })"
                >{{ liveGameCount }}</span>
              </RouterLink>
              <button v-if="currentUser" class="secondary-cta" type="button" @click="toggleImportGame">
                {{ $t('home.loadGame') }}
              </button>
              <PrimaryButton :label="$t('newGame')" @click="goToNewGame" />
            </div>
          </header>
          <Transition name="slide">
            <div v-if="currentUser && showImportGame" class="load-game-panel">
              <div class="panel-header">
                <h3>{{ $t('home.loadGame') }}</h3>
                <div class="panel-actions">
                  <button class="panel-close" type="button" @click="toggleImportGame">{{ $t('cancel') }}</button>
                  <button
                    v-if="importGameSelected"
                    class="panel-load"
                    type="button"
                    :disabled="!importGameCanSubmit"
                    @click="submitImportGame"
                  >
                    {{ importGameLoading ? 'Loading…' : 'Load Game' }}
                  </button>
                </div>
              </div>
              <ImportGame ref="importGameRef" />
            </div>
          </Transition>
          <template v-if="tab === 'active'">
            <div v-if="activeGames.length === 0 && events.length === 0 && brokenGames.length === 0" class="box">
              <p>{{ $t('home.noActiveGames') }}</p>
            </div>
            <div v-for="game in brokenGames" :key="game.id" class="broken-game">
              <div class="broken-game-details">
                <span class="broken-game-title">{{ game.name }}</span>
                <span class="broken-game-error">{{ game.error }}</span>
              </div>
              <a href="#delete" class="broken-game-delete" @click.prevent="deleteBrokenGame(game)">
                <font-awesome-icon icon="trash" />
              </a>
            </div>
            <EventRow
              v-for="event in events"
              :key="event.id"
              :event="event"
              :deleteEvent="() => deleteEpicEvent(event)"
            />
            <GameRow v-for="game in activeGames" :key="game.id" :game="game" :deleteGame="() => deleteGameEvent(game)" />
          </template>

          <template v-else>
            <SegmentedToggle
              v-if="finishedGames.length > 0"
              class="outcome-filter"
              :model-value="outcomeFilter"
              :options="outcomeOptions"
              :label="$t('home.outcomeFilterLabel')"
              @update:model-value="outcomeFilter = $event as OutcomeFilter"
            />
            <div v-if="shownFinishedGames.length === 0" class="box">
              <p>{{ finishedGames.length === 0 ? $t('home.noFinishedGames') : $t('home.noGamesForOutcome') }}</p>
            </div>
            <GameRow
              v-for="game in shownFinishedGames"
              :key="game.id"
              :game="game"
              :deleteGame="() => deleteGameEvent(game)"
            />
          </template>
        </section>
      </div>
    </div>
  </div>

</template>

<style scoped>
/* Switching tabs changes how tall the page is, and a scrollbar that appears and
   disappears with it shifts the whole column sideways under the pointer. Hold
   the gutter open whether or not it is needed. */
.page-container {
  scrollbar-gutter: stable;
}

.broken-game {
  display: flex;
  gap: 10px;
  align-items: center;
  padding: 10px;
  margin-bottom: 10px;
  border: 1px solid var(--delete);
  border-radius: 3px;
  background-color: var(--box-background);
}

.broken-game-details {
  flex: 1;
  display: flex;
  flex-direction: column;
  min-width: 0;
}

.broken-game-title {
  font-family: teutonic, sans-serif;
  font-size: 1.6em;
  color: var(--title);
}

.broken-game-error {
  font-size: 0.8em;
  color: var(--delete);
  overflow-wrap: anywhere;
}

.broken-game-delete {
  padding: 0;
  font-size: 1.2em;
  color: var(--delete);
  &:hover {
    color: #990000;
  }
}

h2 {
  color: var(--title);
  font-size: 2em;
  text-transform: uppercase;
  font-family: teutonic, sans-serif;
  margin: 0;
  @media (max-width: 768px) {
    font-size: 1.5em;
  }
}

.new-game {
  button {
    border-radius: 3px;
    outline: 0;
    padding: 10px 15px;
    background: var(--spooky-green);
    text-transform: uppercase;
    color: white;
    border: 0;
    width: 100%;
    &:hover {
      background: hsl(80, 35%, 32%);
    }
  }
}

/* The toggles here are chrome: a muted pill, so the eye still goes to New Game. */
.home {
  --segmented-accent: var(--button-1-muted);
  --segmented-accent-highlight: var(--button-1-muted-highlight);
  max-width: 98vw;
  min-width: 60vw;
  margin: 0 auto;
  @media (max-width: 768px) {
    min-width: unset;
    width: 100%;
    padding: 20px 12px 10px;
    box-sizing: border-box;
  }
}

.slide-enter-active,
.slide-leave-active {
  transition: all 0.3s ease-in-out;
}

.slide-enter-to,
.slide-leave-from {
  opacity: 1;
}

.slide-enter-from,
.slide-leave-to {
  opacity: 0;
}

button {
  transition: background-color 0.3s linear;
}

button.new-game-button {
  height: fit-content;
  align-self: center;
  font-size: 1em;
  font-weight: bolder;
  width: fit-content;
  margin-block: 10px;
}

p {
  margin: 0;
  padding: 0;
}

.box {
  background-color: var(--box-background);
  border: 1px solid var(--box-border);
  color: var(--title);
  padding: 10px;
  border-radius: 5px;
}

form.box {
  display: flex;
  flex-direction: column;
  gap: 10px;

  button {
    text-transform: uppercase;
    padding: 10px;
  }
}

header {
  display: flex;
  margin-bottom: 10px;
  align-items: center;
  gap: 12px;
  h2 {
    flex: 1;
  }

  button {
    height: fit-content;
    align-self: center;
    background-color: var(--button-1);
    border-radius: 3px;
    outline: 0;
    padding: 10px 15px;
    text-transform: uppercase;
    color: white;
    border: 0;
    font-size: 1em;
    font-weight: bolder;
    &:hover {
      background: hsl(80, 35%, 32%);
    }
  }
}

.games {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.header-actions {
  display: flex;
  align-items: center;
  gap: 8px;
  flex-wrap: wrap;
  justify-content: flex-end;
}

.secondary-cta {
  align-self: center;
  background: transparent;
  border: 1px solid var(--box-border);
  border-radius: 3px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.85em;
  font-weight: 700;
  opacity: 0.8;
  outline: 0;
  padding: 8px 12px;
  text-transform: uppercase;

  &:hover {
    background: rgba(255, 255, 255, 0.06);
    opacity: 1;
  }

  @media (max-width: 768px) {
    padding: 6px 9px;
    font-size: 0.75em;
  }
}

.load-game-panel {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  padding: 12px;
  margin-bottom: 12px;
  box-shadow: 0 8px 24px rgba(0, 0, 0, 0.18);
}

.panel-header {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-bottom: 10px;
}

.panel-header h3 {
  flex: 1;
  margin: 0;
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 1.4em;
  text-transform: uppercase;
}

.panel-actions {
  display: flex;
  align-items: center;
  gap: 8px;
}

.panel-load,
.panel-close {
  border: 1px solid var(--box-border);
  border-radius: 3px;
  cursor: pointer;
  font-size: 0.8em;
  font-weight: bolder;
  padding: 7px 10px;
  text-transform: uppercase;
}

.panel-load {
  background: var(--spooky-green);
  border-color: var(--spooky-green);
  color: white;

  &:hover:not(:disabled) {
    background: hsl(80, 35%, 32%);
  }

  &:disabled {
    cursor: not-allowed;
    opacity: 0.55;
  }
}

.panel-close {
  background: transparent;
  color: var(--title);

  &:hover {
    background: rgba(255, 255, 255, 0.06);
  }
}

.notification {
  --text: #816F3A;
  --border: var(--text);
  --background: #FFF8E6;
  display: flex;
  flex-direction: row;
  box-sizing: border-box;
  padding: 10px;
  border: 2px solid var(--border);
  color: var(--text);
  margin-block: 10px;
  font-size: 1.2em;
  border-radius: 5px;
  background-color: var(--background);
  gap: 5px;

  > p {
    flex: 1;
  }

  :deep(a) {
    color: var(--seeker-dark);
    text-decoration: underline;
  }
}

/* The tabs belong to the title, not to the actions, so they travel with it and
   the pair wraps together once the header stops fitting on one line. */
.main-header {
  flex-wrap: wrap;
}

.title-group {
  display: flex;
  flex: 1;
  flex-wrap: wrap;
  align-items: center;
  gap: 12px;

  h2 {
    flex: none;
  }
}

.tabs {
  flex: none;
  min-width: 220px;
}

@media (max-width: 768px) {
  .main-header {
    flex-direction: column;
    align-items: stretch;
  }

  .header-actions {
    justify-content: flex-start;
  }
}

.outcome-filter {
  margin-bottom: 10px;
  max-width: 360px;
}

/* A link wearing a button's clothes, so it needs the centring a button has. */
.watch-cta {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  text-decoration: none;
}

/* Reads as a count on the button rather than as a second word in its label, so
   it keeps the pill shape at any number of digits. */
.watch-count {
  background: var(--background-dark);
  border-radius: 999px;
  font-variant-numeric: tabular-nums;
  line-height: 1;
  min-width: 1.6em;
  padding: 3px 6px;
  text-align: center;
}
</style>
