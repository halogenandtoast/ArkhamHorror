<script lang="ts" setup>
import { computed, watch, ref } from 'vue'
import { storeToRefs } from 'pinia'
import { useI18n } from 'vue-i18n'
import { portraitImage } from '@/arkham/cardImages'
import {
  fetchArkhamBuildConnection,
  fetchArkhamBuildDecks,
  fetchInvestigators,
  newDeck,
  validateDeck,
  type ArkhamBuildDeck,
} from '@/arkham/api'
import ArkhamBuildLogo from '@/components/icons/ArkhamBuildLogo.vue'
import ArkhamDbDeck from '@/arkham/components/ArkhamDbDeck.vue';
import { ArkhamDbDecklist } from '@/arkham/types/Deck';
import { useCardStore } from '@/stores/cards'
import { isDevBuild } from '@/arkham/displayRules'
import { normalizeArkhamBuildDeckCodes } from '@/arkham/arkhamBuildImport'
import { libraryCard, loadLibrary } from '@/arkham/customCardLibrary'
import { arkhamBuildCustomCardCode } from '@/arkham/customCards'

const { t } = useI18n()

const props = withDefaults(defineProps<{
  noPortrait?: boolean
  alwaysSave?: boolean
  setPortrait?: (src: string) => void
}>(), { noPortrait: false, alwaysSave: false })

const emit = defineEmits(['newDeck', 'newDeckList'])

const store = useCardStore()
const investigators = ref<string[]>([])
const { cards }  = storeToRefs(store)

investigators.value = await fetchInvestigators()

/* Where the deck is coming from. The arkham.build tab is dev-only while the
 * integration settles, and beyond that is offered only when this server has an
 * arkham.build client at all. It leads when the player has actually connected
 * one -- that is the list they will nearly always want. */
type Source = 'build' | 'link' | 'file'

const dev = isDevBuild()
const buildConfigured = ref(false)
const buildConnected = ref(false)
if (dev) {
  try {
    const status = await fetchArkhamBuildConnection()
    buildConfigured.value = status.configured
    buildConnected.value = status.connection !== null
  } catch {
    /* Loading a deck by link must keep working whether or not this answers. */
  }
}

const source = ref<Source>(buildConnected.value ? 'build' : 'link')
const sources = computed<Source[]>(() =>
  dev && buildConfigured.value ? ['build', 'link', 'file'] : ['link', 'file'],
)
const sourceLabels: Record<Source, string> = {
  build: 'newDeck.sourceArkhamBuild',
  link: 'newDeck.sourceLink',
  file: 'newDeck.sourceFile',
}

const buildDecks = ref<ArkhamBuildDeck[]>([])
const buildLoaded = ref(false)
const buildLoading = ref(false)
const buildError = ref(false)
const buildArkhamdbAvailable = ref(true)
const buildSearch = ref('')
const selectedBuildKey = ref<string | null>(null)

const buildDeckKey = (d: ArkhamBuildDeck) => `${d.source}:${d.id}`

const visibleBuildDecks = computed(() => {
  const needle = buildSearch.value.trim().toLowerCase()
  const matches = needle
    ? buildDecks.value.filter((d) => d.decklist.name.toLowerCase().includes(needle) ||
        (d.decklist.investigator_name ?? '').toLowerCase().includes(needle))
    : buildDecks.value
  return [...matches].sort((a, b) => b.updatedAt.localeCompare(a.updatedAt))
})

async function loadBuildDecks() {
  buildLoading.value = true
  buildError.value = false
  try {
    const data = await fetchArkhamBuildDecks()
    buildDecks.value = data.decks
    buildArkhamdbAvailable.value = data.providers.arkhamdb
    buildLoaded.value = true
  } catch {
    buildError.value = true
  } finally {
    buildLoading.value = false
  }
}

function chooseBuildDeck(d: ArkhamBuildDeck) {
  selectedBuildKey.value = buildDeckKey(d)
  deckList.value = d.decklist
}

/* Fetching is two round trips to arkham.build, so it waits until the tab is
 * actually looked at, and is not repeated once it has answered. */
watch(
  [source, buildConnected],
  ([s, connected]) => {
    if (s === 'build' && connected && !buildLoaded.value && !buildLoading.value) loadBuildDecks()
  },
  { immediate: true },
)

interface UnimplementedCardError {
  tag: string
  contents: string
}

function validationErrorsFromResponse(err: unknown): string[] {
  const response = err as { response?: { data?: unknown } }
  // No response object at all means the request never completed -- it timed out,
  // the connection dropped, or something blocked it. That is not the server
  // telling us anything about the deck, so it must not be reported as one.
  if (!response.response) {
    requestFailed.value = true
    return []
  }
  const payload = response.response?.data
  if (!Array.isArray(payload)) {
    if (payload && typeof payload === 'object' && 'message' in payload) {
      return [String((payload as { message: unknown }).message)]
    }
    return ['Unable to validate deck']
  }

  return payload.map((e) => {
    const code = typeof e === 'object' && e !== null && 'contents' in e
      ? String((e as UnimplementedCardError).contents)
      : String(e)
    const key = normalizeCode(code)
    const hit = cardByCode.value.get(key)
    if (hit) return hit.xp ? `${hit.name.title} (${hit.xp})` : hit.name.title
    /* Not an official card, so try the custom library before giving up on the
     * name. The code arrives either as this app's own derived code or as the
     * arkham.build id the deck was built against, which derives to it. A def
     * stores its code with the leading `c` that `normalizeCode` strips. */
    const custom =
      libraryCard(`c${key}`) ?? libraryCard(`c${arkhamBuildCustomCardCode(key)}`)
    if (custom) return `${custom.def.name.title} (custom)`
    return `Unknown card: ${code}`
  })
}

interface ArkhamDBCard {
  name: { title: string; subtitle: string | null }
  cardCode: string
  xp?: string | number | null
}

const errors = ref<string[]>([])
const requestFailed = ref(false)
const valid = ref(false)
const saveDeck = computed(() => props.alwaysSave ? true : saveDeckToggle.value)
const saveDeckToggle = ref(true)
const investigatorError = ref<string | null>(null)
const investigator = ref<string | null>(null)
const deck = ref<string | null>(null)
const deckId = ref<string | null>(null)
const deckName = ref<string | null>(null)
const deckUrl = ref<string | null>(null)
const deckList = ref<ArkhamDbDecklist | null>(null)
const normalizeCode = (code: string) => code.replace(/^c/, '')
const isInvestigatorImplemented = (code: string) =>
  investigators.value.includes(code) || investigators.value.includes(normalizeCode(code))
/* `portraitImage` rather than the portraits directory: a custom investigator has
 * nothing under it, and carries its own portrait in meta. */
const maybeSetPortrait = (code: string | null | undefined) => {
  if (!code || !props.setPortrait) return
  props.setPortrait(portraitImage(code))
}
const resolvedInvestigatorCode = (d: ArkhamDbDecklist) => {
  const meta = (() => {
    if (typeof d.meta !== 'string') return d.meta
    try {
      return JSON.parse(d.meta || '{}')
    } catch (_e) {
      return {}
    }
  })()
  return meta?.alternate_front ?? d.investigator_code
}

function loadDeckFromFile(e: Event) {
  valid.value = false
  const files = (e.target as HTMLInputElement).files || (e as DragEvent).dataTransfer?.files || [];
  const deck = files[0]
  if (deck) {
    const reader = new FileReader()
    reader.onloadend = (e1: ProgressEvent<FileReader>) => {
      if(!e1?.target?.result) return
      // A file downloaded straight from arkham.build has the same shape its
      // share API returns, including bare-UUID custom card codes, which need
      // the same rewrite `processArkhamBuildDeck` applies to a fetched deck --
      // this path bypasses that function entirely, so it has to be done here.
      let data = normalizeArkhamBuildDeckCodes(JSON.parse(e1.target.result.toString()))
      deckList.value = data
      investigator.value = null
      investigatorError.value = null
      if (isInvestigatorImplemented(data.investigator_code)) {
        investigator.value = data.meta?.alternate_front ?? data.investigator_code
        maybeSetPortrait(investigator.value)

      } else {
        investigatorError.value = `${data.investigator_name} is not yet implemented, please use a different deck ${data}`
      }
      deckId.value = data.id.toString()
      deckName.value = data.name

      runValidations()
    }
    reader.readAsText(deck)
  }
}

watch(deckList, loadDeck)

async function loadDeck() {
  valid.value = false
  errors.value = []
  requestFailed.value = false
  investigator.value = null
  investigatorError.value = null

  const dl = deckList.value
  if (!dl) return

  const invCode = resolvedInvestigatorCode(dl)
  const invImplemented = isInvestigatorImplemented(dl.investigator_code)

  if (invImplemented) {
    investigator.value = invCode
    maybeSetPortrait(invCode)
  } else {
    investigatorError.value =
      `${dl.investigator_name} is not yet implemented, please use a different deck ${JSON.stringify(dl)}`
  }

  deckId.value = String(dl.id)
  deckName.value = dl.name
  deckUrl.value = dl.url

  await runValidations()
}

const cardByCode = computed(() => {
  const m = new Map<string, ArkhamDBCard>()
  for (const c of cards.value) m.set(normalizeCode(c.cardCode), c)
  return m
})

async function runValidations() {
  valid.value = false
  errors.value = []
  requestFailed.value = false
  try {
    if (!deckList.value) return
    await validateDeck(deckList.value)
    valid.value = true
  } catch (err: unknown) {
    // The custom library is what names a custom card in the error list, and
    // this page never needed it before now.
    await loadLibrary()
    errors.value = validationErrorsFromResponse(err)
  }
}

async function createDeck() {
  errors.value = []
  requestFailed.value = false
  if (!valid.value || !deckList.value) return

  if (!saveDeck.value) {
    const dl = deckList.value
    deckId.value = null
    deckName.value = null
    deckUrl.value = null
    investigator.value = null
    deck.value = null
    emit('newDeckList', dl)
    return
  }

  if (!(deckId.value && deckName.value)) return

  try {
    const created = await newDeck(deckId.value, deckName.value, deckUrl.value, deckList.value)
    deckId.value = null
    deckName.value = null
    deckUrl.value = null
    investigator.value = null
    deck.value = null
    emit('newDeck', created)
  } catch (err: unknown) {
    await loadLibrary()
    errors.value = validationErrorsFromResponse(err)
  }
}
</script>

<template>
  <div class="new-deck">
    <div class="form-body">
      <img v-if="investigator && !noPortrait" class="portrait" :src="portraitImage(investigator)" />
      <div class="fields">
        <div class="sources" role="tablist">
          <button
            v-for="s in sources"
            :key="s"
            type="button"
            role="tab"
            class="source-tab"
            :class="{ active: source === s }"
            :aria-selected="source === s"
            @click.prevent="source = s"
          >
            <ArkhamBuildLogo v-if="s === 'build'" class="source-mark" />
            {{ t(sourceLabels[s]) }}
          </button>
        </div>

        <div v-if="source === 'build'" class="build-panel">
          <template v-if="!buildConnected">
            <p class="build-note">{{ t('newDeck.notConnected') }}</p>
            <router-link to="/settings" class="build-link">{{ t('newDeck.openSettings') }}</router-link>
          </template>
          <p v-else-if="buildLoading" class="build-note">{{ t('newDeck.loadingDecks') }}</p>
          <template v-else-if="buildError">
            <p class="build-note">{{ t('newDeck.decksFailed') }}</p>
            <button type="button" class="build-retry" @click.prevent="loadBuildDecks">
              {{ t('newDeck.retry') }}
            </button>
          </template>
          <template v-else>
            <p v-if="!buildArkhamdbAvailable" class="build-note">
              {{ t('newDeck.arkhamdbUnavailable') }}
            </p>
            <input
              v-if="buildDecks.length"
              v-model="buildSearch"
              type="search"
              :placeholder="t('newDeck.searchDecks')"
            />
            <p v-if="!buildDecks.length" class="build-note">{{ t('newDeck.noDecks') }}</p>
            <p v-else-if="!visibleBuildDecks.length" class="build-note">
              {{ t('newDeck.noMatchingDecks') }}
            </p>
            <ul v-else class="build-decks">
              <li v-for="d in visibleBuildDecks" :key="buildDeckKey(d)">
                <button
                  type="button"
                  class="build-deck"
                  :class="{ chosen: selectedBuildKey === buildDeckKey(d) }"
                  @click.prevent="chooseBuildDeck(d)"
                >
                  <img class="build-portrait" :src="portraitImage(resolvedInvestigatorCode(d.decklist))" />
                  <span class="build-deck-body">
                    <span class="build-deck-name">{{ d.decklist.name }}</span>
                    <span class="build-deck-meta">{{ d.decklist.investigator_name }}</span>
                  </span>
                  <span class="build-deck-source">{{ d.source === 'arkhamdb' ? 'ArkhamDB' : '' }}</span>
                </button>
              </li>
            </ul>
          </template>
        </div>

        <ArkhamDbDeck v-else-if="source === 'link'" v-model="deckList" />

        <input
          v-else
          type="file"
          accept=".json,application/json"
          @change="loadDeckFromFile"
        />
        <input v-if="investigator" v-model="deckName" />
        <div v-if="!alwaysSave" class="save-option" :class="{ active: saveDeck }" @click="saveDeckToggle = !saveDeckToggle" role="checkbox" :aria-checked="saveDeck">
          <div class="save-option-body">
            <span class="save-option-title">{{ $t('deckList.saveToDeckList') }}</span>
            <span class="save-option-desc">{{ $t('deckList.saveToDeckListDescription') }}</span>
          </div>
          <div class="save-option-toggle" :class="{ on: saveDeck }">
            <div class="save-option-thumb" />
          </div>
        </div>
        <button :disabled="!valid" @click.prevent="createDeck" class="primary-action">{{ alwaysSave ? t('newDeck.save') : saveDeck ? t('newDeck.saveAndUse') : t('newDeck.useWithoutSaving') }}</button>
      </div>
    </div>
    <div class="errors" v-if="investigatorError">
      {{investigatorError}}
    </div>
    <div class="errors" v-if="requestFailed">
      {{ t('newDeck.requestFailed') }}
    </div>
    <div class="errors" v-if="errors.length > 0">
      <p>{{ t('newDeck.unimplementedError') }}</p>
      <ul>
        <li class="error" v-for="(error, idx) in errors" :key="idx">
          {{error}}
        </li>
      </ul>
    </div>
  </div>
</template>

<style scoped>
.new-deck {
  :deep(input[type=url]) {
    margin-bottom: 0;
  }
  :deep(input) {
    outline: 0;
    border: 1px solid rgba(255,255,255,0.10);
    border-radius: 5px;
    padding: 12px 14px;
    color: #e0e0e0;
    background: var(--background-dark);
    width: 100%;
    font-size: 0.92em;
    transition: border-color 120ms ease;

    &:focus {
      border-color: rgba(110, 134, 64, 0.7);
    }
  }
  :deep(input[type=file]) {
    padding: 8px 12px;
    color: #888;
    cursor: pointer;

    &::file-selector-button {
      background: rgba(255,255,255,0.08);
      border: 1px solid rgba(255,255,255,0.15);
      border-radius: 4px;
      color: #ccc;
      padding: 5px 12px;
      font-size: 0.82em;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      cursor: pointer;
      margin-right: 10px;
      transition: background 150ms ease;

      &:hover {
        background: rgba(255,255,255,0.14);
      }
    }
  }
  .portrait {
    margin-right: 10px;
    height: 170px;
    border-radius: 5px;
  }
  .fields {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 10px;
  }
  .errors {
    background-color: rgba(100, 0, 0, 0.85);
    border: 1px solid rgba(255,80,80,0.2);
    border-radius: 6px;
    width: 100%;
    margin-top: 10px;
    padding: 14px 16px;
  }
  /* Save option toggle card */
  .save-option {
    display: flex;
    align-items: center;
    gap: 12px;
    padding: 12px 14px;
    border-radius: 6px;
    border: 1px solid rgba(255,255,255,0.10);
    background: rgba(255,255,255,0.04);
    cursor: pointer;
    user-select: none;
    transition: background 150ms ease, border-color 150ms ease;
    width: 100%;

    &:hover {
      background: rgba(255,255,255,0.08);
    }

    &.active {
      border-color: rgba(110, 134, 64, 0.6);
      background: rgba(110, 134, 64, 0.10);
    }
  }

  .save-option-body {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 2px;
  }

  .save-option-title {
    font-size: 0.88em;
    font-weight: 600;
    color: #ddd;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  .save-option-desc {
    font-size: 0.76em;
    color: #888;
  }

  .save-option-toggle {
    width: 38px;
    height: 22px;
    border-radius: 11px;
    background: rgba(255,255,255,0.15);
    border: 1px solid rgba(255,255,255,0.15);
    position: relative;
    flex-shrink: 0;
    transition: background 200ms ease, border-color 200ms ease;

    &.on {
      background: rgba(110, 134, 64, 0.9);
      border-color: rgba(110, 134, 64, 0.6);
    }
  }

  .save-option-thumb {
    position: absolute;
    top: 2px;
    left: 2px;
    width: 16px;
    height: 16px;
    border-radius: 50%;
    background: white;
    box-shadow: 0 1px 3px rgba(0,0,0,0.4);
    transition: transform 200ms ease;

    .save-option-toggle.on & {
      transform: translateX(16px);
    }
  }

  /* Primary action button */
  button.primary-action {
    outline: 0;
    width: 100%;
    height: 48px;
    border-radius: 5px;
    margin-top: 8px;
    border: 1px solid rgba(255,255,255,0.10);
    background: rgba(110, 134, 64, 0.95);
    color: white;
    letter-spacing: 0.08em;
    text-transform: uppercase;
    font-size: 0.88em;
    cursor: pointer;
    box-shadow: 0 5px 18px rgba(0,0,0,0.3);
    transition: transform 120ms ease, background 160ms ease, box-shadow 160ms ease;

    &:hover:not(:disabled) {
      transform: translateY(-1px);
      background: rgba(110, 134, 64, 1);
      box-shadow: 0 10px 28px rgba(0,0,0,0.4);
    }

    &:active:not(:disabled) {
      transform: translateY(0) scale(0.97);
    }

    &:disabled {
      opacity: 0.55;
      cursor: not-allowed;
      box-shadow: none;
      transform: none;
    }
  }
  display: flex;
  flex-direction: column;
  color: #FFF;
  border-radius: 3px;
  a {
    color: #365488;
    font-weight: bolder;
  }
}

.form-body {
  display: flex;
}

.sources {
  display: flex;
  gap: 4px;
}

.source-tab {
  align-items: center;
  background: rgba(255, 255, 255, 0.04);
  border: 1px solid rgba(255, 255, 255, 0.1);
  border-radius: 5px;
  color: #aaa;
  cursor: pointer;
  display: flex;
  gap: 6px;
  font-size: 0.82em;
  letter-spacing: 0.05em;
  padding: 7px 12px;
  text-transform: uppercase;
  transition: background 150ms ease, color 150ms ease, border-color 150ms ease;
}

.source-tab:hover {
  background: rgba(255, 255, 255, 0.08);
  color: #ddd;
}

.source-tab.active {
  background: rgba(110, 134, 64, 0.16);
  border-color: rgba(110, 134, 64, 0.6);
  color: #e6e6e6;
}

.source-mark {
  height: 15px;
  width: 15px;
}

.build-panel {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.build-note {
  color: #999;
  font-size: 0.86em;
  margin: 0;
}

.build-link {
  color: var(--spooky-green);
  font-size: 0.86em;
  width: fit-content;
}

.build-retry {
  width: fit-content;
}

.build-decks {
  display: flex;
  flex-direction: column;
  gap: 4px;
  list-style: none;
  margin: 0;
  /* Enough for a long collection to scroll rather than push the save button
     off the bottom of the panel. */
  max-height: 320px;
  overflow-y: auto;
  padding: 0;
}

.build-deck {
  align-items: center;
  background: rgba(255, 255, 255, 0.04);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 5px;
  cursor: pointer;
  display: flex;
  gap: 10px;
  padding: 6px 10px;
  text-align: left;
  width: 100%;
}

.build-deck:hover {
  background: rgba(255, 255, 255, 0.09);
}

.build-deck.chosen {
  background: rgba(110, 134, 64, 0.16);
  border-color: rgba(110, 134, 64, 0.6);
}

.build-portrait {
  border-radius: 3px;
  flex: none;
  height: 36px;
  object-fit: cover;
  width: 36px;
}

.build-deck-body {
  display: flex;
  flex-direction: column;
  flex: 1;
  gap: 1px;
  min-width: 0;
}

.build-deck-name {
  color: #e6e6e6;
  font-size: 0.94em;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.build-deck-meta {
  color: #999;
  font-size: 0.78em;
}

.build-deck-source {
  color: #888;
  flex: none;
  font-size: 0.7em;
  letter-spacing: 0.05em;
  text-transform: uppercase;
}
</style>
