<script lang="ts" setup>
import { displayTabooList } from '@/arkham/taboo';
import { ref, computed, inject } from 'vue';
import { upgradeDeck } from '@/arkham/api';
import { imgsrc, localizeArkhamDBBaseUrl, processArkhamBuildDeck } from '@/arkham/helpers';
import { ArkhamDbDecklist } from '@/arkham/types/Deck';
import { Game } from '@/arkham/types/Game';
import { Investigator } from '@/arkham/types/Investigator';
import { baseKey } from '@/arkham/types/Log';
import Prompt from '@/components/Prompt.vue';
import XpBreakdown from '@/arkham/components/XpBreakdown.vue';
import type { XpBreakdownStep } from '@/arkham/types/Xp';
import Question from '@/arkham/components/Question.vue';

// TODO should we pass in the investigator
export interface Props {
  game: Game
  playerId: string
}

const question = computed(() => props.game.question[props.playerId])
const questionLabel = computed(() => {
  if (question.value)
    return question.value.tag === 'QuestionLabel' ? question.value.label : null
})
const model = defineModel()
const fetching = ref(false)
const props = defineProps<Props>()
const emit = defineEmits<{ choose: [value: number] }>()
const choose = (idx: number) => emit('choose', idx)
const waiting = ref(false)
const deck = ref<string | null>(null)
const deckUrl = ref<string | null>(null)
const deckList = ref<ArkhamDbDecklist | null>(null)
const solo = inject('solo', false)
const deckInvestigator = ref<string | null>(null)
const investigator = computed(() => {
  return Object.values(props.game.investigators).find((i) => {
    return i.playerId === props.playerId
  })
})
const investigatorId = computed(() => !solo && deckInvestigator.value ? `c${deckInvestigator.value}` : investigator.value?.id)
const investigators = computed(() => Object.values(props.game.investigators))
const originalInvestigatorId = computed(() => investigator.value?.id)
const xp = computed(() => {
  const inv = investigator.value
  if (!inv) return undefined
  return inv.xp - inv.spentXp
})
const skipping = ref(false)

const killedInvestigators = computed(() => {
  const {campaign} = props.game
  if (!campaign) { return [] }
  const {recordedSets} = campaign.log
  const toInvestigators = (k: string) => {
    return (recordedSets[baseKey(k)] ?? []).map((r: {contents: string}) => r.contents)
  }
  return [...toInvestigators('KilledInvestigators'), ...toInvestigators('DrivenInsaneInvestigators')]
})

const error = computed(() => {
  if(!deckInvestigator.value) return null

  const alreadyTaken = Object.values(props.game.investigators).some((i) => {
    return i.id === `c${deckInvestigator.value}` && i.playerId !== props.playerId
  })

  if (alreadyTaken) {
    return 'This investigator is already taken'
  }

  const killedOrInsane = killedInvestigators.value.includes(`c${deckInvestigator.value}`)

  if (killedOrInsane) {
    return 'This investigator was killed or driven insane'
  }

  return null
})

const currentDeckUrl = computed(() => {
  if (!investigator.value) { return null }
  return investigator.value.deckUrl
})

const isArkhamDBDeck = computed(() => {
  if (!currentDeckUrl.value) { return false }
  return currentDeckUrl.value.startsWith('https://arkhamdb.com') || currentDeckUrl.value.startsWith(localizeArkhamDBBaseUrl())
})

const isArkhamBuildDeck = computed(() => {
  if (!currentDeckUrl.value) { return false }
  return currentDeckUrl.value.startsWith('https://api.arkham.build')
})

const deckSource = computed(() => {
  return isArkhamDBDeck.value ? 'ArkhamDB' : (isArkhamBuildDeck.value ? 'arkham.build' : null)
})

function viewDeck() {
  if (currentDeckUrl.value) {
    const arkhamDbApiRegex = /https:\/\/(?:[a-zA-Z0-9-]+\.)?arkhamdb\.com\/api\/public\/deck\/([^/]+)/
    const matches = currentDeckUrl.value.match(arkhamDbApiRegex)
    if (matches) {
      window.open(`${localizeArkhamDBBaseUrl()}/deck/view/${matches[1]}`)
      return
    }

    const arkhamDbDecklistRegex = /https:\/\/(?:[a-zA-Z0-9-]+\.)?arkhamdb\.com\/api\/public\/decklist\/([^/]+)/
    const dlmatches = currentDeckUrl.value.match(arkhamDbDecklistRegex)
    if (dlmatches) {
      window.open(`${localizeArkhamDBBaseUrl()}/decklist/view/${dlmatches[1]}`)
      return
    }

    const arkhamBuildApiRegex = /https:\/\/api.arkham\.build\/v1\/public\/share\/([^/]+)/
    const abmatches = currentDeckUrl.value.match(arkhamBuildApiRegex)
    if (abmatches) {
      window.open(`https://arkham.build/deck/view/${abmatches[1]}?upgrade_xp=${xp.value}`)
      return
    }
  }
}

async function syncUpgrade() {
  if(error.value) return
  if (!investigator.value?.deckUrl) return;
  let nextUrl: string | null = investigator.value.deckUrl;
  if (nextUrl) {
    const arkhamDbApiRegex = /https:\/\/(?:[a-zA-Z0-9-]+\.)?arkhamdb\.com\/api\/public\/deck\/([^/]+)/;
    const matches = nextUrl.match(arkhamDbApiRegex);

    if (matches) {
      let content: ArkhamDbDecklist | null = null;
      fetching.value = true;

      do {
        try {
          const response = await fetch(nextUrl);
          const data = (await response.json()) as ArkhamDbDecklist & { next_deck: string | number | null };
          content = { ...data, url: nextUrl };

          if (data.next_deck != null) {
            nextUrl = `${localizeArkhamDBBaseUrl()}/api/public/deck/${data.next_deck}`;
          } else {
            nextUrl = null;
          }
        } catch (error) {
          nextUrl = null;
        }
      } while (nextUrl);

      if (content && content.url) {
        model.value = content;
        deckList.value = content;
        deck.value = content.url;
        deckUrl.value = content.url;
        upgrade();
      }
    }

    if(!nextUrl) return
    const arkhamBuildApiRegex = /https:\/\/api.arkham\.build\/v1\/public\/share\/([^/]+)/
    const abmatches = nextUrl.match(arkhamBuildApiRegex)
    if (abmatches) {
      let content: ArkhamDbDecklist | null = null;
      fetching.value = true;

      do {
        try {
          const response = await fetch(nextUrl);
          const data = (await response.json()) as ArkhamDbDecklist & { next_deck: string | number | null };
          content = processArkhamBuildDeck(data, nextUrl);

          if (data.next_deck != null) {
            nextUrl = `https://api.arkham.build/v1/public/share/${data.next_deck}`;
          } else {
            nextUrl = null;
          }
        } catch (error) {
          nextUrl = null;
        }
      } while (nextUrl);

      if (content && content.url) {
        model.value = content;
        deckList.value = content;
        deck.value = content.url;
        deckUrl.value = content.url;
        upgrade();
      }
    }
  }
}

function loadDeck() {
  if (!deck.value) return
  model.value = null
  deckList.value = null

  const arkhamDbRegex = /https:\/\/(?:[a-zA-Z0-9-]+\.)?arkhamdb\.com\/(deck(list)?)(\/view)?\/([^/]+)/
  const arkhamBuildRegex = /https:\/\/arkham\.build\/(?:deck\/view|share)\/([^/]+)/

  let matches
  let isArkhamBuild = false
  if ((matches = deck.value.match(arkhamDbRegex))) {
    deckUrl.value = `${localizeArkhamDBBaseUrl()}/api/public/${matches[1]}/${matches[4]}`
  } else if ((matches = deck.value.match(arkhamBuildRegex))) {
    deckUrl.value = `https://api.arkham.build/v1/public/share/${matches[1]}`
    isArkhamBuild = true
  } else {
    return
  }

  const url = deckUrl.value
  fetch(url)
    .then((response) => response.json() as Promise<ArkhamDbDecklist>, () => { model.value = null; deckList.value = null })
    .then((data) => {
      if (!data) return
      const processed: ArkhamDbDecklist = isArkhamBuild
        ? processArkhamBuildDeck(data, url)
        : { ...data, url }
      model.value = processed
      deckList.value = processed
      deckInvestigator.value = processed.investigator_code
    }, () => { model.value = null; deckList.value = null })
}

function pasteDeck(evt: ClipboardEvent) {
  if (evt.clipboardData) {
    deck.value = evt.clipboardData.getData('text');
    loadDeck();
  }
}

function loadDeckFromFile(e: Event) {
  const files = (e.target as HTMLInputElement).files || (e as DragEvent).dataTransfer?.files || [];
  const file = files[0]
  if (!file) return
  const reader = new FileReader()
  reader.onloadend = (e1: ProgressEvent<FileReader>) => {
    if (!e1?.target?.result) return
    try {
      const data = JSON.parse(e1.target.result.toString()) as ArkhamDbDecklist
      model.value = data
      deckList.value = data
      deckUrl.value = data.url ?? null
      deck.value = data.url ?? null
      deckInvestigator.value = data.investigator_code
    } catch {
      // ignore invalid json
    }
  }
  reader.readAsText(file)
  ;(e.target as HTMLInputElement).value = ''
}

async function upgrade() {
  if(error.value) return
  if ((deckUrl.value || deckList.value) && originalInvestigatorId.value) {
   fetching.value = true
   upgradeDeck(props.game.id, originalInvestigatorId.value, deckUrl.value ?? undefined, deckList.value).then(() => {
      if(!solo) {
        waiting.value = true
      }
    }).finally(() => {
      fetching.value = false;
      waiting.value = false;
    });
    deckUrl.value = null;
    deck.value = null;
    deckList.value = null;
  }
}

async function skip() {
  if (!investigatorId.value) { return }
  upgradeDeck(props.game.id, investigatorId.value).then(() => {
    if(!solo) {
      waiting.value = true
    }
    skipping.value = false
  });
}

const allGameInvestigators = computed(() => ({
  ...props.game.investigators,
  ...props.game.killedInvestigators,
}))

function breakdownInvestigators(breakdown: XpBreakdownStep): Investigator[] {
  return breakdown.investigators
    .map(iid => allGameInvestigators.value[iid])
    .filter(Boolean) as Investigator[]
}

const breakdowns = computed<XpBreakdownStep[]>(() => {
  if (props.game.campaign) {
    return props.game.campaign.xpBreakdown
  }

  return []
})

const tabooList = function (investigator: Investigator) {
  return investigator.taboo ? displayTabooList(investigator.taboo) : null
}
</script>

<template>
  <div id="upgrade-deck">
    <h2 class="title">{{ $t('upgrade.title', {xp: xp}) }}</h2>

    <div v-if="!waiting" class="panel">
      <template v-if="question && investigator && question.tag !== 'ChooseUpgradeDeck'">
        <img v-if="investigatorId" class="portrait" :src="imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)" />
        <div v-if="question && playerId == investigator.playerId" class="content question-pane">
          <h3 v-if="questionLabel" class="question-label">{{ questionLabel }}</h3>
          <Question :game="game" :playerId="playerId" @choose="choose" />
        </div>
        <div v-else class="content">
          <div v-if="tabooList(investigator)" class="taboo-list">
            Taboo List: {{tabooList(investigator)}}
          </div>
        </div>
      </template>
      <template v-else>
        <template v-if="investigatorId && killedInvestigators.includes(investigatorId)">
          <img class="portrait killed" :src="imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)" />
          <div class="content">
            <p class="killed-prompt">{{ $t('upgrade.killed') }}</p>
            <p v-if="error" class="error">{{ error }}</p>
            <div class="input-row">
              <input
                type="url"
                v-model="deck"
                @change="loadDeck"
                @paste.prevent="pasteDeck($event)"
                v-bind:placeholder="$t('upgrade.deckUrlPlaceholder')"
              />
              <button class="primary" :class="{disable: error != null || deckInvestigator == null}" :disabled="error != null" @click.prevent="upgrade">{{ $t('upgrade.newInvestigator') }}</button>
            </div>
            <label class="file-upload">
              <span class="file-upload-text">{{ $t('upgrade.orUploadJson') }}</span>
              <input type="file" accept=".json,application/json" @change="loadDeckFromFile" />
            </label>
          </div>
        </template>
        <template v-else>
          <img v-if="investigatorId" class="portrait" :src="imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)" />
          <div class="content">
            <p v-if="error" class="error">{{ error }}</p>
            <template v-if="fetching">
              <p class="info">{{ $t('upgrade.fetching', {deckSource: deckSource}) }}</p>
            </template>
            <template v-else-if="question">
              <template v-if="investigatorId == originalInvestigatorId && deckSource">
                <p class="info">{{ $t('upgrade.directlyUpdateContent', {deckSource: deckSource}) }}</p>
                <div class="step-buttons">
                  <button class="step secondary" @click.prevent="viewDeck">
                    <span class="step-number">1</span>
                    <span class="step-label">{{ $t('upgrade.openDeck', {deckSource: deckSource}) }}</span>
                  </button>
                  <span class="step-arrow" aria-hidden="true">→</span>
                  <button class="step primary" @click.prevent="syncUpgrade">
                    <span class="step-number">2</span>
                    <span class="step-label">{{ $t('upgrade.pullUpdate', {deckSource: deckSource}) }}</span>
                  </button>
                </div>
                <span class="separator">{{ $t('upgrade.OR') }}</span>
              </template>
              <div class="input-row">
                <input
                  type="url"
                  v-model="deck"
                  @change="loadDeck"
                  @paste.prevent="pasteDeck($event)"
                  v-bind:placeholder="$t('upgrade.deckUrlPlaceholder')"
                />
                <button class="primary" @click.prevent="upgrade">{{ originalInvestigatorId && killedInvestigators.includes(originalInvestigatorId) ? $t('upgrade.newInvestigator') : $t('upgrade.Upgrade') }}</button>
              </div>
              <label class="file-upload">
                <span class="file-upload-text">{{ $t('upgrade.orUploadJson') }}</span>
                <input type="file" accept=".json,application/json" @change="loadDeckFromFile" />
              </label>
              <div v-if="investigatorId == originalInvestigatorId" class="footer">
                <button class="skip" @click.prevent="skipping = true">{{ $t('upgrade.continueWithoutUpgrading') }}</button>
              </div>
            </template>
            <p v-else class="info">{{ $t('upgrade.waitingOtherPlayer') }}</p>
          </div>
        </template>
      </template>
    </div>
    <div v-else class="panel waiting">
      {{ $t('upgrade.waitingOtherPlayer') }}
    </div>

    <div v-for="(breakdown, idx) in breakdowns" :key="idx" class="breakdowns">
      <XpBreakdown :game="game" :step="breakdown.step" :entries="breakdown.entries" :playerId="playerId" :showAll="false" :investigators="breakdownInvestigators(breakdown)" />
    </div>
  </div>


  <Prompt
    v-if="skipping"
    v-bind:prompt= "$t('upgrade.skippingPrompt')"
    :yes="skip"
    :no="() => skipping = false"
  />
</template>

<style scoped>
#upgrade-deck {
  overflow: auto;
  display: flex;
  flex-direction: column;
  align-items: center;
  width: 100%;
  color: var(--title);
  font-size: 1em;
  padding: 32px 24px 48px;
  gap: 24px;
}

h2 {
  color: var(--title);
}

.title {
  width: min(1100px, 92vw);
  text-align: left;
}

.panel {
  border-radius: 12px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  padding: 20px 24px;
  display: flex;
  flex-direction: row;
  align-items: flex-start;
  gap: 24px;
  width: min(1100px, 92vw);
  box-shadow: 0 16px 40px rgba(0, 0, 0, 0.32), inset 0 1px 0 rgba(255, 255, 255, 0.04);
}

.panel.waiting {
  justify-content: center;
  text-align: center;
  padding: 32px 24px;
  font-style: italic;
  color: #ccc;
}

@media (max-width: 800px) and (orientation: portrait) {
  .panel {
    flex-direction: column;
    align-items: center;
    padding: 18px;
    gap: 18px;
  }
}

.portrait {
  width: 180px;
  border-radius: 10px;
  box-shadow: 0 8px 22px rgba(0, 0, 0, 0.45);
  flex-shrink: 0;
}

.killed {
  filter: grayscale(1) brightness(0.5) sepia(1) hue-rotate(-90deg) saturate(10);
}

.content {
  flex: 1;
  display: flex;
  flex-direction: column;
  gap: 18px;
  min-width: 0;
}

.content p {
  margin: 0;
  padding: 0;
  text-align: center;
}

.info {
  color: #d8d8d8;
  font-size: 0.95em;
  line-height: 1.55;
  text-align: left;
}

.question-label {
  margin: 0 0 4px;
  font-size: 1.05em;
  font-weight: 600;
  color: var(--title);
}

.question-pane {
  :deep(button) {
    margin-left: 0px;
  }
  :deep(.amount-contents) {
    form {
      display: flex;
      gap: 10px;
      flex-direction: column;
      align-items: flex-start;
    }
    border-radius: 15px;
  }
}

input[type=url] {
  outline: 0;
  border: 1px solid rgba(255, 255, 255, 0.12);
  border-radius: 6px;
  padding: 10px 14px;
  color: #f2f2f2;
  background: var(--background-dark);
  width: 100%;
  font-size: 0.95em;
  margin: 0;
  transition: border-color 150ms ease, box-shadow 150ms ease;

  &::placeholder {
    color: #777;
  }

  &:focus {
    border-color: rgba(110, 134, 64, 0.7);
    box-shadow: 0 0 0 3px rgba(110, 134, 64, 0.18);
  }
}

.input-row {
  display: flex;
  align-items: stretch;

  input {
    flex: 1;
    min-width: 0;
    border-top-right-radius: 0;
    border-bottom-right-radius: 0;
    border-right: 0;

    &:focus {
      box-shadow: none;
    }
  }

  button {
    flex: 0 0 auto;
    width: auto;
    padding: 0 16px;
    border-top-left-radius: 0;
    border-bottom-left-radius: 0;
  }
}

@media (max-width: 600px) {
  .input-row {
    flex-direction: column;
    gap: 8px;
  }
  .input-row input {
    border-radius: 6px;
    border-right: 1px solid rgba(255, 255, 255, 0.12);
  }
  .input-row button {
    width: 100%;
    border-radius: 6px;
  }
}

.buttons {
  display: flex;
  gap: 10px;
}

.step-buttons {
  display: flex;
  align-items: stretch;
  gap: 8px;
}

.step {
  display: inline-flex;
  align-items: stretch;
  justify-content: flex-start;
  gap: 0;
  padding: 0;
  overflow: hidden;
  flex: 1;
  text-align: left;
}

.step-number {
  flex-shrink: 0;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-width: 32px;
  padding: 0 10px;
  background: rgba(0, 0, 0, 0.3);
  border-right: 1px solid rgba(255, 255, 255, 0.12);
  font-size: 0.95em;
  font-weight: 700;
  letter-spacing: 0;
  line-height: 1;
}

.step.primary .step-number {
  background: rgba(0, 0, 0, 0.25);
  border-right-color: rgba(255, 255, 255, 0.18);
}

.step-label {
  flex: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 0 14px;
  white-space: normal;
  line-height: 1.25;
}

.step-arrow {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  color: #888;
  font-size: 1.05em;
  flex-shrink: 0;
  user-select: none;
}

button {
  text-transform: uppercase;
  font-size: 0.78em;
  letter-spacing: 0.06em;
  font-weight: 600;
  padding: 0 18px;
  min-height: 38px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 6px;
  background-color: var(--button-1);
  color: #f4f4f4;
  cursor: pointer;
  transition: background 160ms ease, transform 120ms ease, box-shadow 160ms ease;
  flex: 1;

  &:hover:not(.disable):not(:disabled) {
    background-color: var(--button-1-highlight);
    transform: translateY(-1px);
    box-shadow: 0 6px 14px rgba(0, 0, 0, 0.3);
  }

  &:active:not(.disable):not(:disabled) {
    transform: translateY(0);
    box-shadow: none;
  }
}

button.secondary {
  background: rgba(255, 255, 255, 0.05);
  border-color: rgba(255, 255, 255, 0.12);
  color: #ddd;

  &:hover:not(:disabled) {
    background: rgba(255, 255, 255, 0.1);
  }
}

button.skip {
  background-color: darkgoldenrod;

  &:hover:not(:disabled) {
    background-color: #c8810a;
  }
}

.disable {
  opacity: 0.4;
  cursor: not-allowed;
  &:hover {
    transform: none;
    box-shadow: none;
  }
}

.separator {
  display: flex;
  align-items: center;
  text-align: center;
  font-size: 0.72em;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: #888;
  margin: 2px 0;
}

.separator::before,
.separator::after {
  content: '';
  flex: 1;
  border-bottom: 1px solid rgba(255, 255, 255, 0.08);
}

.separator:not(:empty)::before {
  margin-right: 0.85em;
}

.separator:not(:empty)::after {
  margin-left: 0.85em;
}

.file-upload {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 10px 14px;
  border-radius: 6px;
  border: 1px dashed rgba(255, 255, 255, 0.12);
  background: rgba(255, 255, 255, 0.02);
  color: #888;
  font-size: 0.72em;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  cursor: pointer;
  transition: border-color 150ms ease, background 150ms ease, color 150ms ease;

  &:hover {
    border-color: rgba(255, 255, 255, 0.22);
    background: rgba(255, 255, 255, 0.04);
    color: #aaa;
  }
}

.file-upload-text {
  flex-shrink: 0;
}

.file-upload input[type=file] {
  flex: 1;
  padding: 0;
  margin: 0;
  border: 0;
  background: transparent;
  color: #888;
  font-size: 1em;
  width: auto;

  &::file-selector-button {
    background: rgba(255, 255, 255, 0.06);
    border: 1px solid rgba(255, 255, 255, 0.12);
    border-radius: 4px;
    color: #ccc;
    padding: 4px 10px;
    font-size: 1em;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    cursor: pointer;
    margin-right: 10px;
    transition: background 150ms ease;

    &:hover {
      background: rgba(255, 255, 255, 0.14);
    }
  }
}

.footer {
  display: flex;
  margin-top: 8px;
  padding-top: 18px;
  border-top: 1px solid rgba(255, 255, 255, 0.06);
}

.killed-prompt {
  padding: 12px 16px;
  background-color: rgba(160, 0, 0, 0.18);
  border: 1px solid rgba(220, 60, 60, 0.25);
  border-radius: 8px;
  color: #f0c0c0;
  font-size: 0.95em;
  line-height: 1.5;
}

.error {
  padding: 10px 14px;
  background: rgba(160, 0, 0, 0.2);
  border: 1px solid rgba(220, 60, 60, 0.3);
  border-radius: 6px;
  color: #f0c0c0;
  font-size: 0.88em;
}

.taboo-list {
  font-size: 0.9em;
  color: #aaa;
}

.breakdowns {
  width: min(1100px, 92vw);
}
</style>
