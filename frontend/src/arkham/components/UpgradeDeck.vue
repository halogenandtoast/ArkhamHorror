<script lang="ts" setup>
import { ref, computed, inject } from 'vue';
import { upgradeDeck } from '@/arkham/api';
import { imgsrc, localizeArkhamDBBaseUrl } from '@/arkham/helpers';
import { Game } from '@/arkham/types/Game';
import { Investigator } from '@/arkham/types/Investigator';
import { baseKey } from '@/arkham/types/Log';
import Prompt from '@/components/Prompt.vue';
import XpBreakdown from '@/arkham/components/XpBreakdown.vue';
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
const solo = inject('solo', false)
const deckInvestigator = ref<string | null>(null)
const investigator = computed(() => {
  return Object.values(props.game.investigators).find((i) => {
    return i.playerId === props.playerId
  })
})
const investigatorId = computed(() => deckInvestigator.value ? `c${deckInvestigator.value}` : investigator.value?.id)
const originalInvestigatorId = computed(() => investigator.value?.id)
const xp = computed(() => investigator.value?.xp)
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
    return i.id === deckInvestigator.value && i.playerId !== props.playerId
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
      let content: { url: string; next_deck: string } | null = null;
      fetching.value = true;

      do {
        try {
          const response = await fetch(nextUrl);
          const data = await response.json();
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
        deck.value = content.url;
        deckUrl.value = content.url;
        upgrade();
      }
    }

    if(!nextUrl) return
    const arkhamBuildApiRegex = /https:\/\/api.arkham\.build\/v1\/public\/share\/([^/]+)/
    const abmatches = nextUrl.match(arkhamBuildApiRegex)
    if (abmatches) {
      let content: { url: string; next_deck: string } | null = null;
      fetching.value = true;

      do {
        try {
          const response = await fetch(nextUrl);
          const data = await response.json();
          content = { ...data, url: nextUrl };

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

  const arkhamDbRegex = /https:\/\/(?:[a-zA-Z0-9-]+\.)?arkhamdb\.com\/(deck(list)?)(\/view)?\/([^/]+)/
  const arkhamBuildRegex = /https:\/\/arkham\.build\/(?:deck\/view|share)\/([^/]+)/
  
  let matches
  if ((matches = deck.value.match(arkhamDbRegex))) {
    deckUrl.value = `${localizeArkhamDBBaseUrl()}/api/public/${matches[1]}/${matches[4]}`
  } else if ((matches = deck.value.match(arkhamBuildRegex))) {
    deckUrl.value = `https://api.arkham.build/v1/public/share/${matches[1]}`
  } else {
    return
  }

  fetch(deckUrl.value)
    .then((response) => response.json(), () => model.value = null)
    .then((data) => {
      model.value = {...data, url: deckUrl.value}
      deckInvestigator.value = data.investigator_code
    }, () => model.value = null)
}

function pasteDeck(evt: ClipboardEvent) {
  if (evt.clipboardData) {
    deck.value = evt.clipboardData.getData('text');
    loadDeck();
  }
}

async function upgrade() {
  if(error.value) return
  if (deckUrl.value && originalInvestigatorId.value) {
    upgradeDeck(props.game.id, originalInvestigatorId.value, deckUrl.value).then(() => {
      if(!solo) {
        waiting.value = true
      }
    });
    deckUrl.value = null;
    deck.value = null;
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

const breakdowns = computed(() => {
  if (props.game.campaign) {
    return props.game.campaign.xpBreakdown
  }

  return []
})

const tabooList = function (investigator: Investigator) {
  if (investigator.taboo) {
    switch (investigator.taboo) {
      case "TabooList15": return "1.5 (Apr 23, 2019)"
      case "TabooList16": return "1.6 (Sep 27, 2019)"
      case "TabooList18": return "1.8 (Oct 15, 2020)"
      case "TabooList19": return "1.9 (Jun 28, 2021)"
      case "TabooList20": return "2.0 (Aug 26, 2022)"
      case "TabooList21": return "2.1 (Aug 30, 2023)"
      case "TabooList22": return "2.2 (Feb 20, 2024)"
      case "TabooList23": return "2.3 (Oct 24, 2024)"
      default: return "Unknown Taboo List"
    }
  }

  return null
}
</script>

<template>
  <div id="upgrade-deck" class="column">
    <div class="column">
      <h2 class="title">{{ $t('upgrade.title', {xp: xp}) }}</h2>
      <div v-if="!waiting" class="upgrade-deck">
        <template v-if="question && investigator && question.tag !== 'ChooseUpgradeDeck'">
          <img v-if="investigatorId" class="portrait" :src="imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)" />
          <div v-if="question && playerId == investigator.playerId" class="question">
            <h2 v-if="questionLabel" class="title question-label">{{ questionLabel }}</h2>
            <Question :game="game" :playerId="playerId" @choose="choose" />
          </div>
          <div v-else>
            <div v-if="tabooList(investigator)" class="taboo-list">
              Taboo List: {{tabooList(investigator)}}
            </div>
          </div>
        </template>
        <template v-else>
          <template v-if="investigatorId && killedInvestigators.includes(investigatorId)">
            <img v-if="investigatorId && killedInvestigators.includes(investigatorId)" class="portrait killed" :src="imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)" />
            <div class="fields">
              <p class="killed-prompt"> {{ $t('upgrade.killed') }}</p>
              <p v-if="error" class="error">{{ error }}</p>
              <div class="single-field">
                <input
                  type="url"
                  v-model="deck"
                  @change="loadDeck"
                  @paste.prevent="pasteDeck($event)"
                  v-bind:placeholder="$t('upgrade.deckUrlPlaceholder')"
                />
                <button :class="{disable: error != null || deckInvestigator == null}" :disabled="error != null" @click.prevent="upgrade">{{ $t('upgrade.newInvestigator') }}</button>
              </div>
            </div>
          </template>
          <template v-else>
            <img v-if="investigatorId" class="portrait" :src="imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)" />
            <div class="fields">
              <p v-if="error" class="error">{{ error }}</p>
              <div class="arkhamdb-integration column">
                <template v-if="fetching">
                  <p>{{ $t('upgrade.fetching', {deckSource: deckSource}) }}</p>
                </template>
                <template v-else>
                  <template v-if="investigatorId == originalInvestigatorId && deckSource">
                    <p>{{ $t('upgrade.directlyUpdateContent', {deckSource: deckSource}) }}</p>
                    <div class="buttons">
                      <button @click.prevent="viewDeck">{{ $t('upgrade.openDeck', {deckSource: deckSource}) }}</button>
                      <button @click.prevent="syncUpgrade">{{ $t('upgrade.pullUpdate', {deckSource: deckSource}) }}</button>
                    </div>
                    <span class="separator">{{ $t('upgrade.OR') }}</span>
                  </template>
                  <div class="single-field">
                    <input
                      type="url"
                      v-model="deck"
                      @change="loadDeck"
                      @paste.prevent="pasteDeck($event)"
                      v-bind:placeholder="$t('upgrade.deckUrlPlaceholder')"
                    />
                    <button @click.prevent="upgrade">{{ originalInvestigatorId && killedInvestigators.includes(originalInvestigatorId) ? $t('upgrade.newInvestigator') : $t('upgrade.Upgrade') }}</button>
                  </div>
                  <div v-if="investigatorId == originalInvestigatorId" class="buttons">
                    <button class="skip" @click.prevent="skipping = true">{{ $t('upgrade.continueWithoutUpgrading') }}</button>
                  </div>
                </template>
              </div>
            </div>
          </template>
        </template>
      </div>
      <div v-else class="upgrade-deck">
        {{ $t('upgrade.waitingOtherPlayer') }}
      </div>
    </div>

    <div v-for="([step, entries], idx) in breakdowns" :key="idx" class="breakdowns">
      <XpBreakdown :game="game" :step="step" :entries="entries" :playerId="playerId" />
    </div>
  </div>


  <Prompt
    v-if="skipping"
    v-bind:prompt= "$t('upgrade.skippingPrompt')"
    :yes="skip"
    :no="() => skipping = false"
  />
</template>

<style scoped lang="scss">
#upgrade-deck {
  overflow: auto;
  display: flex;
  flex-direction: column;
  align-items: center;
  min-width: 70vw;
  color: var(--title);
  font-size: 1.2em;
  padding: 20px;
  > :deep(.column) {
    width: 100%;
  }
}

.upgrade-deck {
  border-radius: 5px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  padding: 10px;
  display: flex;
  flex-direction: row;
  align-items: flex-start;
  gap: 10px;
  min-width: 70vw;
  :deep(button){
    font-size: small;
    hyphens: auto;
    overflow-wrap: break-word;
    word-wrap: break-word;
    white-space: pre-wrap;
  }
  @media (max-width: 800px) and (orientation: portrait) {
      flex-direction: column;
      align-items: center;
  }
}

.breakdowns {
  min-width: 100%;
}

h2 {
  color: var(--title);
}

p {
  margin: 0;
  padding: 0;
  text-align: center;
}

input {
    outline: 0;
    border: 1px solid var(--background);
    padding: 15px;
    color: #F2F2F2;
    background: var(--background-dark);
    width: 100%;
    margin-bottom: 10px;
}

.buttons {
  display: flex;
  gap: 5px;
}

button {
  text-transform: uppercase;
  flex: 1;
  padding: 10px;
  border: 0;
  background-color: var(--button-1);
  &:hover {
    background-color: var(--button-1-highlight);
  }
}

.fields {
  display: flex;
  flex-direction: column;
  gap: 10px;
  flex: 1;
}

.portrait {
  width: 150px;
  border-radius: 10px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}

.question {
  flex: 1;
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

.single-field {
  display: grid;
  grid-template-rows: 2em; grid-template-columns: 1fr auto;
  padding-bottom: 10px;
  input {
    border: 0;
    height: 100%;
  }

  button {
    height: 100%;
    min-width: fit-content;
    width: fit-content;
  }
}

.skip {
  background-color: darkgoldenrod;
}

p.secondary {
  font-size: 0.7em;
}

.separator {
  display: flex;
  align-items: center;
  text-align: center;
}

.separator::before,
.separator::after {
  content: '';
  flex: 1;
  border-bottom: 2px solid rgba(0, 0, 0, 0.2);
}

.separator:not(:empty)::before {
  margin-right: .25em;
}

.separator:not(:empty)::after {
  margin-left: .25em;
}

.killed {
  filter: grayscale(1) brightness(0.5) sepia(1) hue-rotate(-90deg) saturate(10);
}

.disable {
  color: #666;
  background-color: #333;
  &:hover {
    cursor: not-allowed;
    color: #666;
    background-color: #333;
  }
}

.killed-prompt {
  padding: 10px;
  margin-block: 10px;
  background-color: #660000;
  border-radius: 10px;
}
</style>
