<script lang="ts" setup>
import { ref, computed, inject } from 'vue';
import { upgradeDeck } from '@/arkham/api';
import { imgsrc } from '@/arkham/helpers';
import { Game } from '@/arkham/types/Game';
import Prompt from '@/components/Prompt.vue';
import XpBreakdown from '@/arkham/components/XpBreakdown.vue';
import Question from '@/arkham/components/Question.vue';

// TODO should we pass in the investigator
export interface Props {
  game: Game
  playerId: string
}

const question = computed(() => props.game.question[props.playerId])
const model = defineModel()
const fetching = ref(false)
const props = defineProps<Props>()
const emit = defineEmits<{ choose: [value: number] }>()
const choose = (idx: number) => emit('choose', idx)
const waiting = ref(false)
const deck = ref<string | null>(null)
const deckUrl = ref<string | null>(null)
const solo = inject('solo', false)
const investigator = computed(() => {
  return Object.values(props.game.investigators).find((i) => {
    return i.playerId === props.playerId
  })
})
const investigatorId = computed(() => investigator.value?.id)
const xp = computed(() => investigator.value?.xp)
const skipping = ref(false)

const currentDeckUrl = computed(() => {
  if (!investigator.value) { return null }
  return investigator.value.deckUrl
})

const isArkhamDBDeck = computed(() => {
  if (!currentDeckUrl.value) { return false }
  return currentDeckUrl.value.startsWith('https://arkhamdb.com')
})

const deckSource = computed(() => {
  return isArkhamDBDeck.value ? 'ArkhamDB' : 'arkham.build'
})

function viewDeck() {
  if (currentDeckUrl.value) {
    const arkhamDbApiRegex = /https:\/\/(?:[a-zA-Z0-9-]+\.)?arkhamdb\.com\/api\/public\/deck\/([^/]+)/
    const matches = currentDeckUrl.value.match(arkhamDbApiRegex)
    if (matches) {
      window.open(`https://arkhamdb.com/deck/view/${matches[1]}`)
    }

    console.log(currentDeckUrl.value)
    const arkhamBuildApiRegex = /https:\/\/api.arkham\.build\/v1\/public\/share\/([^/]+)/
    const abmatches = currentDeckUrl.value.match(arkhamBuildApiRegex)
    if (abmatches) {
      window.open(`https://arkham.build/deck/view/${abmatches[1]}?upgrade_xp=${xp.value}`)
    }
  }
}

async function syncUpgrade() {
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
            nextUrl = `https://arkhamdb.com/api/public/deck/${data.next_deck}`;
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
    deckUrl.value = `https://arkhamdb.com/api/public/${matches[1]}/${matches[4]}`
  } else if ((matches = deck.value.match(arkhamBuildRegex))) {
    deckUrl.value = `https://api.arkham.build/v1/public/share/${matches[1]}`
  } else {
    return
  }

  fetch(deckUrl.value)
    .then((response) => response.json(), () => model.value = null)
    .then((data) => model.value = {...data, url: deckUrl.value}, () => model.value = null)
}

function pasteDeck(evt: ClipboardEvent) {
  if (evt.clipboardData) {
    deck.value = evt.clipboardData.getData('text');
    loadDeck();
  }
}

async function upgrade() {
  if (deckUrl.value && investigatorId.value) {
    upgradeDeck(props.game.id, investigatorId.value, deckUrl.value).then(() => {
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
        <template v-if="question && question.tag !== 'ChooseUpgradeDeck'">
          <img v-if="investigatorId" class="portrait" :src="imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)" />
          <div v-if="question && playerId == investigator.playerId" class="question">
            <Question :game="game" :playerId="playerId" @choose="choose" />
          </div>
          <div v-else>
            <div v-if="tabooList(investigator)" class="taboo-list">
              Taboo List: {{tabooList(investigator)}}
            </div>
          </div>
        </template>
        <template v-else>
          <img v-if="investigatorId" class="portrait" :src="imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)" />
          <div class="fields">
            <div class="arkhamdb-integration column">
              <template v-if="fetching">
                <p>{{ $t('upgrade.fetching', {deckSource: deckSource}) }}</p>
              </template>
              <template v-else>
                <p>{{ $t('upgrade.directlyUpdateContent', {deckSource: deckSource}) }}</p>
                <div class="buttons">
                  <button @click.prevent="viewDeck">{{ $t('upgrade.openDeck', {deckSource: deckSource}) }}</button>
                  <button @click.prevent="syncUpgrade">{{ $t('upgrade.pullUpdate', {deckSource: deckSource}) }}</button>
                </div>
                <span class="separator">{{ $t('upgrade.OR') }}</span>
                <div class="single-field">
                  <input
                    type="url"
                    v-model="deck"
                    @change="loadDeck"
                    @paste.prevent="pasteDeck($event)"
                    v-bind:placeholder="$t('upgrade.deckUrlPlaceholder')"
                  />
                  <button @click.prevent="upgrade">{{ $t('upgrade.Upgrade') }}</button>
                </div>
                <div class="buttons">
                  <button class="skip" @click.prevent="skipping = true">{{ $t('upgrade.continueWithoutUpgrading') }}</button>
                </div>
              </template>
            </div>
          </div>
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
</style>
