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

function loadDeck() {
  if (!deck.value) return
  model.value = null

  const arkhamDbRegex = /https:\/\/arkhamdb\.com\/(deck(list)?)(\/view)?\/([^/]+)/
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
    return [...props.game.campaign.xpBreakdown].reverse()
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
      default: return "Unknown Taboo List"
    }
  }

  return null
}
</script>

<template>
  <div id="upgrade-deck" class="column">
    <div class="column">
      <h2 class="title">Upgrade Deck ({{xp}} TOTAL xp)</h2>
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
            <p>To upgrade your deck paste a URL from ArkhamDB, this can either be a brand new deck or the existing url</p>
            <input
              type="url"
              v-model="deck"
              @change="loadDeck"
              @paste.prevent="pasteDeck($event)"
              placeholder="ArkhamDB or arkham.build deck url"
            />
            <div class="buttons">
              <button @click.prevent="upgrade">Upgrade</button>
              <button @click.prevent="skipping = true">Do not Upgrade</button>
            </div>
          </div>
        </template>
      </div>
      <div v-else class="upgrade-deck">
        Waiting for other players to upgrade deck.
      </div>
    </div>

    <div v-for="([step, entries], idx) in breakdowns" :key="idx" class="breakdowns">
      <XpBreakdown :game="game" :step="step" :entries="entries" :playerId="playerId" />
    </div>
  </div>


  <Prompt
    v-if="skipping"
    prompt="Are you sure you want to skip upgrading this deck?"
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
</style>
