<script lang="ts" setup>
import { ref, computed, inject } from 'vue';
import { upgradeDeck } from '@/arkham/api';
import { imgsrc } from '@/arkham/helpers';
import { Game } from '@/arkham/types/Game';
import Prompt from '@/components/Prompt.vue';

// TODO should we pass in the investigator
export interface Props {
  game: Game
  playerId: string
}

const props = defineProps<Props>()
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
  if (!deck.value) {
    return;
  }

  const matches = deck.value.match(/\/(deck(list)?)(\/view)?\/([^/]+)/);

  if (matches) {
    deckUrl.value = `https://arkhamdb.com/api/public/${matches[1]}/${matches[4]}`
    fetch(deckUrl.value)
      .then((response) => response.json(), () => {
        deckUrl.value = null;
      })
  } else {
    deckUrl.value = null;
  }
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
</script>

<template>
  <div id="upgrade-deck">
    <div>
      <h2>Upgrade Deck ({{xp}} xp)</h2>
      <div v-if="!waiting" class="upgrade-deck">
        <img v-if="investigatorId" class="portrait" :src="imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)" />
        <div class="fields">
          <p>To upgrade your deck paste a URL from ArkhamDB, this can either be a brand new deck or the existing url</p>
          <input
            type="url"
            v-model="deck"
            @change="loadDeck"
            @paste.prevent="pasteDeck($event)"
            placeholder="ArkhamDB deck url"
          />
          <div class="buttons">
            <button @click.prevent="upgrade">Upgrade</button>
            <button @click.prevent="skipping = true">Do not Upgrade</button>
          </div>
        </div>
      </div>
      <div v-else class="upgrade-deck">
        Waiting for other players to upgrade deck.
      </div>
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
  display: flex;
  flex-direction: column;
  align-items: center;
  min-width: 70vw;
  color: var(--title);
  font-size: 1.2em;
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
    box-sizing: border-box;
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
}

.portrait {
  width: 150px;
  border-radius: 10px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}
</style>
