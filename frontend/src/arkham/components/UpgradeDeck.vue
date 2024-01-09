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
          <input
            type="url"
            v-model="deck"
            @change="loadDeck"
            @paste.prevent="pasteDeck($event)"
            placeholder="ArkhamDB deck url"
          />
          <button @click.prevent="upgrade">Upgrade</button>
          <button @click.prevent="skipping = true">Do not Upgrade</button>
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
