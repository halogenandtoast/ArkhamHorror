<template>
  <div id="upgrade-deck">
    <div>
      <h2>Upgrade Deck ({{xp}} xp)</h2>
      <div class="upgrade-deck">
        <img class="portrait" :src="`/img/arkham/portraits/${investigatorId}.jpg`" />
        <div class="fields">
          <input
            type="url"
            v-model="deck"
            @change="loadDeck"
            @paste.prevent="pasteDeck($event)"
            placeholder="ArkhamDB deck url"
          />
          <button @click.prevent="upgrade">Upgrade</button>
        </div>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, ref, computed } from 'vue';
import { upgradeDeck } from '@/arkham/api';

export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true }
  },
  setup(props) {
    const waiting = ref(false)
    const deck = ref<string | null>(null)
    const deckUrl = ref<string | null>(null)
    const investigator = computed(() => props.game.currentData.investigators[props.investigatorId])

    const xp = computed(() => investigator.value.contents.xp)

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
      if (deckUrl.value) {
        upgradeDeck(props.game.id, deckUrl.value).then(() => waiting.value = true);
        deckUrl.value = null;
        deck.value = null;
      }
    }

    return { pasteDeck, upgrade, deck, loadDeck, investigator, xp }
  }
})
</script>
