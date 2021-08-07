<template>
  <div id="decks">
    <div>
      <h2>New Deck</h2>
      <div class="new-deck">
        <img v-if="investigator" class="portrait" :src="`${baseUrl}/img/arkham/portraits/${investigator.replace('c', '')}.jpg`" />
        <div class="fields">
          <input
            type="url"
            v-model="deck"
            @change="loadDeck"
            @paste.prevent="pasteDeck($event)"
            placeholder="ArkhamDB deck url"
          />
          <input v-if="investigator" v-model="deckName" />
          <button :disabled="!investigator" @click.prevent="createDeck">Create</button>
        </div>
      </div>
    </div>
    <h2>Existing Decks</h2>
    <div v-for="deck in decks" :key="deck.id" class="deck">
      <span>{{deck.name}}</span>
      <div class="deck-delete">
        <a href="#delete" @click.prevent="deleteId = deck.id"><font-awesome-icon icon="trash" /></a>
      </div>
    </div>

    <Prompt
      v-if="deleteId"
      prompt="Are you sure you want to delete this deck?"
      :yes="deleteDeckEvent"
      :no="() => deleteId = null"
    />
  </div>
</template>

<script lang="ts">
import { defineComponent, ref } from 'vue';
import * as Arkham from '@/arkham/types/Deck';
import Prompt from '@/components/Prompt.vue';
import { fetchDecks, newDeck, deleteDeck } from '@/arkham/api';

export default defineComponent({
  components: { Prompt },
  setup() {
    const ready = ref(false)
    const decks = ref<Arkham.Deck[]>([])
    const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';

    const deck = ref<string | null>(null)
    const investigator = ref<string | null>(null)
    const deckId = ref<string | null>(null)
    const deckName = ref<string | null>(null)
    const deckUrl = ref<string | null>(null)
    const deleteId = ref<string | null>(null)

    async function deleteDeckEvent() {
      const { value } = deleteId
      if (value) {
        deleteDeck(value).then(() => {
          decks.value = decks.value.filter((deck) => deck.id !== value);
          deleteId.value = null;
        });
      }
    }

    fetchDecks().then((response) => {
      decks.value = response
      ready.value = true
    })

    function loadDeck() {
      if (!deck.value) {
        return;
      }

      const matches = deck.value.match(/\/(deck(list)?)(\/view)?\/([^/]+)/);
      if (matches) {
        deckUrl.value = `https://arkhamdb.com/api/public/${matches[1]}/${matches[4]}`
        fetch(deckUrl.value)
          .then((response) => response.json(), () => {
            investigator.value = null;
            deckId.value = null
            deckName.value = null
            deckUrl.value = null;
          })
          .then((data) => {
            if(data.meta && data.meta.alternate_front) {
              investigator.value = data.meta.alternate_front
            } else {
              investigator.value = data.investigator_code
            }
            deckId.value = matches[4]
            deckName.value = data.name
          })
      } else {
        investigator.value = null;
        deckId.value = null
        deckName.value = null
        deckUrl.value = null;
      }
    }

    function pasteDeck(evt: ClipboardEvent) {
      if (evt.clipboardData) {
        deck.value = evt.clipboardData.getData('text');
        loadDeck();
      }
    }

    async function createDeck() {
      if (deckId.value && deckName.value && deckUrl.value) {
        newDeck(deckId.value, deckName.value, deckUrl.value).then((deck) => decks.value.push(deck));
        deckId.value = null;
        deckName.value = null;
        deckUrl.value = null;
        investigator.value = null;
        deck.value = null;
      }
    }

    return { baseUrl, pasteDeck, createDeck, deleteDeckEvent, deleteId, deck, decks, loadDeck, investigator, deckName }
  }
})
</script>

<style lang="scss" scoped>
.new-deck {
  input {
    outline: 0;
    border: 1px solid #000;
    padding: 15px;
    background: #F2F2F2;
    width: 100%;
    box-sizing: border-box;
    margin-bottom: 10px;
  }
  .portrait {
    margin-right: 10px;
    height: 170px;
  }
  .fields {
    flex: 1;
    display: flex;
    flex-direction: column;
    flex-flow: wrap;
  }
  button {
    outline: 0;
    padding: 15px;
    background: #6E8640;
    text-transform: uppercase;
    color: white;
    border: 0;
    width: 100%;
    &:hover {
      background: darken(#6E8640, 7%);
    }
  }
  button[disabled] {
    background: #999;
    cursor: not-allowed;
    &:hover {
      background: #999;
    }
  }
  display: flex;
  color: #FFF;
  background-color: #15192C;
  margin: 10px;
  padding: 10px;
  border-radius: 3px;
  a {
    color: #365488;
    font-weight: bolder;
  }
}

.deck {
  display: flex;
  background-color: #15192C;
  color: #f0f0f0;
  margin: 10px;
  padding: 10px;
  border-radius: 3px;
  span {
    flex: 1;
  }
  a {
    color: #365488;
    font-weight: bolder;
  }
}

h2 {
  color: #656A84;
  margin-left: 10px;
  text-transform: uppercase;
}

#decks {
  width: 100%;
  max-width: 800px;
  margin: 0 auto;
}

.deck-delete {
  justify-self: flex-end;
  align-self: flex-start;
  a {
    color: #660000;
    &:hover {
      color: #990000;
    }
  }
}

</style>
