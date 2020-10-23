<template>
  <div id="decks">
    <div>
      <h2>New Deck</h2>
      <div class="new-deck">
        <img v-if="investigator" class="portrait" :src="`/img/arkham/portraits/${investigator}.jpg`" />
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

    <div v-if="deleteId" class="cd-popup" role="alert">
       <div class="cd-popup-container">
          <p>Are you sure you want to delete this deck?</p>
          <ul class="cd-buttons">
             <li><a @click.prevent="deleteDeckEvent" href="#yes">Yes</a></li>
             <li><a @click.prevent="deleteId = null" href="#no">No</a></li>
          </ul>
          <a
            @click.prevent="deleteId = null"
            href="#cancel"
            class="cd-popup-close img-replace">Close</a>
       </div>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, ref } from 'vue';
import * as Arkham from '@/arkham/types/Deck';
import { fetchDecks, newDeck, deleteDeck } from '@/arkham/api';

export default defineComponent({
  setup() {
    const ready = ref(false)
    const decks = ref<Arkham.Deck[]>([])

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

    return { pasteDeck, createDeck, deleteDeckEvent, deleteId, deck, decks, loadDeck, investigator, deckName }
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

.cd-popup {
  position: fixed;
  left: 0;
  top: 0;
  height: 100%;
  width: 100%;
  background-color: rgba(94,110,141,.9);
}

.cd-popup-container {
  position: relative;
  width: 90%;
  max-width: 400px;
  margin: 4em auto;
  background: #fff;
  border-radius: .25em .25em 0 0;
  text-align: center;
  box-shadow: 0 0 20px rgba(0,0,0,.2);

  p {
    padding: 3em 1em;
    margin: 0;
  }
}

.cd-popup-close {
  position: absolute;
  top: 8px;
  right: 8px;
  width: 30px;
  height: 30px;

  &::before {
    transform: rotate(45deg);
    left: 8px;
    content: '';
    position: absolute;
    top: 12px;
    width: 14px;
    height: 3px;
    background-color: #8f9cb5;
  }

  &::after {
    transform: rotate(135deg);
    left: 8px;
    content: '';
    position: absolute;
    top: 12px;
    width: 14px;
    height: 3px;
    background-color: #8f9cb5;
  }
}

.cd-buttons {
  list-style: none;
  padding: 0;
  margin: 0;
  a {
    display: block;
    height: 60px;
    line-height: 60px;
    text-transform: uppercase;
    text-decoration: none;
    color: #fff;
  }
  li:first-child a {
    background: #fc7169;
    border-radius: 0 0 0 .25em;
    &:hover {
        background-color: #fc8982;
    }
  }
  li:last-child a {
    background: #b6bece;
    border-radius: 0 0 .25em 0;
    &:hover {
        background-color: #c5ccd8;
    }
  }
  li {
    float: left;
    width: 50%;
  }
}

.img-replace {
  display: inline-block;
  overflow: hidden;
  text-indent: 100%;
  color: transparent;
  white-space: nowrap;
}
</style>
