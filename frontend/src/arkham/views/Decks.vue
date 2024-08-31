<script lang="ts" setup>
import { ref, computed } from 'vue'
import * as Arkham from '@/arkham/types/Deck'
import Prompt from '@/components/Prompt.vue'
import { fetchDecks, deleteDeck, syncDeck } from '@/arkham/api'
import { capitalize } from '@/arkham/helpers'
import NewDeck from '@/arkham/components/NewDeck.vue';
import Deck from '@/arkham/components/Deck.vue';
import { useToast } from "vue-toastification";

const allDecks = ref<Arkham.Deck[]>([])
const deleteId = ref<string | null>(null)
const toast = useToast()

interface Filter {
  classes: string[]
}

const allClasses = ["guardian", "seeker", "rogue", "mystic", "survivor", "neutral"]

const filter = ref<Filter>({ classes: [] })

async function addDeck(d: Arkham.Deck) {
  allDecks.value.push(d)
}

async function deleteDeckEvent() {
  const { value } = deleteId
  if (value) {
    deleteDeck(value).then(() => {
      allDecks.value = allDecks.value.filter((deck) => deck.id !== value)
      deleteId.value = null
    })
  }
}

fetchDecks().then(async (response) => {
  allDecks.value = response
})

const decks = computed(() =>
  allDecks.value.filter((deck) => {
    if (filter.value.classes.length === 0) {
      return true
    }
    return filter.value.classes.some((k) => Arkham.deckClass(deck)[k])
  })
)

async function sync(deck: Arkham.Deck) {
  syncDeck(deck.id).then(() => {
    toast.success("Deck synced successfully", { timeout: 3000 })
  })
}

function toggleClass(c: string) {
  const { classes } = filter.value
  const index = classes.indexOf(c)
  if (index === -1) {
    classes.push(c)
  } else {
    classes.splice(index, 1)
  }
}
</script>

<template>
  <div class="page-container">
    <div id="decks" class="page-content column">
      <section>
        <header><h2 class="title">New Deck</h2></header>
        <NewDeck @new-deck="addDeck"/>
      </section>
      <section class="column">
        <h2 class="title">Existing Decks</h2>
        <section>
          <ul class="button-list">
            <li v-for="iclass in allClasses" :key="iclass" :class="{ [iclass]: true, off: !filter.classes.includes(iclass) }" @click="toggleClass(iclass)"><span :class="{ [`${iclass}-icon`]: true }"></span> {{capitalize(iclass)}}</li>
          </ul>
        </section>
        <div v-if="decks.length == 0" class="box">
          <p>You currently have no decks.</p>
        </div>
        <div v-else class="decks column">
          <transition-group name="deck">
            <div v-for="deck in decks" :key="deck.id" class="deck">
              <Deck :deck="deck" :markDelete="() => deleteId = deck.id" :sync="() => sync(deck)" />
            </div>
          </transition-group>
        </div>
      </section>

      <Prompt
        v-if="deleteId"
        prompt="Are you sure you want to delete this deck?"
        :yes="deleteDeckEvent"
        :no="() => deleteId = null"
      />
    </div>
  </div>
</template>

<style lang="scss" scoped>
#decks {
  min-width: 60vw;
  margin: 0 auto;
}

.open-deck {
  justify-self: flex-end;
  align-self: flex-start;
  margin-right: 10px;
}

.sync-deck {
  justify-self: flex-end;
  align-self: flex-start;
  margin-right: 10px;
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

.portrait--decklist {
  width: 100px;
  margin-right: 10px;
}

.deck-title {
  font-weight: 800;
  font-size: 1.2em;
  a {
    text-decoration: none;
    &:hover {
      color: #336699;
    }
  }
}

.deck-move,
.deck-enter-active,
.deck-leave-active {
  transition: all 0.5s ease;
}

.deck-enter-from,
.deck-leave-to {
  opacity: 0;
  transform: translateX(30px);
}

.deck-leave-active {
  position: absolute;
}

.deck span.taboo-list {
  font-size: 0.8em;
  background: rgba(255, 255, 255, 0.2);
  color: #efefef;
  display: inline-block;
  width: fit-content;
  height: fit-content;
  padding: 5px;
  border-radius: 5px;
  flex: 0;
  flex-basis: fit-content;
}

.deck-details {
  display: flex;
  flex-direction: column;
  flex: 1;
}

.button-list {
  display: flex;
  list-style: none;
  gap: 2px;
  li {
    flex-grow: 1;
    text-align: center;
    padding: 5px 10px;
    color: #fff;
    display: flex;
    flex-direction: row;
    justify-content: center;
    align-content: center;
    gap: 5px;
    cursor: pointer;
    user-select: none;

    &.guardian {
      background: var(--guardian-dark);
    }

    &.seeker {
      background: var(--seeker-dark);
    }

    &.rogue {
      background: var(--rogue-dark);
    }

    &.mystic {
      background: var(--mystic-dark);
    }

    &.survivor {
      background: var(--survivor-dark);
    }

    &.neutral {
      background: var(--neutral-dark);
    }

    &.off {
      background: #333;
    }
  }
}
</style>
