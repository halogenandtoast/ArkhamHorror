<script lang="ts" setup>
import { computed } from 'vue'
import {imgsrc, investigatorClass} from '@/arkham/helpers';
import * as Arkham from '@/arkham/types/Deck'

interface Props {
  deck: Arkham.Deck
  sync?: () => void
  markDelete?: () => void
}

const props = defineProps<Props>()

const deckUrlToPage = (url: string): string => {
  // converts https://arkhamdb.com/api/public/decklist/25027
  // to https://arkhamdb.com/decklist/view/25027
  // OR
  // converts https://arkhamdb.com/api/public/deck/25027
  // to https://arkhamdb.com/deck/view/25027
  return url.replace("/api/public/decklist", "/decklist/view").replace("/api/public/deck", "/deck/view")
}

const deckInvestigator = computed(() => {
  if (props.deck.list.meta) {
    try {
      const result = JSON.parse(props.deck.list.meta)
      if (result && result.alternate_front) {
        return result.alternate_front
      }
    } catch (e) { console.log("No parse") }
  }
  return props.deck.list.investigator_code.replace('c', '')
})

const deckClass = computed(() => {
  if (deckInvestigator.value) {
    return investigatorClass(deckInvestigator.value)
  }

  return {};
})

const tabooList = computed(() => {
  if (props.deck.list.taboo_id) {
    switch (props.deck.list.taboo_id) {
      case 1: return "1.5 (Apr 23, 2019)"
      case 2: return "1.6 (Sep 27, 2019)"
      case 3: return "1.8 (Oct 15, 2020)"
      case 4: return "1.9 (Jun 28, 2021)"
      case 5: return "2.0 (Aug 26, 2022)"
      case 6: return "2.1 (Aug 30, 2023)"
      case 7: return "2.2 (Feb 20, 2024)"
      default: return "Unknown Taboo List"
    }
  }

  return null
})
</script>

<template>
  <div class="decklist box" :class="deckClass">
    <img class="portrait--decklist" :src="imgsrc(`cards/${deckInvestigator}.jpg`)" />
    <div class="deck-details">
      <span class="deck-title"><router-link :to="{ name: 'Deck', params: { deckId: deck.id }}">{{deck.name}}</router-link></span>
      <span v-if="tabooList" class="taboo-list">Taboo: {{tabooList}}</span>
    </div>
    <div class="open-deck">
      <a v-if="deck.url" :href="deckUrlToPage(deck.url)" target="_blank" rel="noreferrer noopener"><font-awesome-icon alt="View Deck in ArkhamDB" icon="external-link" /></a>
    </div>
    <div v-if="deck.url && sync" class="sync-deck">
      <a href="#" @click.prevent="sync"><font-awesome-icon icon="refresh" /></a>
    </div>
    <div v-if="markDelete" class="deck-delete">
      <a href="#delete" @click.prevent="markDelete"><font-awesome-icon icon="trash" /></a>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.deck {
  display: flex;
  margin: 10px;
  padding: 10px;
  border-radius: 3px;
  span {
    flex: 1;
  }
  a {
    color: var(--title);
    &:hover {
      color: var(--title);
    }
  }
}

h2 {
  color: #656A84;
  margin-left: 10px;
  text-transform: uppercase;
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
  width: 150px;
  margin-right: 10px;
  border-radius: 5px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}

.deck-title {
  font-weight: 800;
  font-size: 1.2em;
  a {
    color: var(--title);
    text-decoration: none;
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

.decklist {
  display: flex;
  color: #f0f0f0;
  transition: all 0.5s; 

  span {
    flex: 1;
  }

  a {
    color: var(--title);
    font-weight: bolder;

    &:hover {
      color: rgba(0, 0, 0, 0.4);
    }
  }

  &.guardian:hover {
    background-color: var(--guardian-extra-dark);
    border-color: var(--guardian-dark);
  }

  &.seeker:hover {
    background-color: var(--seeker-extra-dark);
    border-color: var(--seeker-dark);
  }

  &.rogue:hover {
    background-color: var(--rogue-extra-dark);
    border-color: var(--rogue-dark);
  }

  &.mystic:hover {
    background-color: var(--mystic-extra-dark);
    border-color: var(--mystic-dark);
  }

  &.survivor:hover {
    background-color: var(--survivor-extra-dark);
    border-color: var(--survivor-dark);
  }

  &.neutral:hover {
    background-color: var(--neutral-extra-dark);
    border-color: var(--neutral-dark);
  }
}
</style>
