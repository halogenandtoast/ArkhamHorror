<template>
  <div class="card-row">
    <header>
      <h2>Committed Skills</h2>
    </header>
    <div class="card-row-cards">
      <div v-for="card in cards" :key="card.id" class="card-row-card">
        <Card :game="game" :card="card" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { CardContents } from '@/arkham/types/Card';
import { MessageType } from '@/arkham/types/Message';
import Card from '@/arkham/components/Card.vue';

export default defineComponent({
  components: {
    Card,
  },
  props: {
    game: { type: Object as () => Game, required: true },
    cards: { type: Array as () => CardContents[], required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props) {
    const image = (card: CardContents) => {
      const { cardCode, isFlipped } = card
      const suffix = isFlipped === true ? 'b' : ''
      const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : ''
      return `${baseUrl}/img/arkham/cards/${cardCode.replace('c', '')}${suffix}.jpg`;
    }

    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    const targetAction = (cardId: string) => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.TARGET_LABEL
          && c.contents[0].contents === cardId);
    }

    const cardAction = (cardId: string) => {
      if (targetAction(cardId) !== -1) {
        return targetAction(cardId)
      }

      return -1;
    }

    return { image, cardAction }
  }
})
</script>

<style scoped lang="scss">
.card-row {
  background: #759686;
  width: 100%;
  overflow-x: auto;
  text-align: center;

  header {
    padding: 10px;
  }

  h2 {
    font-size: 1.8em;
    color: rgba(255,255,255,0.5);
    padding: 0;
    margin: 0;
  }
}

.card-row-cards {
  display: flex;
  align-items: center;
  justify-content: center;
  padding-bottom: 10px;
}

.card-row-card {
  margin-left: 10px;
  position: relative;
}

.card {
  width: $card-width;
  border-radius: 6px;
  margin: 2px;
}

.skill--can-interact {
  border: 3px solid $select;
  cursor: pointer;
}
</style>
