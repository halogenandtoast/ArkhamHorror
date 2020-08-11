<template>
  <div v-if="source" class="modal">
    <div class="modal-contents">
      <img :src="image" />
      <template v-if="choiceUI === 'label'">
        <button v-for="(choice, index) in choices" :key="index" @click="$emit('choose', index)">
          {{choice.label}}
        </button>
      </template>
      <template v-if="choiceUI === 'image'">
        <div class="images">
          <img
            v-for="(choice, index) in choices"
            :key="index"
            :src="`/img/arkham/cards/${choice.label}.jpg`"
            @click="$emit('choose', index)"
          />
        </div>
      </template>
    </div>
  </div>
  <div v-else-if="focusedCards.length > 0" class="modal">
    <div class="modal-contents focused-cards">
      <FocusedCard
        v-for="(card, index) in focusedCards"
        :card="card"
        :game="game"
        :key="index"
        @choose="$emit('choose', $event)"
      />
    </div>
  </div>
  <div v-else-if="resolutions.length > 0" class="modal">
    <div class="modal-contents">
      <button
        v-for="{ choice, idx } in resolutions"
        :key="idx"
        @click="$emit('choose', idx)"
      >
        Resolution {{choice.contents}}
      </button>
    </div>
  </div>
</template>

<script lang="ts">
import { Vue, Component, Prop } from 'vue-property-decorator';
import { choicesSource, choices, Game } from '@/arkham/types/Game';
import { MessageType, Message } from '@/arkham/types/Message';
import FocusedCard from '@/arkham/components/FocusedCard.vue';

@Component({
  components: {
    FocusedCard,
  },
})
export default class ChoiceModal extends Vue {
  @Prop(Object) readonly game!: Game;
  @Prop(String) readonly investigatorId!: string

  get choices(): Message[] {
    return choices(this.game, this.investigatorId);
  }

  get focusedCards() {
    return this.game.currentData.focusedCards;
  }

  get choiceUI() {
    if (!this.source) {
      return null;
    }

    const { tag } = this.source;

    switch (tag) {
      case 'DeckSource':
        return 'image';
      default:
        return 'label';
    }
  }

  get source() {
    return choicesSource(this.game, this.investigatorId);
  }

  get resolutions() {
    return this
      .choices
      .map((choice, idx) => ({ choice, idx }))
      .filter(({ choice }) => choice.tag === MessageType.RESOLUTION);
  }

  get cardCode() {
    if (!this.source) {
      return null;
    }

    const { tag, contents } = this.source;

    if (!contents) {
      return null;
    }

    switch (tag) {
      case 'AgendaSource':
        return `${this.game.currentData.agendas[contents].contents.id}b`;
      default:
        return null;
    }
  }

  get image() {
    if (this.cardCode) {
      return `/img/arkham/cards/${this.cardCode}.jpg`;
    }

    return null;
  }
}
</script>

<style scoped lang="scss">
.modal {
  top: 0;
  left: 0;
  width: 100vw;
  height: 100vh;
  position: fixed;
  background-color: rgba(0, 0, 0, .7);
  display: flex;
  justify-content: center;
  align-items: center;
}

.modal-contents {
  width: 50vw;
  height: 30vw;
  background: white;
  border-radius: 20px;
  display: flex;
  align-items: center;
  justify-content: center;
  flex-direction: column;
}

.focused-cards {
  flex-direction: row;
}

.images img {
  width: 150px;
  margin: 2px;
  border: 3px solid #FF00FF;
  cursor: pointer;
}
</style>
