<template>
  <div>
    <img
      v-if="investigatorPortrait"
      class="portrait"
      :src="investigatorPortrait"
    />
    <img
      class="deck"
      src="/img/arkham/back.png"
      :class="{ 'can-interact': deckAction !== -1 }"
      @click="$emit('choose', deckAction)"
    />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';

@Component
export default class EncounterDeck extends Vue {
  @Prop(Object) readonly game!: Game;

  get choices() {
    return choices(this.game);
  }

  get drawEncounterCardAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.INVESTIGATOR_DRAW_ENCOUNTER_CARD);
  }

  get searchTopOfEncounterCardAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.SEARCH_TOP_OF_DECK && c.contents[1].tag === 'EncounterDeckTarget');
  }

  get deckAction() {
    if (this.drawEncounterCardAction !== -1) {
      return this.drawEncounterCardAction;
    }

    return this.searchTopOfEncounterCardAction;
  }

  get investigatorPortrait() {
    const choice = this.choices[this.deckAction];

    if (!choice) {
      return null;
    }

    switch (choice.tag) {
      case MessageType.INVESTIGATOR_DRAW_ENCOUNTER_CARD:
        return `/img/arkham/portraits/${choice.contents}.jpg`;
      default:
        return `/img/arkham/portraits/${choice.contents[0]}.jpg`;
    }
  }
}
</script>

<style scoped lang="scss">
.deck {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  width: 100px;
}
.portrait {
  width: 100px;
}
.can-interact {
  border: 3px solid #FF00FF;
  cursor: pointer;
}
</style>
