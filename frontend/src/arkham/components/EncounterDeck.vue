<template>
  <div>
    <img
      v-if="investigatorPortrait"
      class="portrait"
      :src="investigatorPortrait"
    />
    <img
      class="card"
      src="/img/arkham/back.png"
      :class="{ 'can-draw': drawEncounterCardAction !== -1 }"
      @click="$emit('choose', drawEncounterCardAction)"
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

  get investigatorPortrait() {
    const choice = this.choices.find((c) => c.tag === MessageType.INVESTIGATOR_DRAW_ENCOUNTER_CARD);
    if (choice) {
      return `/img/arkham/portraits/${choice.contents}.jpg`;
    }

    return null;
  }
}
</script>

<style scoped lang="scss">
.card {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 13px;
  margin: 2px;
  width: 130px;
}
.portrait {
  width: 100px;
}
.can-draw {
  border: 3px solid #FF00FF;
  cursor: pointer;
}
</style>
