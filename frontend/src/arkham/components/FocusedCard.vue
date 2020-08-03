<template>
  <div class="card-container">
    <img
      :class="{'card--can-interact': cardAction !== -1}"
      class="card"
      :src="image"
      @click="$emit('choose', cardAction)"
    />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { Card } from '@/arkham/types/Card';
import { choices, Game } from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';

@Component
export default class FocusedCard extends Vue {
  @Prop(Object) readonly card!: Card
  @Prop(Object) readonly game!: Game

  get image() {
    const { cardCode } = this.card.contents;
    return `/img/arkham/cards/${cardCode}.jpg`;
  }

  get id() {
    return this.card.contents.id;
  }

  get choices() {
    return choices(this.game);
  }

  get cardAction() {
    return this.choices.findIndex(this.canInteract);
  }

  canInteract(c: Message): boolean {
    switch (c.tag) {
      case MessageType.ADD_FOCUSED_TO_HAND:
        return c.contents[1] === this.id;
      case MessageType.ADD_FOCUSED_TO_TOP_OF_DECK:
        return c.contents[1] === this.id;
      case MessageType.RUN:
        return c.contents.some((c1: Message) => this.canInteract(c1));
      default:
        return false;
    }
  }
}
</script>

<style scoped lang="scss">

.card {
  width: 120px;
  min-width: 120px;
  border-radius: 7px;
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 13px;
  margin: 2px;
  display: inline-block;

  &--can-interact {
    border: 2px solid #FF00FF;
    cursor: pointer;
  }
}
</style>
