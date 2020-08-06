<template>
  <div class="card-container">
    <img
      :class="classObject"
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
export default class HandCard extends Vue {
  @Prop(Object) readonly card!: Card
  @Prop(Object) readonly game!: Game
  @Prop(Boolean) readonly isCommited!: boolean

  get classObject() {
    return {
      'card--can-interact': this.cardAction !== -1,
      'card--committed': this.uncommitCardAction !== -1,
    };
  }

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

  get discardCardAction() {
    return this.choices.findIndex(this.mustDiscard);
  }

  get playCardAction() {
    return this.choices.findIndex(this.canPlay);
  }

  get commitCardAction() {
    return this.choices.findIndex(this.canCommit);
  }

  get uncommitCardAction() {
    return this.choices.findIndex(this.canUncommit);
  }

  get cardAction() {
    if (this.playCardAction !== -1) {
      return this.playCardAction;
    }

    if (this.uncommitCardAction !== -1) {
      return this.uncommitCardAction;
    }

    if (this.discardCardAction !== -1) {
      return this.discardCardAction;
    }

    return this.commitCardAction;
  }

  canPlay(c: Message): boolean {
    switch (c.tag) {
      case MessageType.PLAY_CARD:
        return c.contents[1] === this.id;
      case MessageType.RUN:
        return c.contents.some((c1: Message) => this.canPlay(c1));
      default:
        return false;
    }
  }

  mustDiscard(c: Message): boolean {
    switch (c.tag) {
      case MessageType.DISCARD_CARD:
        return c.contents[1] === this.id;
      case MessageType.RUN:
        return c.contents.some((c1: Message) => this.mustDiscard(c1));
      default:
        return false;
    }
  }

  canCommit(c: Message): boolean {
    switch (c.tag) {
      case MessageType.COMMIT_CARD:
        return c.contents[1] === this.id;
      case MessageType.RUN:
        return c.contents.some((c1: Message) => this.canCommit(c1));
      default:
        return false;
    }
  }

  canUncommit(c: Message): boolean {
    switch (c.tag) {
      case MessageType.UNCOMMIT_CARD:
        return c.contents[1] === this.id;
      case MessageType.RUN:
        return c.contents.some((c1: Message) => this.canUncommit(c1));
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

  &--can-uncommit {
    margin-top: -10px;
  }
}
</style>
