<template>
  <div>
    <img
      :class="{ 'card--can-play': playCardAction !== -1 }"
      class="card"
      :src="image"
      @click="$emit('choose', playCardAction)"
    />

  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { Card } from '@/arkham/types/Card';
import { Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';

@Component
export default class HandCard extends Vue {
  @Prop(Object) readonly card!: Card
  @Prop(Object) readonly game!: Game
  @Prop(Boolean) readonly canPlay!: boolean
  @Prop(Boolean) readonly canCommit!: boolean
  @Prop(Boolean) readonly isCommited!: boolean

  get image() {
    const { cardCode } = this.card.contents;
    return `/img/arkham/cards/${cardCode}.jpg`;
  }

  get id() {
    return this.card.contents.id;
  }

  get choices() {
    return this.game.currentData.question.contents;
  }

  get playCardAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.PLAY_CARD && c.contents[1] === this.id);
  }
}
</script>

<style scoped lang="scss">

.card {
  width: 150px;
  border-radius: 7px;
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 13px;
  margin: 2px;

  &.commited {
    margin-top: -10px;
  }

  &.playable {
    border: 2px solid #ff00ff;
    cursor: pointer;
  }

  &.commitable {
    border: 2px solid #ff00ff;
    cursor: pointer;
  }

  &--can-play {
    border: 3px solid #FF00FF;
    border-radius: 10px;
    cursor: pointer;
  }
}
</style>
