<template>
  <div>
    <img
      :class="{ 'location--can-interact': cardAction !== -1 }"
      class="card"
      :src="image"
      @click="$emit('choose', cardAction)"
    />
    <div
      v-for="cardCode in location.contents.investigators"
      :key="cardCode"
    >
      <img
        :src="portrait(cardCode)"
        class="portrait"
        width="80"
      />
    </div>
    <div
      v-for="enemyId in location.enemies"
      :key="enemyId"
    >
      <img
        v-if="!game.gameState.enemies[enemyId].isEngaged"
        :src="game.gameState.enemies[enemyId].image"
        width="250"
      />
    </div>
    <div v-if="location.contents.clues > 0" >
      <div>
        <img src="/img/arkham/clue.png" />
        {{location.contents.clues}}
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import * as Arkham from '@/arkham/types/Location';

@Component
export default class Location extends Vue {
  @Prop(Object) readonly game!: Game;
  @Prop(Object) readonly location!: Arkham.Location;

  portrait = (cardCode: string) => `/img/arkham/portraits/${cardCode}.jpg`

  get image() {
    const { id, revealed } = this.location.contents;
    const suffix = revealed ? '' : 'b';

    return `/img/arkham/cards/${id}${suffix}.jpg`;
  }

  get id() {
    return this.location.contents.id;
  }

  get choices() {
    return this.game.currentData.question.contents;
  }

  get cardAction() {
    if (this.investigateAction !== -1) {
      return this.investigateAction;
    }

    return this.moveAction;
  }

  get investigateAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.INVESTIGATE && c.contents[1] === this.id);
  }

  get moveAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.MOVE && c.contents[1] === this.id);
  }
}
</script>

<style scoped lang="scss">
.location--can-interact {
  border: 3px solid #FF00FF;
  cursor: pointer;
}
</style>
