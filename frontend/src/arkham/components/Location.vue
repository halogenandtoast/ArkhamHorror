<template>
  <div>
    <img
      :class="{ 'location--can-interact': cardAction !== -1 }"
      class="card"
      :src="image"
      @click="$emit('choose', cardAction)"
    />
    <Asset
      v-for="assetId in location.contents.assets"
      :asset="game.currentData.assets[assetId]"
      :game="game"
      :key="assetId"
      @choose="$emit('choose', $event)"
    />
    <Enemy
      v-for="enemyId in enemies"
      :key="enemyId"
      :enemy="game.currentData.enemies[enemyId]"
      :game="game"
    />
    <Treachery
      v-for="treacheryId in location.contents.treacheries"
      :key="treacheryId"
      :treachery="game.currentData.treacheries[treacheryId]"
      :game="game"
      @choose="$emit('choose', $event)"
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
import { choices, Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import Enemy from '@/arkham/components/Enemy.vue';
import Asset from '@/arkham/components/Asset.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import * as Arkham from '@/arkham/types/Location';

@Component({
  components: { Enemy, Treachery, Asset },
})
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
    return choices(this.game);
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

  get enemies() {
    const enemyIds = this.location.contents.enemies;
    return enemyIds
      .filter((e) => this.game.currentData.enemies[e].contents.engagedInvestigators.length === 0);
  }
}
</script>

<style scoped lang="scss">
.location--can-interact {
  border: 3px solid #FF00FF;
  cursor: pointer;
}

.card {
  width: 200px;
}
</style>
