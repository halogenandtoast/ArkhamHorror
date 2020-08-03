<template>
  <div class="location-container">
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
    <div>
      <img
        v-if="investigateAction !== -1 && clues === 0"
        :class="{ 'location--can-interact': cardAction !== -1 }"
        class="card"
        :src="image"
        @click="warnAction('There are no clues left, are you sure?', investigateAction)"
      />
      <img
        v-else
        :class="{ 'location--can-interact': cardAction !== -1 }"
        class="card"
        :src="image"
        @click="$emit('choose', cardAction)"
      />
      <Treachery
        v-for="treacheryId in location.contents.treacheries"
        :key="treacheryId"
        :treachery="game.currentData.treacheries[treacheryId]"
        :game="game"
        @choose="$emit('choose', $event)"
      />
      <div v-if="location.contents.clues > 0" >
        <div class="poolItem">
          <img src="/img/arkham/clue.png" />
          <span>{{location.contents.clues}}</span>
        </div>
      </div>
    </div>
    <div>
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

  get clues() {
    return this.location.contents.clues;
  }

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

  warnAction(msg: string, action: number) {
    if (window.confirm(msg)) { // eslint-disable-line
      this.$emit('choose', action);
    }
  }
}
</script>

<style scoped lang="scss">
.location--can-interact {
  border: 3px solid #FF00FF;
  cursor: pointer;
}

.card {
  width: 150px;
  border-radius: 10px;
}

/deep/ .enemy {
  width: 100px;
}

/deep/ .treachery {
  object-fit: cover;
  object-position: 0 -104px;
  height: 104px;
}

.poolItem {
  position: relative;
  width: 30px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: black;
  font-weight: 900;
  font-size: 1.7em;

  img {
    width: 100%;
    position: absolute;
    top: 0;
    bottom: 0;
    left: 0;
    right: 0;
    margin: auto;
  }

  span {
    font-family: "Arkham";
    display: flex;
    position: relative;
    background: rgba(255,255,255,0.5);
    border-radius: 20px;
    font-size: 0.8em;
    width: 1.05em;
    height: 1.05em;
    align-items: center;
    justify-content: center;
  }
}

.portrait {
  border-radius: 3px;
}

.location-container {
  display: flex;
  margin: 0 5px;
}
</style>
