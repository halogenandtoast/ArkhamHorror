<template>
  <div class="location-container">
    <div
      v-for="cardCode in location.contents.investigators"
      :key="cardCode"
    >
      <img
        :src="portrait(cardCode)"
        class="portrait"
      />
    </div>
    <div class="location-column">
      <font-awesome-icon v-if="blocked" :icon="['fab', 'expeditedssl']" class="status-icon" />
      <img
        :class="{ 'location--can-interact': cardAction !== -1 }"
        class="card"
        :src="image"
        @click="$emit('choose', cardAction)"
      />
      <button
        v-if="investigateAction !== -1"
        class="button investigate-button"
        @click="doInvestigate"
      >Investigate</button>
      <button
        v-for="ability in abilities"
        :key="ability"
        class="button ability-button"
        @click="$emit('choose', ability)"
        >{{abilityLabel(ability)}}</button>
      <Treachery
        v-for="treacheryId in location.contents.treacheries"
        :key="treacheryId"
        :treachery="game.currentData.treacheries[treacheryId]"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
      <div v-if="location.contents.clues > 0" class="pool">
        <PoolItem type="clue" :amount="location.contents.clues" />
      </div>
    </div>
    <div>
      <Asset
        v-for="assetId in location.contents.assets"
        :asset="game.currentData.assets[assetId]"
        :game="game"
        :investigatorId="investigatorId"
        :key="assetId"
        @choose="$emit('choose', $event)"
      />
      <Enemy
        v-for="enemyId in enemies"
        :key="enemyId"
        :enemy="game.currentData.enemies[enemyId]"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
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
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Location';

@Component({
  components: {
    Enemy,
    Treachery,
    Asset,
    PoolItem,
  },
})
export default class Location extends Vue {
  @Prop(Object) readonly game!: Game;
  @Prop(String) readonly investigatorId!: string;
  @Prop(Object) readonly location!: Arkham.Location;

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
    return choices(this.game, this.investigatorId);
  }

  get cardAction() {
    if (this.attachTreacheryToLocationAction !== -1) {
      return this.attachTreacheryToLocationAction;
    }

    if (this.enemySpawnAction !== -1) {
      return this.enemySpawnAction;
    }

    if (this.moveToAction !== -1) {
      return this.moveToAction;
    }

    return this.moveAction;
  }

  get investigateAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.INVESTIGATE && c.contents[1] === this.id);
  }

  get moveAction() {
    const isRunMove = this.choices.findIndex((c) => c.tag === MessageType.RUN
      && c.contents[0]
      && c.contents[0].tag === MessageType.MOVE
      && c.contents[0].contents[1] === this.id);

    if (isRunMove !== -1) {
      return isRunMove;
    }

    return this
      .choices
      .findIndex((c) => c.tag === MessageType.MOVE && c.contents[1] === this.id);
  }

  get moveToAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.MOVE_TO && c.contents[1] === this.id);
  }

  get attachTreacheryToLocationAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.ATTACH_TREACHERY
        && c.contents[1].contents === this.id);
  }

  get enemySpawnAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.ENEMY_SPAWN
        && c.contents[0] === this.id);
  }

  abilityLabel(idx: number) {
    return this.choices[idx].contents[1].type.contents[1];
  }

  get abilities() {
    return this
      .choices
      .reduce<number[]>((acc, v, i) => {
        if (v.tag === 'ActivateCardAbilityAction' && v.contents[1].source.tag === 'LocationSource' && v.contents[1].source.contents === this.id) {
          return [i, ...acc];
        }

        return acc;
      }, []);
  }

  get enemies() {
    const enemyIds = this.location.contents.enemies;
    return enemyIds
      .filter((e) => this.game.currentData.enemies[e].contents.engagedInvestigators.length === 0);
  }

  get blocked() {
    return this.location.contents.blocked;
  }

  doInvestigate() {
    if (this.clues === 0) {
      this.warnAction('There are no clues left, are you sure?', this.investigateAction);
    } else {
      this.$emit('choose', this.investigateAction);
    }
  }

  warnAction(msg: string, action: number) {
    if (window.confirm(msg)) { // eslint-disable-line
      this.$emit('choose', action);
    }
  }

  portrait = (cardCode: string) => `/img/arkham/portraits/${cardCode}.jpg`
}
</script>

<style scoped lang="scss">
.location--can-interact {
  border: 3px solid #FF00FF;
  cursor: pointer;
}

.card {
  width: 100px;
  border-radius: 10px;
}

/deep/ .enemy {
  width: 80px;
}

/deep/ .treachery {
  object-fit: cover;
  object-position: 0 -74px;
  height: 68px;
  margin-top: 2px;
}

.portrait {
  border-radius: 3px;
  width: 60px;
  margin-right: 2px;
}

.location-container {
  display: flex;
  margin: 0 5px;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.investigate-button {
  background-color: #40263A;
  &:before {
    font-family: "arkham";
    content: "\0046";
    margin-right: 5px;
  }
}

.ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0049";
    margin-right: 5px;
  }
}

.location-column {
  display: flex;
  flex-direction: column;
  position: relative;
}

.pool {
  display: flex;
  flex-direction: row;
  height: 2em;
}

.status-icon {
  align-self: center;
  background: rgba(255, 255, 255, 0.7);
  border-radius: 1.5em;
  font-size: 2.6em;
  color: rgba(0, 0, 0, 0.8);
  position: absolute;
  top: 19px;
  pointer-events: none;
}
</style>
