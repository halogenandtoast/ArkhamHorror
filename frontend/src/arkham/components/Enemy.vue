<template>
  <div class="enemy">
    <img :src="image"
      :class="{'enemy--can-attack': attackAction !== -1 }"
      class="card enemy"
      @click="$emit('choose', attackAction)"
    />
    <div v-if="canInteract" class="enemy-interactions">
      <button
        v-if="fightAction !== -1"
        class="fight-button"
        @click="$emit('choose', fightAction)"
      >Fight</button>
      <button
        v-if="evadeAction !== -1"
        class="evade-button"
        @click="$emit('choose', evadeAction)"
      >Evade</button>
    </div>
    <div class="pool">
      <PoolItem type="health" :amount="enemy.contents.damage" />
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Enemy';

@Component({
  components: { PoolItem },
})
export default class Enemy extends Vue {
  @Prop(Object) readonly game!: Game
  @Prop(Object) readonly enemy!: Arkham.Enemy

  get image() {
    const { cardCode } = this.enemy.contents;
    return `/img/arkham/cards/${cardCode}.jpg`;
  }

  get id() {
    return this.enemy.contents.id;
  }

  get canInteract() {
    return this.fightAction !== -1 || this.evadeAction !== -1;
  }

  get choices() {
    return choices(this.game);
  }

  get attackAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.ENEMY_ATTACK && c.contents[1] === this.id);
  }

  get fightAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.FIGHT_ENEMY && c.contents[1] === this.id);
  }

  get evadeAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.EVADE_ENEMY && c.contents[1] === this.id);
  }
}
</script>

<style scoped lang="scss">
.enemy--can-attack {
  border: 3px solid #FF00FF;
  border-radius: 15px;
  cursor: pointer;
}

.enemy {
  position: relative;
  display: inline-block;
}

.enemy-interactions {
  position: absolute;
  box-sizing: border-box;
  bottom: 58px;
  left: 9px;
  width: calc(100% - 20px);
  display: flex;
  button {
    cursor: pointer;
    flex: 1;
    text-transform: uppercase;
    font-size: 0.7em;
    padding: 5px 0px;
  }
}

.fight-button {
  border: 0;
  color: #FFF;
  background-color: #8F5B41;
  border-top-left-radius: 7px;
  border-bottom-left-radius: 7px;
  border: 1px solid #FF00FF;
  &:before {
    font-family: "Arkham";
    content: "\0044";
    margin-right: 5px;
  }
}

.evade-button {
  border: 0;
  color: #FFF;
  background-color: #576345;
  border-top-right-radius: 7px;
  border-bottom-right-radius: 7px;
  border: 1px solid #FF00FF;
  border-left: 0;
  &:before {
    font-family: "Arkham";
    content: "\0053";
    margin-right: 5px;
  }
}

.card {
  width: 200px;
  border-radius: 5px;
}
</style>
