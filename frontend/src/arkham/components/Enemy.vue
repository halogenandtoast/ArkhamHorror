<template>
  <div class="enemy">
    <img :src="image"
      :class="{'enemy--can-interact': cardAction !== -1 }"
      class="card enemy"
      @click="$emit('choose', cardAction)"
    />
    <button
      v-if="fightAction !== -1"
      class="button fight-button"
      @click="$emit('choose', fightAction)"
    >Fight</button>
    <button
      v-if="evadeAction !== -1"
      class="button evade-button"
      @click="$emit('choose', evadeAction)"
    >Evade</button>
    <button
      v-if="engageAction !== -1"
      class="button engage-button"
      @click="$emit('choose', engageAction)"
    >Engage</button>
    <button
      v-for="ability in abilities"
      :key="ability"
      class="button ability-button"
      @click="$emit('choose', ability)"
      >{{abilityLabel(ability)}}</button>
    <div class="pool">
      <PoolItem type="health" :amount="enemy.contents.damage" />
      <PoolItem v-if="enemy.contents.doom > 0" type="doom" :amount="enemy.contents.doom" />
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
  @Prop(String) readonly investigatorId!: string
  @Prop(Object) readonly enemy!: Arkham.Enemy

  get image() {
    const { cardCode } = this.enemy.contents;
    return `/img/arkham/cards/${cardCode}.jpg`;
  }

  get id() {
    return this.enemy.contents.id;
  }

  get cardAction() {
    if (this.attackAction !== -1) {
      return this.attackAction;
    }

    if (this.moveAction !== -1) {
      return this.moveAction;
    }

    if (this.placeDoomAction !== -1) {
      return this.placeDoomAction;
    }

    return this.damageAction;
  }

  get choices() {
    return choices(this.game, this.investigatorId);
  }

  get attackAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.ENEMY_ATTACK && c.contents[1] === this.id);
  }

  get moveAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.ENEMY_MOVE && c.contents[0] === this.id);
  }

  get placeDoomAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.PLACE_DOOM && c.contents[0].contents === this.id);
  }

  get damageAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.ENEMY_DAMAGE && c.contents[0] === this.id);
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

  get engageAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.ENGAGE_ENEMY && c.contents[1] === this.id);
  }

  abilityLabel(idx: number) {
    return this.choices[idx].contents[1].type.contents[1];
  }

  get abilities() {
    return this
      .choices
      .reduce<number[]>((acc, v, i) => {
        if (v.tag === 'ActivateCardAbilityAction' && v.contents[1].source.tag === 'EnemySource' && v.contents[1].source.contents === this.id) {
          return [...acc, i];
        }

        return acc;
      }, []);
  }
}
</script>

<style scoped lang="scss">
.enemy--can-interact {
  border: 3px solid #FF00FF;
  border-radius: 15px;
  cursor: pointer;
}

.enemy {
  display: flex;
  flex-direction: column;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.fight-button {
  background-color: #8F5B41;
  &:before {
    font-family: "Arkham";
    content: "\0044";
    margin-right: 5px;
  }
}

.evade-button {
  background-color: #576345;
  &:before {
    font-family: "Arkham";
    content: "\0053";
    margin-right: 5px;
  }
}

.engage-button {
  background-color: #555;
  &:before {
    font-family: "Arkham";
    content: "\0048";
    margin-right: 5px;
  }
}

.card {
  width: 100px;
  max-width: 100px;
  border-radius: 5px;
}

.pool {
  display: flex;
  flex-direction: row;
  height: 2em;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0049";
    margin-right: 5px;
  }
}
</style>
