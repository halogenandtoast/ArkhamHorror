<template>
  <div class="act-container">
    <img
      :class="{ 'act--can-progress': advanceActAction !== -1 }"
      class="card card--sideways"
      @click="$emit('choose', advanceActAction)"
      :src="image"
    />
    <button
      v-for="ability in abilities"
      :key="ability"
      class="button ability-button"
      @click="$emit('choose', ability)"
      >{{abilityLabel(ability)}}</button>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import * as Arkham from '@/arkham/types/Act';

@Component
export default class Act extends Vue {
  @Prop(Object) readonly act!: Arkham.Act;
  @Prop(Object) readonly game!: Game;
  @Prop(String) readonly investigatorId!: string;

  get id() {
    return this.act.contents.id;
  }

  get image() {
    if (this.act.contents.flipped) {
      return `/img/arkham/cards/${this.id}b.jpg`;
    }

    return `/img/arkham/cards/${this.id}.jpg`;
  }

  get choices() {
    return choices(this.game, this.investigatorId);
  }

  get advanceActAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.ADVANCE_ACT);
  }

  abilityLabel(idx: number) {
    return this.choices[idx].contents[1].type.contents[1];
  }

  get abilities() {
    return this
      .choices
      .reduce<number[]>((acc, v, i) => {
        if (v.tag === 'ActivateCardAbilityAction' && v.contents[1].source.tag === 'ActSource' && v.contents[1].source.contents === this.id) {
          return [...acc, i];
        }

        return acc;
      }, []);
  }
}
</script>

<style scoped lang="scss">
.card {
  width: 100px;
  -webkit-box-shadow: 0 3px 6px rgba(0, 0, 0, 0.23), 0 3px 6px rgba(0, 0, 0, 0.53);
  box-shadow: 0 3px 6px rgba(0, 0, 0, 0.23), 0 3px 6px rgba(0, 0, 0, 0.53);
  border-radius: 6px;
  margin: 2px;
}

.act-container {
  display: flex;
  flex-direction: column;
}

.card--sideways {
  width: auto;
  height: 100px;
}

.act--can-progress {
  border: 3px solid #ff00ff;
  border-radius: 8px;
  cursor: pointer;
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
