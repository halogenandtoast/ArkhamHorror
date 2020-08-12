<template>
  <div class="act-container">
    <img
      :class="{ 'act--can-progress': advanceActAction !== -1 }"
      class="card card--sideways"
      @click="$emit('choose', advanceActAction)"
      :src="image"
    />
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

.card--sideways {
  width: auto;
  height: 100px;
}

.act--can-progress {
  border: 3px solid #ff00ff;
  border-radius: 8px;
  cursor: pointer;
}
</style>
