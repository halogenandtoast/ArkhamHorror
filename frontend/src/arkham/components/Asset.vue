<template>
  <div>
    <img
      :src="image"
      :class="{ 'asset--can-interact': availableAction !== -1}"
      class="card"
      @click="$emit('choose', availableAction)"
    />
    <div
      v-if="asset.contents.uses && asset.contents.uses.amount > 0"
      class="poolItem poolItem-resource"
    >
      <img src="/img/arkham/resource.png" />
      {{asset.contents.uses.amount}}
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';
import * as Arkham from '@/arkham/types/Asset';

@Component
export default class Asset extends Vue {
  @Prop(Object) readonly game!: Game
  @Prop(Object) readonly asset!: Arkham.Asset

  get id() {
    return this.asset.contents.id;
  }

  get cardCode() {
    return this.asset.contents.cardCode;
  }

  get image() {
    return `/img/arkham/cards/${this.cardCode}.jpg`;
  }

  get availableAction() {
    if (this.activateAbilityAction !== -1) {
      return this.activateAbilityAction;
    }

    if (this.useAbilityAction !== -1) {
      return this.useAbilityAction;
    }

    return this.discardAssetAction;
  }

  get choices() {
    return choices(this.game);
  }

  get useAbilityAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.USE_CARD_ABILITY && this.isIn(c.contents));
  }

  get activateAbilityAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.ACTIVATE_ABILITY && this.isIn(c.contents));
  }

  isIn(contents: any) {
    return contents[1][0].contents === this.id;
  }

  get discardAssetAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.DISCARD_ASSET && c.contents === this.id);
  }
}
</script>

<style lang="scss" scoped>
.card {
  width: 200px;
}

.asset--can-interact {
  border: 2px solid #FF00FF;
  cursor:pointer;
}
</style>
