<template>
  <div>
    <img
      :src="image"
      :class="{ 'asset--can-interact': cardAction !== -1, exhausted}"
      class="card"
      @click="$emit('choose', cardAction)"
    />
    <div
      v-if="asset.contents.uses && asset.contents.uses.amount > 0"
      class="poolItem poolItem-resource"
    >
      <img src="/img/arkham/resource.png" />
      <span>{{asset.contents.uses.amount}}</span>
    </div>
    <div v-if="hasAnyDamage" class="pool">
      <div
        v-if="asset.contents.sanityDamage && asset.contents.sanityDamage > 0"
        class="poolItem poolItem-sanity"
      >
        <img src="/img/arkham/sanity.png" />
        <span>{{asset.contents.sanityDamage}}</span>
      </div>
      <div
        v-if="asset.contents.healthDamage && asset.contents.healthDamage > 0"
        class="poolItem poolItem-health"
      >
        <img src="/img/arkham/health.png" />
        <span>{{asset.contents.healthDamage}}</span>
      </div>
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

  get hasAnyDamage() {
    const { sanityDamage, healthDamage } = this.asset.contents;
    return (sanityDamage && sanityDamage > 0) || (healthDamage && healthDamage > 0);
  }

  get exhausted() {
    return this.asset.contents.exhausted;
  }

  get cardCode() {
    return this.asset.contents.cardCode;
  }

  get image() {
    return `/img/arkham/cards/${this.cardCode}.jpg`;
  }

  get choices() {
    return choices(this.game);
  }

  get cardAction() {
    return this.choices.findIndex(this.canInteract);
  }

  canInteract(c: Message): boolean {
    switch (c.tag) {
      case MessageType.DISCARD_ASSET:
        return c.contents === this.id;
      case MessageType.ASSET_DAMAGE:
        return c.contents[0] === this.id;
      case MessageType.ACTIVATE_ABILITY:
        return c.contents[1][0].contents === this.id;
      case MessageType.USE_CARD_ABILITY:
        return c.contents[1][0].contents === this.id;
      case MessageType.RUN:
        return c.contents.some((c1: Message) => this.canInteract(c1));
      default:
        return false;
    }
  }
}
</script>

<style lang="scss" scoped>
.card {
  width: 130px;
  border-radius: 5px;
}

.exhausted {
  transform: rotate(90deg);
  padding: 0 30px;
}

.asset--can-interact {
  border: 2px solid #FF00FF;
  cursor:pointer;
}

.pool {
  display: flex;
  flex-direction: row;
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
</style>
