<template>
  <div>
    <img
      :src="image"
      class="card treachery"
      :class="{ 'treachery--can-interact': cardAction !== -1 }"
      @click="$emit('choose', cardAction)"
    />
    <div
      v-if="treachery.contents.clues && treachery.contents.clues > 0"
      class="poolItem"
    >
      <img src="/img/arkham/clue.png" />
      <span>{{treachery.contents.clues}}</span>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';
import * as Arkham from '@/arkham/types/Treachery';

@Component
export default class Treachery extends Vue {
  @Prop(Object) readonly game!: Game
  @Prop(Object) readonly treachery!: Arkham.Treachery

  get image() {
    return `/img/arkham/cards/${this.treachery.contents.cardCode}.jpg`;
  }

  get id() {
    return this.treachery.contents.id;
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
  width: 150px;
  border-radius: 5px;
}

.treachery--can-interact {
  border: 2px solid #FF00FF;
  cursor:pointer;
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
