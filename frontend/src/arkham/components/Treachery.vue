<template>
  <div>
    <img
      :src="image"
      class="card treachery"
      :class="{ 'treachery--can-interact': cardAction !== -1 }"
      @click="$emit('choose', cardAction)"
    />
    <PoolItem
      v-if="treachery.contents.clues && treachery.contents.clues > 0"
      type="clue"
      :amount="treachery.contents.clues"
    />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Treachery';

@Component({
  components: { PoolItem },
})
export default class Treachery extends Vue {
  @Prop(Object) readonly game!: Game
  @Prop(String) readonly investigatorId!: string
  @Prop(Object) readonly treachery!: Arkham.Treachery

  get image() {
    return `/img/arkham/cards/${this.treachery.contents.cardCode}.jpg`;
  }

  get id() {
    return this.treachery.contents.id;
  }

  get choices() {
    return choices(this.game, this.investigatorId);
  }

  get cardAction() {
    return this.choices.findIndex(this.canInteract);
  }

  canInteract(c: Message): boolean {
    switch (c.tag) {
      case MessageType.DISCARD:
        return c.contents.contents === this.id;
      case MessageType.ASSET_DAMAGE:
        return c.contents[0] === this.id;
      case MessageType.ACTIVATE_ABILITY:
        return c.contents[1].source.contents === this.id;
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
  width: 100px;
  max-width: 100px;
  border-radius: 5px;
}

.treachery--can-interact {
  border: 2px solid #FF00FF;
  cursor:pointer;
}
</style>
