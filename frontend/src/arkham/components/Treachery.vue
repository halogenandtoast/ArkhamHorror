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
    <PoolItem
      v-if="treachery.contents.resources && treachery.contents.resources > 0"
      type="resource"
      :amount="treachery.contents.resources"
    />
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Treachery';

export default defineComponent({
  components: { PoolItem },
  props: {
    game: { type: Object as () => Game, required: true },
    treachery: { type: Object as () => Arkham.Treachery, required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props) {
    const image = computed(() => {
      const baseUrl = process.env.NODE_ENV == 'production' ? "https://arkham-horror-assets.s3.amazonaws.com" : '';
      return `${baseUrl}/img/arkham/cards/${props.treachery.contents.cardDef.cardCode}.jpg`
    })
    const id = computed(() => props.treachery.contents.id)
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    function canInteract(c: Message): boolean {
      switch (c.tag) {
        case MessageType.DISCARD:
          return c.contents.contents === id.value;
        case MessageType.ASSET_DAMAGE:
          return c.contents[0] === id.value;
        case MessageType.ACTIVATE_ABILITY:
          return c.contents[1].source.contents === id.value;
        case MessageType.USE_CARD_ABILITY:
          return c.contents[1].contents === id.value;
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canInteract(c1));
        default:
          return false;
      }
    }

    const cardAction = computed(() => choices.value.findIndex(canInteract))

    return { image, cardAction }
  }
})
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
