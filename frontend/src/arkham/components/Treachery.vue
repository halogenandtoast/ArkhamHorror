<template>
  <div class="treachery">
    <img
      :src="image"
      class="card treachery"
      :class="{ 'treachery--can-interact': cardAction !== -1 }"
      @click="$emit('choose', cardAction)"
    />
    <AbilityButton
      v-for="ability in abilities"
      :key="ability"
      :ability="choices[ability]"
      @click="$emit('choose', ability)"
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
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import * as Arkham from '@/arkham/types/Treachery';

export default defineComponent({
  components: { PoolItem, AbilityButton },
  props: {
    game: { type: Object as () => Game, required: true },
    treachery: { type: Object as () => Arkham.Treachery, required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props) {
    const image = computed(() => {
      const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';
      return `${baseUrl}/img/arkham/cards/${props.treachery.contents.cardCode.replace('c', '')}.jpg`
    })
    const id = computed(() => props.treachery.contents.id)
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    function canInteract(c: Message): boolean {
      switch (c.tag) {
        case MessageType.DISCARD:
          return c.contents.contents === id.value;
        case MessageType.ASSET_DAMAGE:
          return c.contents[0] === id.value;
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canInteract(c1));
        default:
          return false;
      }
    }

    function isActivate(v: Message) {
      if (v.tag !== 'UseAbility') {
        return false
      }

      const { tag, contents } = v.contents[1].source;

      if (tag === 'TreacherySource' && contents === id.value) {
        return true
      }

      if (tag === 'ProxySource' && contents[0].tag === 'TreacherySource' && contents[0].contents === id.value) {
        return true
      }

      return false
    }

    const abilities = computed(() => {
      return choices
        .value
        .reduce<number[]>((acc, v, i) => {
          if (v.tag === 'Run' && isActivate(v.contents[0])) {
            return [...acc, i];
          } else if (isActivate(v)) {
            return [...acc, i];
          }

          return acc;
        }, []);
    })

    const cardAction = computed(() => choices.value.findIndex(canInteract))

    return { abilities, choices, image, cardAction }
  }
})
</script>

<style lang="scss" scoped>
.card {
  width: $card-width;
  max-width: $card-width;
  border-radius: 5px;
}

.treachery--can-interact {
  border: 2px solid $select;
  cursor:pointer;
}

.treachery {
  display: flex;
  flex-direction: column;
}
</style>
