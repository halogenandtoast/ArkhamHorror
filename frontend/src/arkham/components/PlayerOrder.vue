<template>
  <div v-if="playerOrderChoices.length > 0" class="player-order">
    <div class="modal-contents choose-player-order">
      <p>Choose {{ordinal}} Player</p>
      <div class="choose-player-order-portraits">
        <div
          v-for="{ choice, idx } in playerOrderChoices"
          :key="idx"
          @click="$emit('choose', idx)"
        >
          <img
            class="portrait"
            :src="investigatorPortrait(choice)"
          />
        </div>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';

export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true }
  },
  setup(props) {
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    const playerOrderChoices = computed(() => {
      return choices
        .value
        .map((choice, idx) => ({ choice, idx }))
        .filter(({ choice }) => choice.tag === MessageType.CHOOSE_PLAYER_ORDER);
    })

    const ordinal = computed(() => {
      switch (playerOrderChoices.value[0].choice.contents[1].length) {
        case 1: return 'First';
        case 2: return 'Second';
        case 3: return 'Third';
        case 4: return 'Fourth';
        default: return 'Unknown';
      }
    })

    const investigatorPortrait = (choice: Message) => {
      const iid = choice.contents[1].slice(-1);
      const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';
      return `${baseUrl}/img/arkham/portraits/${iid.replace('c', '')}.jpg`;
    }

    return { investigatorPortrait, ordinal, playerOrderChoices }
  }
})
</script>

<style scoped lang="scss">
.portrait {
  margin: 0 5px;
  width: $card-width;
  border: 3px solid $select;
  cursor: pointer;
}

.choose-player-order-portraits {
  display: flex;
  flex-direction: row;
}

p {
  text-align: center;
}

.player-order {
  display: flex;
  flex-direction: column;
  align-items: center;
  background-color: #336699;
}
</style>
