<template>
  <div v-if="playerChoices.length > 0" class="player-selector">
    <p>Choose lead investigator</p>
    <div class="modal-contents choose-player-order">
      <div class="choose-player-portraits">
        <div
          v-for="{ choice, idx } in playerChoices"
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

    const playerChoices = computed(() => {
      return choices
        .value
        .map((choice, idx) => ({ choice, idx }))
        .filter(({ choice }) => choice.tag === MessageType.CHOOSE_PLAYER);
    })

    const investigatorPortrait = (choice: Message) => {
      const iid = choice.contents[0];
      const baseUrl = process.env.NODE_ENV == 'production' ? process.env.VUE_APP_ASSET_HOST : '';
      return `${baseUrl}/img/arkham/portraits/${iid}.jpg`;
    }

    return { playerChoices, investigatorPortrait }
  }
})
</script>

<style scoped lang="scss">
.portrait {
  margin: 0 5px;
  width: 100px;
  border: 3px solid #FF00FF;
  cursor: pointer;
}

.choose-player-portraits {
  display: flex;
  flex-direction: row;
}

p {
  text-align: center;
}

.player-selector {
  display: flex;
  flex-direction: column;
  align-items: center;
  background-color: #336699;
}
</style>
