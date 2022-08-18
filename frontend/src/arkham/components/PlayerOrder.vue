<script lang="ts" setup>
import { computed, inject } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { Message } from '@/arkham/types/Message';

export interface Props {
  game: Game
  investigatorId: string
}

const props = defineProps<Props>()

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

const playerOrderChoices = computed(() => {
  return choices
    .value
    .map((choice, idx) => ({ choice, idx }))
    .filter(({ choice }) => choice.tag === "WOMBAT");
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

const baseUrl = inject('baseUrl')

const investigatorPortrait = (choice: Message) => {
  const iid = choice.contents[1].slice(-1)[0];
  return `${baseUrl}/img/arkham/portraits/${iid.replace('c', '')}.jpg`;
}
</script>

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
