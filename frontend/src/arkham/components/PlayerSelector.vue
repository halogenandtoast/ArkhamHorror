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

const playerChoices = computed(() => {
  return choices
    .value
    .map((choice, idx) => ({ choice, idx }))
    .filter(({ choice }) => choice.tag === "WOMBAT");
})

const playerChoicesMessage = computed(() => {
  const messageType = playerChoices.value[0]?.choice?.contents[1]
  switch (messageType) {
    case 'SetTurnPlayer': return "Choose player to take turn"
    case 'SetLeadInvestigator': return "Choose lead investigator"
    default: return null
  }
})

const baseUrl = inject('baseUrl')

const investigatorPortrait = (choice: Message) => {
  const iid = choice.contents[0];
  return `${baseUrl}/img/arkham/portraits/${iid.replace('c', '')}.jpg`;
}
</script>

<template>
  <div v-if="playerChoices.length > 0" class="player-selector">
    <p>{{playerChoicesMessage}}</p>
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

<style scoped lang="scss">
.portrait {
  margin: 0 5px;
  width: $card-width;
  border: 3px solid $select;
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
