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
import { Vue, Component, Prop } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';

@Component
export default class PlayerSelector extends Vue {
  @Prop(Object) readonly game!: Game;
  @Prop(String) readonly investigatorId!: string;

  get choices(): Message[] {
    return choices(this.game, this.investigatorId);
  }

  get playerChoices() {
    return this
      .choices
      .map((choice, idx) => ({ choice, idx }))
      .filter(({ choice }) => choice.tag === MessageType.CHOOSE_PLAYER);
  }

  investigatorPortrait = (choice: Message) => {
    const iid = choice.contents[0];
    return `/img/arkham/portraits/${iid}.jpg`;
  }
}
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
