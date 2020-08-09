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
import { Vue, Component, Prop } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';

@Component
export default class PlayerOrder extends Vue {
  @Prop(Object) readonly game!: Game;

  get choices(): Message[] {
    return choices(this.game);
  }

  get ordinal() {
    switch (this.playerOrderChoices[0].choice.contents[1].length) {
      case 1: return 'First';
      case 2: return 'Second';
      case 3: return 'Third';
      case 4: return 'Fourth';
      default: return 'Unknown';
    }
  }

  get playerOrderChoices() {
    return this
      .choices
      .map((choice, idx) => ({ choice, idx }))
      .filter(({ choice }) => choice.tag === MessageType.CHOOSE_PLAYER_ORDER);
  }

  investigatorPortrait = (choice: Message) => {
    const iid = choice.contents[1].slice(-1);
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
