<template>
  <div class="modal">
    <div class="modal-contents">
      <img :src="targetImage" />
      <button v-for="(choice, index) in choices" :key="index" @click="$emit('choose', index + 1)">
        {{choice}}
      </button>
    </div>
  </div>
</template>

<script lang="ts">
import { Vue, Component, Prop } from 'vue-property-decorator';
import { ArkhamGame, ArkhamStepTypes } from '@/arkham/types/game';

@Component
export default class ChoiceModal extends Vue {
  @Prop(Object) readonly game!: ArkhamGame;

  get choices() {
    if (this.game.gameState.step.tag === ArkhamStepTypes.CHOOSE_ONE) {
      return this.game.gameState.step.contents.choices;
    }

    return [];
  }

  get targetImage() {
    if (this.game.gameState.step.tag === ArkhamStepTypes.CHOOSE_ONE) {
      const { choiceTarget } = this.game.gameState.step.contents;
      switch (choiceTarget.tag) {
        case 'AgendaTarget': return choiceTarget.contents.imageBack;
        case 'LocationTarget': return choiceTarget.contents.image;
        case 'EnemyTarget': return this.game.gameState.enemies[choiceTarget.contents].image;
        default:
        {
          const _exhaustiveCheck: never = choiceTarget; // eslint-disable-line
          throw new Error('Not handlig target type');
        }
      }
    }

    return null;
  }
}
</script>

<style scoped lang="scss">
.modal {
  top: 0;
  left: 0;
  width: 100vw;
  height: 100vh;
  position: fixed;
  background-color: rgba(0, 0, 0, .7);
  display: flex;
  justify-content: center;
  align-items: center;
}

.modal-contents {
  width: 50vw;
  height: 30vw;
  background: white;
  border-radius: 20px;
  display: flex;
  align-items: center;
  justify-content: center;
  flex-direction: column;
}
</style>
