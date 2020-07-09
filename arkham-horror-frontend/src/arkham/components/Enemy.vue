<template>
  <div class="enemy">
    <img
      v-if="canInteract"
      @click="interact"
      :src="enemy.image"
      class="card enemy--can-fight"
    />
    <img v-else :src="enemy.image" class="card" />
    <div v-if="focused" class="enemy-interactions">
      <button class="fight-button" @click="fightEnemy">Fight</button>
      <button class="evade-button" @click="evadeEnemy">Evade</button>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { ArkhamAction, ArkhamActionTypes } from '@/arkham/types/action';
import { ArkhamGame, ArkhamStepTypes } from '@/arkham/types/game';
import { performAction, performSelectEnemy } from '@/arkham/api';

@Component
export default class Enemy extends Vue {
  @Prop(Object) readonly game!: ArkhamGame
  @Prop(String) readonly enemyId!: string
  @Prop(Boolean) readonly focused!: boolean

  get enemy() {
    return this.game.gameState.enemies[this.enemyId];
  }

  get canInteract() {
    if (this.focused) {
      return false;
    }

    return this.canFight || this.shouldResolve;
  }

  get canFight() {
    return this.game.gameState.step.tag === ArkhamStepTypes.INVESTIGATOR_ACTION;
  }

  get canEvade() {
    return this.game.gameState.step.tag === ArkhamStepTypes.INVESTIGATOR_ACTION;
  }

  get shouldResolve() {
    return (
      this.game.gameState.step.tag === ArkhamStepTypes.RESOLVE_ENEMIES
      || this.game.gameState.step.tag === ArkhamStepTypes.RESOLVE_ATTACKS_OF_OPPORTUNITY
    )
    && this.game.gameState.step.contents.enemyIds.includes(this.enemyId);
  }

  interact() {
    if (this.canFight || this.canEvade) {
      this.$emit('focusEnemy', this.enemyId);
    }

    if (this.shouldResolve) {
      performSelectEnemy(this.game.id, this.enemyId).then((game: ArkhamGame) => {
        this.$emit('update', game);
      });
    }
  }

  fightEnemy() {
    const action: ArkhamAction = {
      tag: ArkhamActionTypes.FIGHT_ENEMY,
      contents: this.enemyId,
    };

    performAction(this.game.id, action).then((game: ArkhamGame) => {
      this.$emit('update', game);
      this.$emit('focusedEnemy', null);
    });
  }

  evadeEnemy() {
    const action: ArkhamAction = {
      tag: ArkhamActionTypes.EVADE_ENEMY,
      contents: this.enemyId,
    };

    performAction(this.game.id, action).then((game: ArkhamGame) => {
      this.$emit('update', game);
      this.$emit('focusedEnemy', null);
    });
  }
}
</script>

<style scoped lang="scss">
.enemy--can-fight {
  border: 3px solid #FF00FF;
  border-radius: 15px;
  cursor: pointer;
}

.enemy {
  position: relative;
  display: inline-block;
}

.enemy-interactions {
  position: absolute;
  box-sizing: border-box;
  bottom: 58px;
  left: 9px;
  width: calc(100% - 20px);
  display: flex;
  button {
    cursor: pointer;
    flex: 1;
    text-transform: uppercase;
    font-weight: bold;
    padding: 5px 0px;
  }
}

.fight-button {
  border: 0;
  color: #FFF;
  background-color: #8F5B41;
  border-top-left-radius: 7px;
  border-bottom-left-radius: 7px;
  border: 3px solid #FF00FF;
  &:before {
    font-family: "Arkham";
    content: "\0044";
    margin-right: 5px;
  }
}

.evade-button {
  border: 0;
  color: #FFF;
  background-color: #576345;
  border-top-right-radius: 7px;
  border-bottom-right-radius: 7px;
  border: 3px solid #FF00FF;
  border-left: 0;
  &:before {
    font-family: "Arkham";
    content: "\0053";
    margin-right: 5px;
  }
}

.card {
  width: 200px;
}
</style>
