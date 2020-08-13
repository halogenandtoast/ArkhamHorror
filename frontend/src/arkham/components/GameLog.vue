<template>
  <div id="game-log">
    <input type="checkbox" v-model="visible" />
    <transition name="slide">
      <div v-if="visible" class="drawer">
        <div v-for="(entry, index) in entries" :key="index">
          {{entry}}
        </div>
      </div>
    </transition>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { Game } from '@/arkham/types/Game';

@Component
export default class GameLog extends Vue {
  @Prop(Object) readonly game!: Game

  visible = true

  get entries() {
    return this.game.currentData.gameLog;
  }
}
</script>

<style lang="scss">
#game-log {
  display: flex;
  height: 100%;
  top: 0;
  left: 0;
  position: absolute;
  width: 200px;
}

.drawer {
  background: #FFF;
  border: 1px solid #AAA;
  padding: 10px;
  font-size: 0.8em;
  font-weight: 400;
  overflow-y: auto;
  box-sizing: border-box;
  width: 200px;
  padding-left: 10px;
  z-index: -1;
}

.tab {
  position: absolute;
  z-index: 100;
}

.slide-enter-active {
  animation: drawer .3s reverse;
}
.slide-leave-active {
  animation: drawer .3s;
}
@keyframes drawer {
  0% {
    margin-left: 0;
  }
  100% {
    margin-left: -210px;
  }
}
</style>
