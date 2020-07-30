<template>
  <div v-if="source" class="modal">
    <div class="modal-contents">
      <img :src="image" />
      <button v-for="(choice, index) in choices" :key="index" @click="$emit('choose', index)">
        {{choice.label}}
      </button>
    </div>
  </div>
</template>

<script lang="ts">
import { Vue, Component, Prop } from 'vue-property-decorator';
import { choicesSource, choices, Game } from '@/arkham/types/Game';

@Component
export default class ChoiceModal extends Vue {
  @Prop(Object) readonly game!: Game;

  get choices() {
    return choices(this.game);
  }

  get source() {
    return choicesSource(this.game);
  }

  get cardCode() {
    if (!this.source) {
      return null;
    }

    const { tag, contents } = this.source;

    if (!contents) {
      return null;
    }

    switch (tag) {
      case 'AgendaSource':
        return `${this.game.currentData.agendas[contents].contents.id}b`;
      default:
        return null;
    }
  }

  get image() {
    if (this.cardCode) {
      return `/img/arkham/cards/${this.cardCode}.jpg`;
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
