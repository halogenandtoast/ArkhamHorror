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
import { Game } from '@/arkham/types/Game';

@Component
export default class ChoiceModal extends Vue {
  @Prop(Object) readonly game!: Game;
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
