<template>
  <footer>
    <div v-if="afterDiscoverCluesAction !== -1">
      <p>You got some clues</p>
      <button @click="$emit('choose', afterDiscoverCluesAction)">Continue</button>
    </div>
  </footer>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';

@Component
export default class StatusBar extends Vue {
  @Prop(Object) readonly game!: Game

  get choices() {
    return this.game.currentData.question.contents;
  }

  get afterDiscoverCluesAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.AFTER_DISCOVER_CLUES);
  }
}
</script>
