<template>
  <footer>
    <div v-if="afterDiscoverCluesAction !== -1">
      <p>You got some clues</p>
      <button @click="$emit('choose', afterDiscoverCluesAction)">Continue</button>
    </div>

    <div v-if="continueAction !== -1">
      <button @click="$emit('choose', continueAction)">{{continueLabel}}</button>
    </div>
  </footer>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';

@Component
export default class StatusBar extends Vue {
  @Prop(Object) readonly game!: Game

  get choices() {
    return choices(this.game);
  }

  get afterDiscoverCluesAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.AFTER_DISCOVER_CLUES);
  }

  get continueAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.CONTINUE);
  }

  get continueLabel() {
    if (this.continueAction !== -1) {
      return this.choices[this.continueAction].contents;
    }

    return '';
  }
}
</script>
