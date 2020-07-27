<template>
  <footer>
    <p>Status bar</p>
  </footer>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ArkhamGame, ArkhamStepTypes } from '@/arkham/types/game';

@Component
export default class StatusBar extends Vue {
  @Prop(Object) readonly game!: ArkhamGame
}
</script>
