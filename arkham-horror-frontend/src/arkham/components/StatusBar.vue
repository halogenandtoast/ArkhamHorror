<template>
  <footer>
    <p>Status bar</p>
  </footer>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { Game } from '@/arkham/types/Game';

@Component
export default class StatusBar extends Vue {
  @Prop(Object) readonly game!: Game
}
</script>
