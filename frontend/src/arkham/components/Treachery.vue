<template>
  <div>
    <img :src="image" class="card" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { Game } from '@/arkham/types/Game';
import * as Arkham from '@/arkham/types/Treachery';

@Component
export default class Asset extends Vue {
  @Prop(Object) readonly game!: Game
  @Prop(Object) readonly treachery!: Arkham.Treachery

  get image() {
    return `/img/arkham/cards/${this.treachery.contents.cardCode}.jpg`;
  }
}
</script>

<style lang="scss" scoped>
.card {
  width: 200px;
}
</style>
