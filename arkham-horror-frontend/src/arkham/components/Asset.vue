<template>
  <div>
    <img :src="asset.image" class="card" />
    <div
      v-if="asset.uses && asset.uses > 0"
      class="poolItem poolItem-resource"
    >
      <img src="/img/arkham/resource.png" />
      {{asset.uses}}
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { ArkhamGame } from '@/arkham/types/game';
import { ArkhamAsset } from '@/arkham/types';

@Component
export default class Asset extends Vue {
  @Prop(Object) readonly game!: ArkhamGame
  @Prop(Object) readonly asset!: ArkhamAsset
}
</script>

<style lang="scss" scoped>
.card {
  width: 200px;
}
</style>
