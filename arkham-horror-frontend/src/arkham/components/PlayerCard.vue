<template>
  <div>
    <img :src="card.contents.image" class="card" />
    <div
      v-if="card.contents.uses && card.contents.uses > 0"
      class="poolItem poolItem-resource"
    >
      <img src="/img/arkham/resource.png" />
      {{card.contents.uses}}
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { ArkhamGame } from '@/arkham/types/game';
import { ArkhamCard } from '@/arkham/types/card';

@Component
export default class PlayerCard extends Vue {
  @Prop(Object) readonly game!: ArkhamGame
  @Prop(Object) readonly card!: ArkhamCard
}
</script>
