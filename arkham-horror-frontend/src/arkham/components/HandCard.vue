<template>
  <div>
    <img
      v-if="canPlay"
      class="card playable"
      :src="card.contents.image"
      @click="$emit('playCard')"
    />
    <img
      v-else-if="canCommit"
      :class="['card', 'commitable', { commited: isCommited }]"
      :src="card.contents.image"
      @click="$emit('commitCard')"
    />
    <img v-else class="card" :src="card.contents.image" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { Card } from '@/arkham/types/Card';

@Component
export default class HandCard extends Vue {
  @Prop(Object) readonly card!: Card
  @Prop(Boolean) readonly canPlay!: boolean
  @Prop(Boolean) readonly canCommit!: boolean
  @Prop(Boolean) readonly isCommited!: boolean
}
</script>

<style scoped lang="scss">

.card {
  width: 150px;
  border-radius: 7px;
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 13px;
  margin: 2px;

  &.commited {
    margin-top: -10px;
  }

  &.playable {
    border: 2px solid #ff00ff;
    cursor: pointer;
  }

  &.commitable {
    border: 2px solid #ff00ff;
    cursor: pointer;
  }
}
</style>
