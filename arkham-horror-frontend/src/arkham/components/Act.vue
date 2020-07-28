<template>
  <div class="act-container">
    <img
      v-if="act.contents.canAdvance"
      class="card card--sideways act--can-progress"
      @click="$emit('progressAct')"
      :src="image"
    />
    <img
      v-else
      class="card card--sideways"
      :src="image"
    />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import * as Arkham from '@/arkham/types/Act';

@Component
export default class Act extends Vue {
  @Prop(Object) readonly act!: Arkham.Act;

  get image() {
    const { id } = this.act.contents;
    return `/img/arkham/cards/${id}.jpg`;
  }
}
</script>

<style scoped lang="scss">
.card {
  width: 250px;
  -webkit-box-shadow: 0 3px 6px rgba(0, 0, 0, 0.23), 0 3px 6px rgba(0, 0, 0, 0.53);
  box-shadow: 0 3px 6px rgba(0, 0, 0, 0.23), 0 3px 6px rgba(0, 0, 0, 0.53);
  border-radius: 13px;
  margin: 2px;
}

.card--sideways {
  width: auto;
  height: 250px;
}

.act--can-progress {
  border: 5px solid #ff00ff;
  border-radius: 20px;
}

</style>
