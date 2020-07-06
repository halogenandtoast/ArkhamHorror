<template>
  <div class="act-container">
    <img
      v-if="act.contents[0].canProgress"
      class="card card--sideways act--can-progress"
      @click="$emit('progressAct')"
      :src="act.contents[0].image"
    />
    <img
      v-else
      class="card card--sideways"
      :src="act.contents[0].image"
    />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ActStack } from '@/arkham/types/game';

@Component
export default class Act extends Vue {
  @Prop(Object) readonly act!: ActStack;
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
