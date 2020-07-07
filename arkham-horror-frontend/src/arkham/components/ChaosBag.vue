<template>
  <div>
    <img v-if="drawnToken" :src="chaosTokenSrc" class="token" />
    <img
      v-else-if="canDrawToken"
      class="token token--can-draw"
      src="/img/arkham/ct_blank.png"
      @click="$emit('drawToken')"
    />
    <img v-else class="token" src="/img/arkham/ct_blank.png" />
    <div v-if="canApplyResult">
      <p>
        Difficulty: {{skillDifficulty}},
        Modified Skill: {{skillModifiedSkillValue}},
        Pending Result: {{pendingResult}}
      </p>
      <button @click="$emit('applyTokenResult')">Apply Result</button>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';

@Component
export default class ChaosBag extends Vue {
  @Prop(String) readonly drawnToken!: string;
  @Prop(Boolean) readonly canDrawToken!: boolean;
  @Prop(Boolean) readonly canApplyResult!: boolean;
  @Prop(Number) readonly skillDifficulty!: number;
  @Prop(Number) readonly skillModifiedSkillValue!: number;
  @Prop(String) readonly pendingResult!: string;

  get chaosTokenSrc() {
    return `/img/arkham/ct_${this.drawnToken}.png`;
  }
}
</script>

<style scoped lang="scss">
.token--can-draw {
  border: 5px solid #ff00ff;
  border-radius: 500px;
}

.token {
  width: 150px;
  height: auto;
}

</style>
