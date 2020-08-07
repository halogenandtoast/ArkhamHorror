<template lang="html">
  <div class='tab' v-show='isActive'>
    <slot></slot>
  </div>
</template>

<script lang="ts">
import { Vue, Prop, Component } from 'vue-property-decorator';

@Component
export default class PlayerTab extends Vue {
  @Prop(String) readonly title!: string
  @Prop(Boolean) readonly activePlayer!: boolean

  isActive = true
}
</script>
