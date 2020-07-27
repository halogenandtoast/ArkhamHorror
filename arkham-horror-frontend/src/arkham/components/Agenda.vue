<template>
  <div class="agenda-container">
    <img
      class="card card--sideways"
      :src="agenda.contents[0].image"
    />
    <div v-if="agenda.contents[0].doom">
      <img src="/img/arkham/doom.png"/> {{agenda.contents[0].doom}}
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import * as Arkham from '@/arkham/types/Agenda';

@Component
export default class Agenda extends Vue {
  @Prop(Object) readonly agenda!: Arkham.Agenda;
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

</style>
