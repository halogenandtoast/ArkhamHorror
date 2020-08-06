<template>
  <div class="agenda-container">
    <img
      class="card card--sideways"
      :src="image"
    />
    <PoolItem
      type="doom"
      :amount="agenda.contents.doom"
    />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Agenda';

@Component({
  components: { PoolItem },
})
export default class Agenda extends Vue {
  @Prop(Object) readonly agenda!: Arkham.Agenda;

  get image() {
    const { id } = this.agenda.contents;
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
  height: 200px;
}
</style>
