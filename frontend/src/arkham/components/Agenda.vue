<template>
  <div class="agenda-container">
    <img
      class="card card--sideways"
      :src="image"
    />
    <div v-if="agenda.contents.doom" class="poolItem">
      <img src="/img/arkham/doom.png"/>
      <span>{{agenda.contents.doom}}</span>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import * as Arkham from '@/arkham/types/Agenda';

@Component
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

.poolItem {
  position: relative;
  width: 30px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: black;
  font-weight: 900;
  font-size: 1.7em;

  img {
    width: 100%;
    position: absolute;
    top: 0;
    bottom: 0;
    left: 0;
    right: 0;
    margin: auto;
  }

  span {
    font-family: "Arkham";
    display: flex;
    position: relative;
    background: rgba(255,255,255,0.5);
    border-radius: 20px;
    font-size: 0.8em;
    width: 1.05em;
    height: 1.05em;
    align-items: center;
    justify-content: center;
  }
}
</style>
