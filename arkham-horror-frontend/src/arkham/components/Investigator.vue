<template>
  <div>
    <img class="card" :src="image" />

    <div class="resources">
      <div
        v-if="canTakeActions"
        class="poolItem poolItem-resource"
        @click="$emit('takeResource')"
      >
        <img
          class="resource--can-take"
          src="/img/arkham/resource.png"
        />
        <span>{{player.contents.resources}}</span>
      </div>
      <div v-else class="poolItem poolItem-resource">
        <img src="/img/arkham/resource.png" />
        <span>{{player.contents.resources}}</span>
      </div>
      <div class="poolItem">
        <img src="/img/arkham/clue.png"/>
        <span>{{player.contents.clues}}</span>
      </div>
      <div class="poolItem">
        <img src="/img/arkham/health.png"/>
        <span>{{player.contents.healthDamage}}</span>
      </div>
      <div class="poolItem">
        <img src="/img/arkham/sanity.png"/>
        <span>{{player.contents.sanityDamage}}</span>
      </div>
      <p><i class="action" v-for="n in player.contents.remainingActions" :key="n"></i></p>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import * as Arkham from '@/arkham/types/Investigator';

@Component
export default class Investigator extends Vue {
  @Prop(Object) readonly player!: Arkham.Investigator
  @Prop(Boolean) readonly canTakeActions!: boolean
  @Prop(Boolean) readonly inActionWindow!: boolean

  get image() {
    const { id } = this.player.contents;
    return `/img/arkham/cards/${id}.png`;
  }
}
</script>

<style scoped lang="scss">
i.action {
  font-family: 'Arkham';
  speak: none;
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;

  &:before {
    font-family: "Arkham";
    content: "\0049";
  }
}

.poolItem {
  position: relative;
  width: 57px;
  height: 73px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: black;
  font-weight: 900;
  font-size: 1.7em;

  img {
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
    width: 1.25em;
    height: 1.25em;
    align-items: center;
    justify-content: center;
  }
}

.resources {
  display: flex;
  align-self: center;
  align-items: center;
}

.turn-info {
  display: flex;
  align-self: center;
  align-items: center;
}

.poolItem-resource {
  padding-right:5px;
  clip-path: polygon(50% 0%, 100% 25%, 100% 75%, 50% 100%, 0% 75%, 0% 25%);
}

.resource--can-take {
  padding: 3px;
  cursor: pointer;
  background-color: #FF00FF;
}

.card {
  width: auto;
  height: 250px;
}
</style>
