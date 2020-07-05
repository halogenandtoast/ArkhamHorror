<template>
  <div>
    <img class="card" :src="player.investigator.image" />

    <div class="resources">
      <div
        v-if="canTakeResources"
        class="poolItem poolItem-resource"
        @click="$emit('takeResource')"
      >
        <img
          class="resource--can-take"
          src="/img/arkham/resource.png"
        />
        {{player.resources}}
      </div>
      <div v-else class="poolItem poolItem-resource">
        <img src="/img/arkham/resource.png" />
        {{player.resources}}
      </div>
      <div class="poolItem"><img src="/img/arkham/clue.png"/> {{player.clues}}</div>
      <div class="poolItem"><img src="/img/arkham/health.png"/> {{player.healthDamage}}</div>
      <div class="poolItem"><img src="/img/arkham/sanity.png"/> {{player.sanityDamage}}</div>
      <p><i class="action" v-for="n in player.actionsRemaining" :key="n"></i></p>
      <button @click="$emit('endTurn')">End turn</button>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { ArkhamPlayer } from '@/arkham/types';

@Component
export default class Investigator extends Vue {
  @Prop(Object) readonly player!: ArkhamPlayer
  @Prop(Boolean) readonly canTakeResources!: boolean
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
  font-size: 1.5em;

  img {
    position: absolute;
    top: 0;
    bottom: 0;
    left: 0;
    right: 0;
    margin: auto;
    z-index: -1;
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

</style>
