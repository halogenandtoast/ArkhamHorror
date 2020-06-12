<template>
  <div id="game" class="game">
    <div>{{game.cycle}} -  {{game.scenario}}</div>

    <Player :player="game.player" />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ArkhamGame } from '@/arkham/types/ArkhamGame';
import Player from '@/arkham/components/Player.vue';

@Component({
  components: { Player },
})
export default class Scenario extends Vue {
  @Prop(Object) readonly game!: ArkhamGame;
}
</script>
