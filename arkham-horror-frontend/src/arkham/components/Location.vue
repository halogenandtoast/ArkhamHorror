<template>
  <div>
    <img
      class="card"
      :src="image"
    />
    <div
      v-for="cardCode in location.contents.investigators"
      :key="cardCode"
    >
      <img
        :src="portrait(cardCode)"
        class="portrait"
        width="80"
      />
    </div>
    <div
      v-for="enemyId in location.enemies"
      :key="enemyId"
    >
      <img
        v-if="!game.gameState.enemies[enemyId].isEngaged"
        :src="game.gameState.enemies[enemyId].image"
        width="250"
      />
    </div>
    <div v-if="location.clues > 0" >
      <div
        v-if="canInvestigate"
        class="clue clue--can-investigate"
        @click="investigate(location)"
      >
        <img src="/img/arkham/clue.png" />
        {{location.clues}}
      </div>
      <div v-else>
        <img src="/img/arkham/clue.png" />
        {{location.clues}}
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { Game } from '@/arkham/types/Game';
import * as Arkham from '@/arkham/types/Location';

@Component
export default class Location extends Vue {
  @Prop(Object) readonly game!: Game;
  @Prop(Object) readonly location!: Arkham.Location;

  portrait = (cardCode: string) => `/img/arkham/portraits/${cardCode}.jpg`

  get image() {
    const { id, revealed } = this.location.contents;
    const suffix = revealed ? '' : 'b';

    return `/img/arkham/cards/${id}${suffix}.png`;
  }
}
</script>
