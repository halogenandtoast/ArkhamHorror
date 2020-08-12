<template>
  <div class="agenda-container">
    <img
      :class="{ 'agenda--can-progress': advanceAgendaAction !== -1 }"
      class="card card--sideways"
      @click="$emit('choose', advanceAgendaAction)"
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
import { choices, Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Agenda';

@Component({
  components: { PoolItem },
})
export default class Agenda extends Vue {
  @Prop(Object) readonly agenda!: Arkham.Agenda;
  @Prop(Object) readonly game!: Game;
  @Prop(String) readonly investigatorId!: string;

  get id() {
    return this.agenda.contents.id;
  }

  get image() {
    if (this.agenda.contents.flipped) {
      return `/img/arkham/cards/${this.id}b.jpg`;
    }

    return `/img/arkham/cards/${this.id}.jpg`;
  }

  get choices() {
    return choices(this.game, this.investigatorId);
  }

  get advanceAgendaAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.ADVANCE_AGENDA);
  }
}
</script>

<style scoped lang="scss">
.card {
  width: 100px;
  -webkit-box-shadow: 0 3px 6px rgba(0, 0, 0, 0.23), 0 3px 6px rgba(0, 0, 0, 0.53);
  box-shadow: 0 3px 6px rgba(0, 0, 0, 0.23), 0 3px 6px rgba(0, 0, 0, 0.53);
  border-radius: 6px;
  margin: 2px;
}

.card--sideways {
  width: auto;
  height: 100px;
}

.agenda--can-progress {
  border: 3px solid #ff00ff;
  border-radius: 8px;
  cursor: pointer;
}
</style>
