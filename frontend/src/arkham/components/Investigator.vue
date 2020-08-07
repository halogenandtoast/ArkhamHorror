<template>
  <div>
    <img
      :class="{ 'investigator--can-interact': investigatorAction !== -1 }"
      class="card"
      :src="image"
      @click="$emit('choose', investigatorAction)"
    />

    <div class="resources">
      <PoolItem
        type="resource"
        :amount="player.contents.resources"
        :class="{ 'resource--can-take': takeResourceAction !== -1 }"
        @choose="$emit('choose', takeResourceAction)"
      />
      <PoolItem
        type="clue"
        :amount="player.contents.clues"
        :class="{ 'resource--can-spend': spendCluesAction !== -1 }"
        @choose="$emit('choose', spendCluesAction)"
      />
      <PoolItem
        type="health"
        :amount="player.contents.healthDamage"
      />
      <PoolItem
        type="sanity"
        :amount="player.contents.sanityDamage"
      />
      <span><i class="action" v-for="n in player.contents.remainingActions" :key="n"></i></span>
      <span v-if="player.contents.tomeActions && player.contents.tomeActions > 0">
        <i class="action tomeAction" v-for="n in player.contents.tomeActions" :key="n"></i>
      </span>
      <button
        :disabled="endTurnAction === -1"
        @click="$emit('choose', endTurnAction)"
      >End turn</button>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import * as Arkham from '@/arkham/types/Investigator';
import { choices, Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import PoolItem from '@/arkham/components/PoolItem.vue';

@Component({
  components: { PoolItem },
})
export default class Investigator extends Vue {
  @Prop(Object) readonly player!: Arkham.Investigator
  @Prop(Object) readonly game!: Game

  get choices() {
    return choices(this.game);
  }

  get id() {
    return this.player.contents.id;
  }

  get investigatorAction() {
    if (this.searchTopOfDeckAction !== -1) {
      return this.searchTopOfDeckAction;
    }

    if (this.runSkillTestAction !== -1) {
      return this.runSkillTestAction;
    }

    if (this.enemyEngageInvestigatorAction !== -1) {
      return this.enemyEngageInvestigatorAction;
    }

    return this.takeDamageAction;
  }

  get runSkillTestAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.BEGIN_SKILL_TEST && c.contents[0] === this.id);
  }

  get enemyEngageInvestigatorAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.ENEMY_ENGAGE_INVESTIGATOR
        && c.contents[1] === this.id);
  }

  get searchTopOfDeckAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.SEARCH_TOP_OF_DECK && c.contents[0] === this.id);
  }

  get takeDamageAction() {
    return this.choices.findIndex((c) => c.tag === MessageType.INVESTIGATOR_DAMAGE);
  }

  get takeResourceAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.TAKE_RESOURCES && c.contents[0] === this.id);
  }

  get spendCluesAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.INVESTIGATOR_SPEND_CLUES
        && c.contents[0] === this.id);
  }

  get endTurnAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.END_TURN && c.contents === this.id);
  }

  get image() {
    const { id } = this.player.contents;
    return `/img/arkham/cards/${id}.jpg`;
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

.investigator--can-interact {
  border: 2px solid #FF00FF;
  cursor: pointer;
}

.card {
  width: auto;
  height: 200px;
}

.tomeAction {
  color: orange;
}
</style>
