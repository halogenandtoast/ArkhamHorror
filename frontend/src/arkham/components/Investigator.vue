<template>
  <div>
    <img
      :class="{ 'investigator--can-interact': investigatorAction !== -1 }"
      class="card"
      :src="image"
      @click="$emit('choose', investigatorAction)"
    />

    <div class="resources">
      <div
        :class="{ 'resource--can-take': takeResourceAction !== -1 }"
        class="poolItem poolItem-resource"
        @click="$emit('choose', takeResourceAction)"
      >
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

@Component
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

    return this.takeDamageAction;
  }

  get runSkillTestAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.BEGIN_SKILL_TEST && c.contents[0] === this.id);
  }

  get searchTopOfDeckAction() {
    return this
      .choices
      .findIndex((c) => c.tag === MessageType.SEARCH_TOP_OF_DECK && c.contents[0] === this.id);
  }

  get takeDamageAction() {
    return this.choices.findIndex((choice) => choice.tag === MessageType.INVESTIGATOR_DAMAGE);
  }

  get takeResourceAction() {
    return this.choices.findIndex((choice) => choice.tag === MessageType.TAKE_RESOURCES);
  }

  get endTurnAction() {
    return this.choices.findIndex((choice) => choice.tag === MessageType.END_TURN);
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

.poolItem {
  position: relative;
  width: 30px;
  height: 40px;
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
  padding-right:8px;
  clip-path: polygon(50% 0%, 100% 25%, 100% 75%, 50% 100%, 0% 75%, 0% 25%);
}

.resource--can-take {
  padding: 0px;
  cursor: pointer;
  background-color: #FF00FF;
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
