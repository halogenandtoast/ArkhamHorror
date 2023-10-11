<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Difficulty } from '@/arkham/types/Difficulty';
import type { Campaign } from '@/arkham/types/Campaign';
import type { Scenario } from '@/arkham/types/Scenario';
import { imgsrc } from '@/arkham/helpers';

export interface Props {
  game: Game
}

const props = defineProps<Props>()
const campaign = computed<Campaign | null>(() => props.game.campaign)
const scenario = computed<Scenario | null>(() => props.game.scenario)

const difficulty = computed<Difficulty>(() => {
  if (campaign.value) {
    return campaign.value.difficulty
  }

  if (scenario.value) {
    return scenario.value.difficulty
  }

  return 'Easy'
})

const box = computed(() => {
  if (campaign.value) {
    return `url('${imgsrc(`boxes/${campaign.value.id}.jpg`)}')`
  }

  if (scenario.value) {
    return `url('${imgsrc(`boxes/${scenario.value.id.replace(/^c/, '')}.jpg`)}')`
  }

  return null
})

const toCssName = (s: string): string => s.charAt(0).toLowerCase() + s.substring(1)
</script>

<template>
  <div class="game" :class="{ 'finished-game': game.gameState.tag == 'IsOver' }">
    <div class="game-details">
      <div class="game-title">
        <div class="campaign-icon-container" v-if="campaign">
          <img class="campaign-icon" :src="imgsrc(`sets/${campaign.id}.png`)" />
        </div>
        <div class="campaign-icon-container" v-else-if="scenario">
          <img class="campaign-icon" :src="imgsrc(`sets/${scenario.id.replace('c', '').slice(0,2)}.png`)" />
        </div>
        <router-link class="title" :to="`/games/${game.id}`">{{game.name}}</router-link>
        <div v-if="scenario" class="scenario-details">
          <img class="scenario-icon" :src="imgsrc(`sets/${scenario.id.replace('c', '')}.png`)" />
          <span>{{scenario.name.title}}</span>
        </div>

        <div class="game-delete">
          <a href="#delete" @click.prevent="$emit('delete')"><font-awesome-icon icon="trash" /></a>
        </div>
      </div>
      <div class="investigators">
        <div
          v-for="investigator in game.investigators"
          :key="investigator.id"
          class="investigator"
        >
          <div :class="`investigator-portrait-container ${toCssName(investigator.class)}`">
            <img :src="imgsrc(`cards/${investigator.id.replace('c', '')}.jpg`)" class="investigator-portrait"/>
          </div>
        </div>
        <div class="game-difficulty">{{difficulty}}</div>
      </div>
    </div>
  </div>
</template>

<style lang="scss" scoped>
h2 {
  color: #6E8644;
  font-size: 2em;
  text-transform: uppercase;
}
.game {
  display: flex;
  background-color: #15192C;
  border-left: 10px solid #6e8640;
  color: #f0f0f0;
  border-radius: 3px;
  margin-bottom: 10px;
  a {
    color: lighten(#365488, 10%);
    font-weight: bolder;
    &:hover {
      color: lighten(#365488, 20%);
    }
  }
}

.campaign-icon-container {
  display: flex;
  align-items: center;
  margin-right: 10px;
}

.campaign-icon {
  filter: invert(28%) sepia(100%) hue-rotate(-180deg) saturate(3);
  max-height: 50px;
}

.scenario-icon {
  height: 30px;
  margin: 0 20px 0 10px;
  filter: invert(100%);
}

.game-details {
  flex: 1;
}

.game-delete {
  margin-left: auto;
  align-self: flex-start;
  a {
    font-size: 1.2em;
    color: #660000;
    &:hover {
      color: #990000;
    }
  }
}

.scenario-details {
  display: flex;
  background-color: rgba(255, 255, 255, 0.1);
  padding: 5px 10px;
  margin-left: 10px;
  margin-right: 10px;
  border-radius: 10px;
  align-items: center;
  flex: 1;
  span {
    line-height: 25px;
  }
}

.title {
  font-family: teutonic, sans-serif;
  font-size: 1.6em;
  a {
    text-decoration: none;
  }
}

.investigator {
  display: inline;
  padding: 5px;
  border-radius: 10px;
}

.investigator-portrait-container {
  width: 50px;
  height:50px;
  overflow: hidden;
  border-radius: 5px;
  margin-right: 10px;

  &.survivor {
    border: 3px solid $survivor;
  }

  &.guardian {
    border: 3px solid $guardian;
  }

  &.mystic {
    border: 3px solid $mystic;
  }

  &.seeker {
    border: 3px solid $seeker;
  }

  &.rogue {
    border: 3px solid $rogue;
  }

  &.neutral {
    border: 3px solid $neutral;
  }
}

.investigator-portrait {
  width: 150px;
  margin: -18px;
}

.investigators {
  display: flex;
  background: rgba(255,255,255,0.02);
  padding: 10px;
}

.game-title {
  display: flex;
  flex-direction: row;
  align-items: center;
  padding: 10px;
  box-sizing: border-box;
  position: relative;
  * {
    z-index: 1;
  }

  &:before {
    content: ' ';
    display: block;
    position: absolute;
    left: 0;
    top: 0;
    width: 100%;
    height: 100%;
    opacity: 0.1;
    background-image: v-bind(box);
    background-repeat: no-repeat;
    background-position: center;
    background-size: cover;
    z-index: 0;
  }
}

.finished-game {
  border-left: 10px solid #999;
  background: #222;
  color: #999;

  a {
    color: #494949;
  }

  .campaign-icon {
    filter: invert(28%) sepia(0%) hue-rotate(-180deg) saturate(3);
    max-height: 50px;
  }
}

.game-difficulty {
  margin-left: auto;
  align-self: flex-end;
  justify-self: flex-end;
  padding: 5px 15px;
  background: rgba(0, 0, 0, 0.5);
  border-radius: 10px;
  text-transform: uppercase;
}
</style>
