<script lang="ts" setup>
import { computed, ref } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Difficulty } from '@/arkham/types/Difficulty';
import type { Campaign } from '@/arkham/types/Campaign';
import type { Scenario } from '@/arkham/types/Scenario';
import { imgsrc } from '@/arkham/helpers';

export interface Props {
  game: Game
  deleteGame: () => void
}

const props = defineProps<Props>()
const campaign = computed<Campaign | null>(() => props.game.campaign)
const scenario = computed<Scenario | null>(() => props.game.scenario)
const deleting = ref(false)

const difficulty = computed<Difficulty>(() => {
  if (campaign.value) {
    return campaign.value.difficulty
  }

  if (scenario.value) {
    return scenario.value.difficulty
  }

  return 'Easy'
})

const currentHeading = computed(() => {
  if (campaign.value?.meta?.currentCampaignMode) {
    return campaign.value?.meta.currentCampaignMode === "TheWebOfDreams" ? "The Web of Dreams" : "The Dream-Quest"
  }

  return null
})

const otherHeading = computed(() => {
  if (currentHeading.value) {
    return currentHeading.value === "The Web of Dreams" ? "The Dream-Quest" : "The Web of Dreams" 
  }

  return null
})

// const box = computed(() => {
//   if (campaign.value) {
//     return `url('${imgsrc(`boxes/${campaign.value.id}.jpg`)}')`
//   }
// 
//   if (scenario.value) {
//     return `url('${imgsrc(`boxes/${scenario.value.id.replace(/^c/, '')}.jpg`)}')`
//   }
// 
//   return null
// })

const toCssName = (s: string): string => s.charAt(0).toLowerCase() + s.substring(1)
</script>

<template>
  <div class="game" :class="{ 'finished-game': game.gameState.tag == 'IsOver' }">
    <div class="game-details">
      <div class="game-title">
        <div class="main-details">
          <div class="campaign-icon-container" v-if="campaign">
            <img class="campaign-icon" :src="imgsrc(`sets/${campaign.id}.png`)" />
          </div>
          <div class="campaign-icon-container" v-else-if="scenario">
            <img class="campaign-icon" :src="imgsrc(`sets/${scenario.id.replace('c', '')}.png`)" />
          </div>
          <router-link class="title" :to="`/games/${game.id}`">{{game.name}}</router-link>
        </div>
        <div v-if="campaign && scenario" class="scenario-details">
          <img class="scenario-icon" :src="imgsrc(`sets/${scenario.id.replace('c', '')}.png`)" />
          <span>{{scenario.name.title}}</span>
        </div>
        <div class="extra-details">
          <div class="game-difficulty">{{difficulty}}</div>

          <div class="game-delete">
              <transition name="slide">
                <a v-show="!deleting" href="#delete" @click.prevent="deleting = true"><font-awesome-icon icon="trash" /></a>
              </transition>
              <transition name="slide">
                <div class="delete-buttons" v-show="deleting">
                  <button href="#delete" @click.prevent="deleting = false">Cancel</button>
                  <button class="delete-button" href="#delete" @click.prevent="deleteGame">Delete</button>
                </div>
              </transition>
            </div>
        </div>
      </div>
      <div class="game-subdetails">
        <div class="current-subdetails">
          <h2 v-if="currentHeading">{{currentHeading}}</h2>
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
          </div>
        </div>
        <div v-if="Object.keys(game.otherInvestigators).length > 0" class="other-subdetails">
          <h2 v-if="otherHeading">{{otherHeading}}</h2>
          <div class="other-investigators">
            <div
              v-for="investigator in game.otherInvestigators"
              :key="investigator.id"
              class="investigator"
            >
              <div :class="`investigator-portrait-container ${toCssName(investigator.class)}`">
                <img :src="imgsrc(`cards/${investigator.id.replace('c', '')}.jpg`)" class="investigator-portrait"/>
              </div>
            </div>
          </div>
        </div>
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
  color: #cecece;
  background-color: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 3px;
  margin-bottom: 10px;
  a {
    color: var(--title);
    font-weight: bolder;
    &:hover {
      color: lighten(#365488, 20%);
    }
  }
}

.campaign-icon-container {
  display: flex;
  align-items: center;
  margin-right: 20px;
  margin-left: 10px;
  width: 50px;
}

.campaign-icon {
  //filter: invert(28%) sepia(100%) hue-rotate(-180deg) saturate(3);
  filter: invert(100%) brightness(85%);
  max-height: 50px;
  width: 100%;
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
  transition: all 0.5s;
  margin: 0 10px;
  position: relative;
  align-self: center;
  display: flex;
  a {
    font-size: 1.2em;
    color: #660000;
    &:hover {
      color: #990000;
    }
  }
}

.scenario-details {
  justify-content: flex-end;
  display: flex;
  margin-left: auto;
  padding: 5px 10px;
  margin-left: 10px;
  margin-right: 10px;
  border-radius: 10px;
  align-items: center;
  span {
    line-height: 25px;
  }

  @media (max-width: 600px) {
    margin: 0;
    padding: 0;
    justify-content: flex-start;
    img {
      margin: 0;
      margin-right: 10px;
    }
  }
}

.title {
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 1.6em;
  text-decoration: none;
  a {
    text-decoration: none;
  }

  &:hover {
    color: #cecece;
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
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);

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

.game-subdetails {
  display: flex;
  background: rgba(255,255,255,0.02);
  flex-grow: 1;
  position: relative;

  h2 {
    margin: 0;
    padding: 0;
  }
}

.current-subdetails {
  display: flex;
  flex-direction: column;
  background: rgba(255,255,255,0.02);
  flex: 1;
  position: relative;
  gap: 10px;

  h2 {
    color: var(--title);
    font-size: 1em;
    margin: 0;
    padding: 0;
    background: var(--background-dark);
    padding: 2px 5px;
  }
}

.investigators {
  border-radius: 10px;
  display: flex;
  padding: 10px;
  flex: 1;
}



.main-details, .extra-details {
  display: flex;
  align-items: center;
}

.main-details {
  flex: 1;
}

.game-title {
  display: flex;
  flex-direction: row;
  align-items: center;
  padding: 10px;
  box-sizing: border-box;
  position: relative;
  border-bottom: 1px solid var(--box-border);

  @media (max-width: 600px) {
    display: flex;
    flex-direction: column;
    font-size: 0.8em;
    align-items: flex-start;
    img { width: 20px; height: auto; }
    gap: 10px;
  }

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
    //background-image: v-bind(box);
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

.other-subdetails {
  text-transform: uppercase;
  display: flex;
  flex: 1;
  flex-direction: column;
  background: var(--box-background);

  h2 {
    background: rgba(255,255,255,0.02);
    color: var(--title);
    font-size: 1em;
    margin: 0;
    padding: 0;
    padding: 2px 5px;
  }
}

.other-investigators {
  background: var(--background-dark);
  display: flex;
  padding: 10px;
  flex: 1;
}

.game-difficulty {
  padding: 5px 15px;
  background: rgba(0, 0, 0, 0.5);
  border-radius: 10px;
  text-transform: uppercase;
  margin-right: 10px;
}

.delete-buttons {
  display: flex;
  gap: 5px;

  button {
    border: 0;
    padding: 5px 10px;
    border-radius: 3px;
  }

  .delete-button {
    background-color: #660000;
  }
}

.slide-leave-active ,
.slide-enter-active {
  transition: all 0.3s linear;
}

.slide-enter-active {
  transition-delay: 0.2s;
}

.slide-enter-to,
.slide-leave-from {
  max-width: 1000px;
  opacity: 1;
}

.slide-enter-from, .slide-leave-to {
  overflow: hidden;
  max-width: 0;
  opacity: 0;
}
</style>
