<script lang="ts" setup>
import { useI18n } from 'vue-i18n'
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Difficulty } from '@/arkham/types/Difficulty';
import type { Campaign } from '@/arkham/types/Campaign';
import type { Scenario } from '@/arkham/types/Scenario';
import { imgsrc, campaignBox, scenarioBox } from '@/arkham/helpers';

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
    return `url('${campaignBox(campaign.value.id)}')`
  }

  if (scenario.value) {
    return `url('${scenarioBox(scenario.value.id)}')`
  }

  return null
})
const { t } = useI18n()

/* The one thing worth saying about a badge: that the Campaign Guide itself
 * hedged the ending. */
const outcomeTooltip = computed(() => {
  const o = props.game.outcome
  if (o?.certainty !== 'Uncertain') return null
  return t('gameRow.outcomeUncertain')
})

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
        <div class="game-difficulty">{{difficulty}}</div>
        <div
          v-if="game.outcome"
          class="outcome"
          :class="game.outcome.result === 'Won' ? 'won' : 'lost'"
          v-tooltip="outcomeTooltip"
        >
          {{
              game.outcome.result === 'Lost'
                ? $t('gameRow.lost')
                : game.outcome.certainty === 'Uncertain'
                  ? $t('gameRow.wonUncertain')
                  : $t('gameRow.won')
            }}
        </div>
      </div>
      <div class="game-subdetails">
        <slot></slot>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* A finished row is desaturated, so the badge stays neutral chrome and lets the
 * word carry the verdict. */
.outcome {
  text-transform: uppercase;
  font-size: 0.75em;
  padding: 2px 8px;
  border-radius: 3px;
  background: rgba(255, 255, 255, 0.14);
  border: 1px solid rgba(255, 255, 255, 0.25);
  color: rgba(255, 255, 255, 0.75);
}

/* Still the neutral chrome grey, tinted just enough to be read at a glance in a
   list. The verdict's own color lives on the game-over screen. */
.outcome.won {
  background: color-mix(in srgb, var(--outcome-won) 16%, transparent);
  border-color: color-mix(in srgb, var(--outcome-won) 30%, transparent);
}

.outcome.lost {
  background: color-mix(in srgb, var(--outcome-lost) 16%, transparent);
  border-color: color-mix(in srgb, var(--outcome-lost) 30%, transparent);
}

h2 {
  color: #6E8644;
  font-size: 2em;
  text-transform: uppercase;
}
.game {
  display: flex;
  background-color: #15192C;
  border-left: 10px solid var(--button-1);
  color: #f0f0f0;
  border-radius: 3px;
  margin-bottom: 10px;
  a {
    color: #4a6594;
    font-weight: bolder;
    &:hover {
      color: #5e76a0;
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
  flex: 1;
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
    border: 3px solid var(--survivor);
  }

  &.guardian {
    border: 3px solid var(--guardian);
  }

  &.mystic {
    border: 3px solid var(--mystic);
  }

  &.seeker {
    border: 3px solid var(--seeker);
  }

  &.rogue {
    border: 3px solid var(--rogue);
  }

  &.neutral {
    border: 3px solid var(--neutral);
  }
}

.investigator-portrait {
  width: 150px;
  margin: -18px;
}

.game-subdetails {
  display: flex;
  background: rgba(255,255,255,0.02);
  padding: 10px;
  flex-grow: 1;
  position: relative;
  gap: 10px;
}

.investigators {
  border-radius: 10px;
  display: flex;
  background: rgba(255,255,255,0.02);
  padding: 10px;
  flex: 1;
}

.game-title {
  width: 100%;
  display: flex;
  flex-direction: row;
  align-items: center;
  padding: 10px;
  position: relative;
  * {
    z-index: var(--z-index-1);
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
    z-index: var(--z-index-0);
  }
}

.finished-game {
  border-left: 10px solid #999;
  background: var(--neutral-extra-dark);
  color: #999;

  a {
    color: #494949;
  }

  .campaign-icon {
    filter: invert(28%) sepia(0%) hue-rotate(-180deg) saturate(3);
    max-height: 50px;
  }
}

.other-investigators {
  border-radius: 10px;
  text-transform: uppercase;
  background: rgba(0, 0, 0, 0.5);
  flex: 1;
  display: flex;
  padding: 10px;
  opacity: 0.5;
}

.game-difficulty {
  padding: 5px 15px;
  background: rgba(0, 0, 0, 0.5);
  border-radius: 10px;
  text-transform: uppercase;
}
</style>
