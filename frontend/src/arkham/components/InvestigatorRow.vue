<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { ref, computed } from 'vue';
import { Investigator } from '@/arkham/types/Investigator';
import Card from '@/arkham/components/Card.vue'
import {imgsrc} from '@/arkham/helpers'

export interface Props {
  investigator: Investigator
  game: Arkham.Game
  bonusXp?: number | null;
}

const props = withDefaults(defineProps<Props>(), {
  bonusXp: null
})

const expanded = ref(false)

const storyCards = computed(() => props.game.campaign.storyCards[props.investigator.id] || [])

</script>

<template>
  <div class="investigator" :class="{[investigator.class.toLowerCase()]: true}">
    <div class="basic">
      <div :class="`investigator-portrait-container ${investigator.class.toLowerCase()}`">
        <img :src="imgsrc(`portraits/${investigator.id.replace('c', '')}.jpg`)" class="investigator-portrait"/>
      </div>
      <div class="name">{{investigator.name.title}}</div>
      <section class="details">
        <svg v-tooltip="'Physical Trauma'" v-for="n in investigator.physicalTrauma" :key="n" class="icon icon-health"><use xlink:href="#icon-health"></use></svg>
        <svg v-tooltip="'Mental Trauma'" v-for="n in investigator.mentalTrauma" :key="n" class="icon icon-sanity"><use xlink:href="#icon-sanity"></use></svg>
      </section>
      <section class="expand" @click="expanded = !expanded">
        <svg class="icon icon-expand" :class="{ expanded }"><use xlink:href="#icon-right-arrow"></use></svg>
      </section>
    </div>
    <div v-if="expanded" class="expanded-details">
      <div><strong>Total XP:</strong> {{investigator.xp}}<span v-if="bonusXp" class="bonus-xp"> ({{bonusXp}} unspendable)</span></div>
      <div><strong>Physical Trauma:</strong> {{investigator.physicalTrauma}}</div>
      <div><strong>Mental Trauma:</strong> {{investigator.mentalTrauma}}</div>
      <section v-if="storyCards.length > 0" class="inner-section">
        <h2>Earned Cards</h2>
        <section class='earned-cards'>
          <div v-for="card in storyCards">
            <Card :game="game" :card="card" :playerId="investigator.id" />
          </div>
        </section>
      </section>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.investigator {
  width: 80%;
  border-radius: 15px;
  &.guardian {
    background: var(--guardian-extra-dark);
  }

  &.seeker {
    background: var(--seeker-extra-dark);
  }

  &.rogue {
    background: var(--rogue-extra-dark);
  }

  &.mystic {
    background: var(--mystic-extra-dark);
  }

  &.survivor {
    background: var(--survivor-extra-dark);
  }

  &.neutral {
    background: var(--neutral-extra-dark);
  }
}
.basic {
  font-size: 1em;
  align-items: center;
  min-width: 200px;
  display: flex;
  flex-direction: row;
  gap: 20px;
  flex: 1;
  color: white;
  padding: 10px;

  .name {
    font-size: 1.5em;
    font-family: serif;
  }
  .portrait {
    width: 25%;
    max-width: 150px;
    border-radius: 10px;
    box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  }

}
.investigator-portrait-container {
  width: 50px;
  height:50px;
  overflow: hidden;
  border-radius: 5px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);

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
}

.details {
  flex: 1;
  text-align: right;
}

.icon {
  display: inline-block;
  width: 1.5em;
  height: 1.5em;
  stroke-width: 0;
  stroke: currentColor;
  fill: currentColor;
}

.expand {
  cursor: pointer;
}

svg {
  transform: rotate(0deg);
  transition: transform 0.3s ease;
}

svg.expanded {
  transform: rotate(90deg);
}

.expanded-details {
  padding: 10px;
  color: rgba(255, 255, 255, 0.8);
  display: flex;
  flex-direction: column;
  gap: 10px;
  strong {
    text-transform: uppercase;
  }

  h2 {
    font-family: "Teutonic", sans-serif;
    font-size: 1.8em;
  }
}

.inner-section {
  background: rgba(0, 0, 0, 0.5);
  padding: 10px;
  border-radius: 10px;

  :deep(.card) {
    width: 10vw;
  }
}

.earned-cards {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  gap: 10px;
}

.bonus-xp {
  font-size: 0.8em;
}
</style>
