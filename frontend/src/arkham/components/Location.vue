<template>
  <div class="location-container">
    <div class="location-investigator-column">
      <div
        v-for="cardCode in location.contents.investigators"
        :key="cardCode"
      >
        <img
          :src="portrait(cardCode)"
          class="portrait"
        />
      </div>
    </div>
    <div class="location-column">
      <font-awesome-icon v-if="blocked" :icon="['fab', 'expeditedssl']" class="status-icon" />

      <div
        :class="{ 'location--can-interact': cardAction !== -1 }"
        class="card-container"
        @click="$emit('choose', cardAction)">
        <div
          class="card location-card"
          :style="{ backgroundImage: `url(${image})` }"
          ></div>
        <div
          class="card location-connections"
          :style="{ backgroundImage: `url(${image})` }"
          ></div>
      </div>
      <button
        v-if="investigateAction !== -1"
        class="button investigate-button"
        @click="doInvestigate"
      >Investigate</button>
      <button
        v-for="ability in abilities"
        :key="ability"
        :class="{button: true, 'ability-button': singleAction(ability), 'double-ability-button': doubleAction(ability), 'fast-ability-button': fastAction(ability) }"
        @click="$emit('choose', ability)"
        >{{abilityLabel(ability)}}</button>
      <Treachery
        v-for="treacheryId in location.contents.treacheries"
        :key="treacheryId"
        :treachery="game.currentData.treacheries[treacheryId]"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
      <div v-if="location.contents.clues > 0" class="pool">
        <PoolItem type="clue" :amount="location.contents.clues" />
      </div>
    </div>
    <div class="location-asset-column">
      <Asset
        v-for="assetId in location.contents.assets"
        :asset="game.currentData.assets[assetId]"
        :game="game"
        :investigatorId="investigatorId"
        :key="assetId"
        @choose="$emit('choose', $event)"
      />
      <Enemy
        v-for="enemyId in enemies"
        :key="enemyId"
        :enemy="game.currentData.enemies[enemyId]"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import { Cost } from '@/arkham/types/Cost';
import * as ArkhamGame from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import Enemy from '@/arkham/components/Enemy.vue';
import Asset from '@/arkham/components/Asset.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Location';

export default defineComponent({
  components: {
    Enemy,
    Treachery,
    Asset,
    PoolItem,
  },
  props: {
    game: { type: Object as () => Game, required: true },
    location: { type: Object as () => Arkham.Location, required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props, { emit }) {
    const clues = computed(() => props.location.contents.clues)
    const image = computed(() => {
      const { id, revealed } = props.location.contents;
      const suffix = revealed ? '' : 'b';

      return `/img/arkham/cards/${id}${suffix}.jpg`;
    })

    const id = computed(() => props.location.contents.id)
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    const attachTreacheryToLocationAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.ATTACH_TREACHERY
          && c.contents[1].contents === id.value);
    })

    const enemySpawnAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.ENEMY_SPAWN
          && c.contents[1] === id.value);
    })

    const moveToAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.MOVE_TO && c.contents[1] === id.value);
    })

    const moveUntilAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.MOVE_UNTIL && c.contents[0] === id.value);
    })

    const createEnemyAtAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.CREATE_ENEMY_AT && c.contents[1] === id.value);
    })

    const moveAction = computed(() => {
      const isRunMove = choices.value.findIndex((c) => c.tag === MessageType.RUN
        && c.contents[0]
        && c.contents[0].tag === MessageType.MOVE
        && c.contents[0].contents[1] === id.value);

      if (isRunMove !== -1) {
        return isRunMove;
      }

      return choices
        .value
        .findIndex((c) => c.tag === MessageType.MOVE && c.contents[1] === id.value);
    })

    const cardAction = computed(() => {
      if (attachTreacheryToLocationAction.value !== -1) {
        return attachTreacheryToLocationAction.value;
      }

      if (enemySpawnAction.value !== -1) {
        return enemySpawnAction.value;
      }

      if (moveToAction.value !== -1) {
        return moveToAction.value;
      }

      if (moveUntilAction.value !== -1) {
        return moveUntilAction.value;
      }

      if (createEnemyAtAction.value !== -1) {
        return createEnemyAtAction.value;
      }

      return moveAction.value;
    })

    const investigateAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.INVESTIGATE && c.contents[1] === id.value);
    })

    function abilityLabel(idx: number) {
      return choices.value[idx].contents[1].type.contents[0];
    }

    const abilities = computed(() => {
      return choices
        .value
        .reduce<number[]>((acc, v, i) => {
          if ((v.tag === 'ActivateCardAbilityAction' || v.tag === 'ActivateCardAbilityActionWithDynamicCost') && v.contents[1].source.tag === 'LocationSource' && v.contents[1].source.contents === id.value) {
            return [i, ...acc];
          }

          if (v.tag === 'ActivateCardAbilityAction' && v.contents[1].source.tag === 'ProxySource' && v.contents[1].source.contents[0].tag === 'LocationSource' && v.contents[1].source.contents[0].contents === id.value) {
            return [...acc, i];
          }

          return acc;
        }, []);
    })

    const enemies = computed(() => {
      const enemyIds = props.location.contents.enemies;
      return enemyIds
        .filter((e) => props.game.currentData.enemies[e].contents.engagedInvestigators.length === 0);
    })

    const blocked = computed(() => props.location.modifiers.some(modifier => modifier.type.tag == "Blocked"))

    function warnAction(msg: string, action: number) {
      if (window.confirm(msg)) { // eslint-disable-line
        emit('choose', action);
      }
    }

    function doInvestigate() {
      if (clues.value === 0) {
        warnAction('There are no clues left, are you sure?', investigateAction.value);
      } else {
        emit('choose', investigateAction.value);
      }
    }

    const portrait = (cardCode: string) => `/img/arkham/portraits/${cardCode}.jpg`

    function singleAction(idx: number) {
      if (choices.value[idx].contents[1].type.tag !== "ActionAbility") {
        return false
      }
      const { contents } = choices.value[idx].contents[1].type.contents[1]
      if (typeof contents.some == 'function') {
        return contents.some((cost: Cost) => cost.tag == "ActionCost" && cost.contents == 1)
      } else {
        return contents === 1
      }
    }

    function doubleAction(idx: number) {
      if (choices.value[idx].contents[1].type.tag !== "ActionAbility") {
        return false
      }
      const { contents } = choices.value[idx].contents[1].type.contents[1]
      if (typeof contents.some == 'function') {
        return contents.some((cost: Cost) => cost.tag == "ActionCost" && cost.contents == 2)
      } else {
        return contents === 2
      }
    }

    function fastAction(idx: number) {
      return choices.value[idx].contents[1].type.tag === "FastAbility"
    }

    return {
      portrait,
      doInvestigate,
      blocked,
      enemies,
      abilities,
      abilityLabel,
      investigateAction,
      cardAction,
      image,
      singleAction,
      doubleAction,
      fastAction
    }
  }
})
</script>

<style scoped lang="scss">
.location--can-interact {
  border: 3px solid #FF00FF;
  cursor: pointer;
}

.card {
  width: 100px;
  border-radius: 3px;
}

::v-deep .enemy {
  width: 80px;
}

::v-deep .treachery {
  object-fit: cover;
  object-position: 0 -74px;
  height: 68px;
  margin-top: 2px;
}

.portrait {
  border-radius: 3px;
  width: 60px;
  margin-right: 2px;
}

.location-container {
  display: flex;
  margin: 0 5px;
  min-width: 187px;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.investigate-button {
  background-color: #40263A;
  &:before {
    font-family: "arkham";
    content: "\0046";
    margin-right: 5px;
  }
}

.ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0049";
    margin-right: 5px;
  }
}

.double-ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0049\0049";
    margin-right: 5px;
  }
}

.fast-ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0075";
    margin-right: 5px;
  }
}

.location-column {
  display: flex;
  flex-direction: column;
  position: relative;
}

.pool {
  display: flex;
  flex-direction: row;
  height: 2em;
}

.status-icon {
  align-self: center;
  background: rgba(255, 255, 255, 0.7);
  border-radius: 1.5em;
  font-size: 2.6em;
  color: rgba(0, 0, 0, 0.8);
  position: absolute;
  top: 19px;
  pointer-events: none;
}

.location-card {
  height: 107px;
  width: 125px;
  background-size: 100%;
  border-bottom-left-radius: 0;
  border-bottom-right-radius: 0;
}

.location-connections {
  height: 27px;
  width: 125px;
  background-size: 100%;
  background-position: bottom;
  border-top-left-radius: 0;
  border-top-right-radius: 0;
}

.card-container {
  border-radius: 5px;
}

.location-investigator-column {
  min-width: 60px;
  height: 100%;
  .portrait {
    height: 25%;
  }
}

.location-asset-column {
  min-width: 60px;
  height: 100%;
  /deep/ .card {
  width: 60px !important;
  }
}
</style>
