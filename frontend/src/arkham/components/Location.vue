<template>
  <div class="location-container">
    <div class="location-investigator-column">
      <div
        v-for="cardCode in location.contents.investigators"
        :key="cardCode"
      >
        <Investigator
          :game="game"
          :investigatorId="investigatorId"
          :portrait="true"
          :player="game.investigators[cardCode]"
          @choose="$emit('choose', $event)"
          />
      </div>
    </div>
    <div class="location-column">
      <font-awesome-icon v-if="blocked" :icon="['fab', 'expeditedssl']" class="status-icon" />

      <div
        :class="{ 'location--can-interact': cardAction !== -1 }"
        :data-id="id"
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
      <AbilityButton
        v-for="ability in abilities"
        :key="ability"
        :ability="choices[ability]"
        @click="$emit('choose', ability)"
        />
      <template v-if="debug">
        <button v-if="!location.contents.revealed" @click="debugChoose({tag: 'RevealLocation', contents: [null, id]})">Reveal</button>
      </template>
      <Treachery
        v-for="treacheryId in location.contents.treacheries"
        :key="treacheryId"
        :treachery="game.treacheries[treacheryId]"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
      <Event
        v-for="eventId in location.contents.events"
        :event="game.events[eventId]"
        :game="game"
        :investigatorId="investigatorId"
        :key="eventId"
        @choose="$emit('choose', $event)"
      />
      <div class="pool">
        <div v-if="location.contents.clues > 0" class="pool">
          <PoolItem type="clue" :amount="location.contents.clues" />
        </div>
        <div v-if="location.contents.doom > 0" class="pool">
          <PoolItem type="doom" :amount="location.contents.doom" />
        </div>
        <div v-if="location.contents.cardsUnderneath.length > 0" class="pool">
          <PoolItem type="card" :amount="location.contents.cardsUnderneath.length" />
        </div>
        <div v-if="location.contents.resources > 0" class="pool">
          <PoolItem type="resource" :amount="location.contents.resources" />
        </div>
      </div>
    </div>
    <div class="location-asset-column">
      <Asset
        v-for="assetId in location.contents.assets"
        :asset="game.assets[assetId]"
        :game="game"
        :investigatorId="investigatorId"
        :key="assetId"
        @choose="$emit('choose', $event)"
      />
      <Enemy
        v-for="enemyId in enemies"
        :key="enemyId"
        :enemy="game.enemies[enemyId]"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed, inject } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';
import Enemy from '@/arkham/components/Enemy.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import Asset from '@/arkham/components/Asset.vue';
import Event from '@/arkham/components/Event.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Location';

export default defineComponent({
  components: {
    Enemy,
    Treachery,
    Asset,
    Event,
    Investigator,
    PoolItem,
    AbilityButton,
  },
  props: {
    game: { type: Object as () => Game, required: true },
    location: { type: Object as () => Arkham.Location, required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props, { emit }) {
    const clues = computed(() => props.location.contents.clues)
    const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';

    const image = computed(() => {
      const { cardCode, revealed } = props.location.contents
      const suffix = revealed ? '' : 'b'

      return `${baseUrl}/img/arkham/cards/${cardCode.replace('c', '')}${suffix}.jpg`
    })

    const id = computed(() => props.location.contents.id)
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    const targetAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.TARGET_LABEL
          && c.contents[0].contents === id.value);
    })

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

    function findForcedAbility(c: Message): boolean {
      switch (c.tag) {
        case MessageType.ACTIVATE_ABILITY:
          return c.contents[1].source.contents === id.value
            && (c.contents[1].type.tag === 'ForcedAbility')
        case MessageType.RUN:
          return c.contents.some((c1: Message) => findForcedAbility(c1));
        default:
          return false;
      }
    }

    const forcedAbility = computed(() => choices.value.findIndex(findForcedAbility));

    const cardAction = computed(() => {
      if (forcedAbility.value !== -1) {
        return forcedAbility.value;
      }

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

      if (targetAction.value !== -1) {
        return targetAction.value;
      }

      return moveAction.value;
    })

    const investigateAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.INVESTIGATE && c.contents[1] === id.value);
    })

    function isAbility(v: Message) {
      if (v.tag !== 'UseAbility') {
        return false
      }

      const { tag, contents } = v.contents[1].source;

      if (tag === 'LocationSource' && contents === id.value) {
        return true
      }

      if (tag === 'ProxySource' && contents[0].tag === 'LocationSource' && contents[0].contents === id.value) {
        return true
      }

      return false
    }

    const abilities = computed(() => {
      return choices
        .value
        .reduce<number[]>((acc, v, i) => {
          if ((v.tag === 'UseAbility' || v.tag === 'ActivateCardAbilityActionWithDynamicCost') && v.contents[1].source.tag === 'LocationSource' && v.contents[1].source.contents === id.value) {
            return [i, ...acc];
          }

          if (v.tag === 'UseAbility' && v.contents[1].source.tag === 'ProxySource' && v.contents[1].source.contents[0].tag === 'LocationSource' && v.contents[1].source.contents[0].contents === id.value) {
            return [...acc, i];
          }

          if (v.tag === 'UseAbility' && v.contents[1].source.tag === 'ProxySource' && v.contents[1].source.contents[0].tag === 'LocationSource' && v.contents[1].source.contents[0].contents === id.value) {
            return [...acc, i];
          }

          if (v.tag === 'Run' && isAbility(v.contents[0])) {
            return [...acc, i];
          }

          return acc;
        }, []);
    })

    const enemies = computed(() => {
      const enemyIds = props.location.contents.enemies;
      return enemyIds
        .filter((e) => props.game.enemies[e].contents.engagedInvestigators.length === 0);
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

    const debug = inject('debug')
    const debugChoose = inject('debugChoose')

    return {
      debug,
      debugChoose,
      id,
      doInvestigate,
      blocked,
      enemies,
      abilities,
      investigateAction,
      cardAction,
      image,
      choices,
    }
  }
})
</script>

<style scoped lang="scss">
.location--can-interact {
  border: 3px solid $select;
  cursor: pointer;
}

.card {
  width: $card-width;
  border-radius: 3px;
}

.location-column :deep(.enemy) {
  width: $card-width * 0.8;
}

.location-column :deep(.treachery) {
  object-fit: cover;
  object-position: 0 -74px;
  height: 68px;
  margin-top: 2px;
}

.location-column :deep(.event) {
  object-fit: cover;
  object-position: 0 -74px;
  height: 68px;
  margin-top: 2px;
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
  height: $card-width * 1.07;
  width: $card-width * 1.25;
  background-size: 100%;
  border-bottom-left-radius: 0;
  border-bottom-right-radius: 0;
}

.location-connections {
  height: $card-width * 0.27;
  width: $card-width * 1.25;
  background-size: 100%;
  background-position: bottom;
  border-top-left-radius: 0;
  border-top-right-radius: 0;
}

.card-container {
  border-radius: 5px;
}

.location-investigator-column {
  min-width: $card-width * 0.6;
  height: 100%;
  &:deep(.portrait) {
    height: 25%;
  }
}

.location-asset-column {
  min-width: $card-width * 0.6;
  height: 100%;
  &:deep(.card) {
    width: $card-width * 0.6 !important;
  }
}

.pool {
  display: flex;
  align-self: flex-start;
  align-items: flex-end;
}
</style>
