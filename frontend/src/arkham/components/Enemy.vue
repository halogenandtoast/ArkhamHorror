<template>
  <div class="enemy">
    <img :src="image"
      :class="{'enemy--can-interact': cardAction !== -1 }"
      class="card enemy"
      @click="$emit('choose', cardAction)"
    />
    <Treachery
      v-for="treacheryId in enemy.contents.treacheries"
      :key="treacheryId"
      :treachery="game.currentData.treacheries[treacheryId]"
      :game="game"
      :investigatorId="investigatorId"
      @choose="$emit('choose', $event)"
    />
    <Asset
      v-for="assetId in enemy.contents.assets"
      :key="assetId"
      :asset="game.currentData.assets[assetId]"
      :game="game"
      :investigatorId="investigatorId"
      @choose="$emit('choose', $event)"
    />
    <button
      v-if="fightAction !== -1"
      class="button fight-button"
      @click="$emit('choose', fightAction)"
    >Fight</button>
    <button
      v-if="evadeAction !== -1"
      class="button evade-button"
      @click="$emit('choose', evadeAction)"
    >Evade</button>
    <button
      v-if="engageAction !== -1"
      class="button engage-button"
      @click="$emit('choose', engageAction)"
    >Engage</button>
    <AbilityButton
      v-for="ability in abilities"
      :key="ability"
      :ability="choices[ability]"
      @click="$emit('choose', ability)"
      />
    <div class="pool">
      <PoolItem type="health" :amount="enemy.contents.damage" />
      <PoolItem v-if="enemy.contents.doom > 0" type="doom" :amount="enemy.contents.doom" />
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import { Message, MessageType } from '@/arkham/types/Message'
import PoolItem from '@/arkham/components/PoolItem.vue'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Treachery from '@/arkham/components/Treachery.vue';
import Asset from '@/arkham/components/Asset.vue';
import * as Arkham from '@/arkham/types/Enemy'

export default defineComponent({
  components: { PoolItem, Treachery, AbilityButton, Asset },
  props: {
    game: { type: Object as () => Game, required: true },
    enemy: { type: Object as () => Arkham.Enemy, required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props) {
    const image = computed(() => {
      const { cardCode } = props.enemy.contents.cardDef;
      return `/img/arkham/cards/${cardCode}.jpg`;
    })

    const id = computed(() => props.enemy.contents.id)

    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    const attackAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.ENEMY_ATTACK && c.contents[1] === id.value)
    })

    const labelAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.TARGET_LABEL && c.contents[0].contents === id.value)
    })

    const moveAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.ENEMY_MOVE && c.contents[0] === id.value)
    })

    const placeDoomAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.PLACE_DOOM && c.contents[0].contents === id.value)
    })

    const damageAction = computed(() => {
      const isRunDamage = choices.value.findIndex((c) => c.tag === MessageType.RUN
        && c.contents[0]
        && c.contents[0].tag === MessageType.ENEMY_DAMAGE
        && c.contents[0].contents[0] === id.value);

      if (isRunDamage !== -1) {
        return isRunDamage;
      }

      return choices
        .value
        .findIndex((c) => c.tag === MessageType.ENEMY_DAMAGE && c.contents[0] === id.value);
    })

    const cardAction = computed(() => {
      if (labelAction.value !== -1) {
        return labelAction.value;
      }

      if (attackAction.value !== -1) {
        return attackAction.value;
      }

      if (moveAction.value !== -1) {
        return moveAction.value;
      }

      if (placeDoomAction.value !== -1) {
        return placeDoomAction.value;
      }

      return damageAction.value;
    });


    const fightAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.FIGHT_ENEMY && c.contents[1] === id.value);
    })

    const evadeAction = computed(() => {
      return choices
        .value
        .findIndex((c) => (c.tag === MessageType.EVADE_ENEMY || c.tag === MessageType.ENEMY_EVADED) && c.contents[1] === id.value);
    })

    const engageAction = computed(() => {
      // This is for the rougarou, we look at the 2nd [1] item in the array because
      // the first is spending clues, there may be a different approach to take here
      const isRunEngage = choices.value.findIndex((c) => c.tag === MessageType.RUN
        && c.contents[1]
        && c.contents[1].tag === MessageType.ENGAGE_ENEMY
        && c.contents[1].contents[1] === id.value);

      if (isRunEngage !== -1) {
        return isRunEngage;
      }

      return choices
        .value
        .findIndex((c) => c.tag === MessageType.ENGAGE_ENEMY && c.contents[1] === id.value);
    })

    function isActivate(v: Message) {
      if (v.tag !== 'ActivateCardAbilityAction') {
        return false
      }

      const { tag, contents } = v.contents[1].source;

      if (tag === 'EnemySource' && contents === id.value) {
        return true
      }

      if (tag === 'ProxySource' && contents[0].tag === 'EnemySource' && contents[0].contents === id.value) {
        return true
      }

      return false
    }

    const abilities = computed(() => {
      return choices
        .value
        .reduce<number[]>((acc, v, i) => {
          if (v.tag === 'Run' && isActivate(v.contents[0])) {
            return [...acc, i];
          } else if (isActivate(v)) {
            return [...acc, i];
          }

          return acc;
        }, []);
    })

    return { abilities, choices, engageAction, fightAction, evadeAction, cardAction, image }
  }
})
</script>

<style scoped lang="scss">
.enemy--can-interact {
  border: 3px solid #FF00FF;
  border-radius: 15px;
  cursor: pointer;
}

.enemy {
  display: flex;
  flex-direction: column;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.fight-button {
  background-color: #8F5B41;
  &:before {
    font-family: "Arkham";
    content: "\0044";
    margin-right: 5px;
  }
}

.evade-button {
  background-color: #576345;
  &:before {
    font-family: "Arkham";
    content: "\0053";
    margin-right: 5px;
  }
}

.engage-button {
  background-color: #555;
  &:before {
    font-family: "Arkham";
    content: "\0048";
    margin-right: 5px;
  }
}

.card {
  width: 100px;
  max-width: 100px;
  border-radius: 5px;
}

.pool {
  display: flex;
  flex-direction: row;
  height: 2em;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

::v-deep .treachery {
  object-fit: cover;
  object-position: 0 -74px;
  height: 68px;
  margin-top: 2px;
}
</style>
