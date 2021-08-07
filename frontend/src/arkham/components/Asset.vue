<template>
  <div class="asset">
    <img
      :src="image"
      :class="{ 'asset--can-interact': cardAction !== -1, exhausted}"
      class="card"
      @click="$emit('choose', cardAction)"
    />
    <AbilityButton
      v-for="ability in abilities"
      :key="ability"
      :ability="choices[ability]"
      @click="$emit('choose', ability)"
      />
    <template v-if="debug">
      <button v-if="!asset.contents.investigator" @click="debugChoose({tag: 'TakeControlOfAsset', contents: [investigatorId, id]})">Take control</button>
      <button v-if="asset.contents.investigator" @click="debugChoose({tag: 'Discard', contents: { tag: 'AssetTarget', contents: id}})">Discard</button>
    </template>
    <div v-if="hasPool" class="pool">
      <PoolItem
        v-if="asset.contents.uses && asset.contents.uses.amount > 0"
        type="resource"
        :amount="asset.contents.uses.amount"
      />
      <PoolItem
        v-if="asset.contents.horror"
        type="sanity"
        :amount="asset.contents.horror"
      />
      <PoolItem
        v-if="asset.contents.health"
        type="health"
        :amount="asset.contents.healthDamage"
        :class="{ 'health--can-interact': healthAction !== -1 }"
        @choose="$emit('choose', healthAction)"
      />
      <PoolItem
        v-if="asset.contents.sanity"
        type="sanity"
        :amount="asset.contents.sanityDamage"
        :class="{ 'sanity--can-interact': sanityAction !== -1 }"
        @choose="$emit('choose', sanityAction)"
      />
      <PoolItem v-if="asset.contents.doom > 0" type="doom" :amount="asset.contents.doom" />
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed, inject } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';
import PoolItem from '@/arkham/components/PoolItem.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import * as Arkham from '@/arkham/types/Asset';

export default defineComponent({
  components: { PoolItem, AbilityButton },
  props: {
    game: { type: Object as () => Game, required: true },
    asset: { type: Object as () => Arkham.Asset, required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props) {
    const id = computed(() => props.asset.contents.id)
    const hasPool = computed(() => {
      const {
        sanity,
        health,
        horror,
        uses,
      } = props.asset.contents;
      return sanity || health || horror || uses;
    })

    const exhausted = computed(() => props.asset.contents.exhausted)
    const cardCode = computed(() => props.asset.contents.cardCode)
    const image = computed(() => {
      const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';
      return `${baseUrl}/img/arkham/cards/${cardCode.value.replace('c', '')}.jpg`
    })
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    function canInteract(c: Message): boolean {
      switch (c.tag) {
        case MessageType.DISCARD:
          return c.contents.contents === id.value
        case MessageType.READY:
          return c.contents.contents === id.value
        case MessageType.FLIP:
          return c.contents[1].contents === id.value
        case MessageType.REMOVE_DOOM:
          return c.contents[0].contents === id.value
        case MessageType.LOOK_AT_REVEALED:
          return c.contents[1].contents === id.value
        case MessageType.ADD_USES:
          return c.contents[0].contents === id.value
        case MessageType.USE_CARD_ABILITY:
          return c.contents[1].contents === id.value
        // case MessageType.ACTIVATE_ABILITY:
        //   return c.contents[1].source.contents === id.value
        //     && (c.contents[1].type.tag === 'ReactionAbility')
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canInteract(c1))
        case MessageType.TARGET_LABEL:
          return c.contents[0].tag === "AssetTarget" && c.contents[0].contents === id.value
        default:
          return false;
      }
    }

    function canAdjustHealth(c: Message): boolean {
      switch (c.tag) {
        case MessageType.ASSET_DAMAGE:
          return c.contents[0] === id.value && c.contents[2] > 0;
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canAdjustHealth(c1));
        default:
          return false;
      }
    }

    function canAdjustSanity(c: Message): boolean {
      switch (c.tag) {
        case MessageType.ASSET_DAMAGE:
          return c.contents[0] === id.value && c.contents[3] > 0;
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canAdjustSanity(c1));
        default:
          return false;
      }
    }

    const cardAction = computed(() => choices.value.findIndex(canInteract))
    const healthAction = computed(() => choices.value.findIndex(canAdjustHealth))
    const sanityAction = computed(() => choices.value.findIndex(canAdjustSanity))

    function isActivate(v: Message) {
      if (v.tag !== 'UseAbility') {
        return false
      }

      const { tag, contents } = v.contents[1].source;

      if (tag === 'AssetSource' && contents === id.value) {
        return true
      }

      if (tag === 'ProxySource' && contents[0].tag === 'AssetSource' && contents[0].contents === id.value) {
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

    const debug = inject('debug')
    const debugChoose = inject('debugChoose')

    return { debug, debugChoose, id, hasPool, exhausted, image, abilities, sanityAction, healthAction, cardAction, choices }
  }
})
</script>

<style lang="scss" scoped>
.card {
  width: $card-width;
  max-width: $card-width;
  border-radius: 5px;
}

.asset {
  display: flex;
  flex-direction: column;
}

.exhausted {
  transform: rotate(90deg);
  padding: 0 30px;
}

.asset--can-interact {
  border: 2px solid $select;
  cursor:pointer;
}

.pool {
  display: flex;
  flex-direction: row;
  height: 2em;
  justify-content: center;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}
</style>
