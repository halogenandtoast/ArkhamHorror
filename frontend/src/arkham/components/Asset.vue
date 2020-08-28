<template>
  <div class="asset">
    <img
      :src="image"
      :class="{ 'asset--can-interact': cardAction !== -1, exhausted}"
      class="card"
      @click="$emit('choose', cardAction)"
    />
    <button
      v-for="ability in abilities"
      :key="ability"
      class="button ability-button"
      @click="$emit('choose', ability)"
      >{{abilityLabel(ability)}}</button>
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
import { Component, Vue, Prop } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Asset';

@Component({
  components: { PoolItem },
})
export default class Asset extends Vue {
  @Prop(Object) readonly game!: Game
  @Prop(String) readonly investigatorId!: string
  @Prop(Object) readonly asset!: Arkham.Asset

  get id() {
    return this.asset.contents.id;
  }

  get hasPool() {
    const {
      sanity,
      health,
      horror,
      uses,
    } = this.asset.contents;
    return sanity || health || horror || uses;
  }

  get exhausted() {
    return this.asset.contents.exhausted;
  }

  get cardCode() {
    return this.asset.contents.cardCode;
  }

  get image() {
    return `/img/arkham/cards/${this.cardCode}.jpg`;
  }

  get choices() {
    return choices(this.game, this.investigatorId);
  }

  get cardAction() {
    return this.choices.findIndex(this.canInteract);
  }

  canInteract(c: Message): boolean {
    switch (c.tag) {
      case MessageType.DISCARD:
        return c.contents.contents === this.id;
      case MessageType.USE_CARD_ABILITY:
        return c.contents[1].contents === this.id;
      case MessageType.ACTIVATE_ABILITY:
        return c.contents[1].source.contents === this.id
          && (c.contents[1].type.tag === 'ReactionAbility' || c.contents[1].type.tag === 'FastAbility');
      case MessageType.RUN:
        return c.contents.some((c1: Message) => this.canInteract(c1));
      default:
        return false;
    }
  }

  get healthAction() {
    return this.choices.findIndex(this.canAdjustHealth);
  }

  get sanityAction() {
    return this.choices.findIndex(this.canAdjustSanity);
  }

  canAdjustHealth(c: Message): boolean {
    switch (c.tag) {
      case MessageType.ASSET_DAMAGE:
        return c.contents[0] === this.id && c.contents[2] > 0;
      case MessageType.RUN:
        return c.contents.some((c1: Message) => this.canAdjustHealth(c1));
      default:
        return false;
    }
  }

  canAdjustSanity(c: Message): boolean {
    switch (c.tag) {
      case MessageType.ASSET_DAMAGE:
        return c.contents[0] === this.id && c.contents[3] > 0;
      case MessageType.RUN:
        return c.contents.some((c1: Message) => this.canAdjustSanity(c1));
      default:
        return false;
    }
  }

  abilityLabel(idx: number) {
    return this.choices[idx].contents[1].type.contents[1];
  }

  get abilities() {
    return this
      .choices
      .reduce<number[]>((acc, v, i) => {
        if (v.tag === 'ActivateCardAbilityAction' && v.contents[1].source.tag === 'AssetSource' && v.contents[1].source.contents === this.id) {
          return [...acc, i];
        }

        return acc;
      }, []);
  }

  canTriggerReaction(c: Message): boolean {
    switch (c.tag) {
      case MessageType.ACTIVATE_ABILITY:
        console.log(c); // eslint-disable-line
        return c.contents[1].source.tag === 'AssetSource' && c.contents[1].source.contents === this.id;
      case MessageType.RUN:
        return c.contents.some((c1: Message) => this.canTriggerReaction(c1));
      default:
        return false;
    }
  }
}
</script>

<style lang="scss" scoped>
.card {
  width: 100px;
  max-width: 100px;
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
  border: 2px solid #FF00FF;
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

.ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0049";
    margin-right: 5px;
  }
}

</style>
