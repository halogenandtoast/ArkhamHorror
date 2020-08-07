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
        v-if="asset.contents.sanity"
        type="sanity"
        :amount="asset.contents.sanityDamage"
      />
      <PoolItem
        v-if="asset.contents.health"
        type="health"
        :amount="asset.contents.healthDamage"
      />
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
    return choices(this.game);
  }

  get cardAction() {
    return this.choices.findIndex(this.canInteract);
  }

  canInteract(c: Message): boolean {
    switch (c.tag) {
      case MessageType.DISCARD_ASSET:
        return c.contents === this.id;
      case MessageType.ASSET_DAMAGE:
        return c.contents[0] === this.id;
      case MessageType.RUN:
        return c.contents.some((c1: Message) => this.canInteract(c1));
      default:
        return false;
    }
  }

  abilityLabel(idx: number) {
    return this.choices[idx].contents[1][3].contents[1];
  }

  get abilities() {
    return this
      .choices
      .reduce<number[]>((acc, v, i) => {
        if (v.tag === 'ActivateCardAbilityAction' && v.contents[1][0].tag === 'AssetSource' && v.contents[1][0].contents === this.id) {
          return [i, ...acc];
        }

        return acc;
      }, []);
  }
}
</script>

<style lang="scss" scoped>
.card {
  width: 130px;
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
