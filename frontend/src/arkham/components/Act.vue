<template>
  <div class="act-container">
    <img
      :class="{ 'act--can-progress': interactAction !== -1 }"
      class="card card--sideways"
      @click="$emit('choose', interactAction)"
      :src="image"
    />
    <button
      v-for="ability in abilities"
      :key="ability"
      class="button ability-button"
      @click="$emit('choose', ability)"
      >{{abilityLabel(ability)}}</button>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import { Message, MessageType } from '@/arkham/types/Message'
import * as Arkham from '@/arkham/types/Act'

export default defineComponent({
  props: {
    act: { type: Object as () => Arkham.Act, required: true },
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true }
  },

  setup(props) {
    const id = computed(() => props.act.contents.id)
    const image = computed(() => {
      if (props.act.contents.flipped) {
        return `/img/arkham/cards/${id.value}b.jpg`;
      }

      return `/img/arkham/cards/${id.value}.jpg`;
    })

    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    function canInteract(c: Message): boolean {
      switch (c.tag) {
        case MessageType.ADVANCE_ACT:
          return true;
        case MessageType.ATTACH_TREACHERY:
          return c.contents[1].contents == id.value;
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canInteract(c1));
        default:
          return false;
      }
    }

    const interactAction = computed(() => choices.value.findIndex(canInteract));

    function abilityLabel(idx: number) {
      return choices.value[idx].contents[1].type.contents[1];
    }

    const abilities = computed(() => {
      return choices.value
        .reduce<number[]>((acc, v, i) => {
          if (v.tag === 'ActivateCardAbilityAction' && v.contents[1].source.tag === 'ActSource' && v.contents[1].source.contents === id.value) {
            return [...acc, i];
          }

          return acc;
        }, [])
    })

    return { abilities, abilityLabel, interactAction, choices, image, id }
  }
})
</script>

<style scoped lang="scss">
.card {
  width: 100px;
  -webkit-box-shadow: 0 3px 6px rgba(0, 0, 0, 0.23), 0 3px 6px rgba(0, 0, 0, 0.53);
  box-shadow: 0 3px 6px rgba(0, 0, 0, 0.23), 0 3px 6px rgba(0, 0, 0, 0.53);
  border-radius: 6px;
  margin: 2px;
}

.act-container {
  display: flex;
  flex-direction: column;
}

.card--sideways {
  width: auto;
  height: 100px;
}

.act--can-progress {
  border: 3px solid #ff00ff;
  border-radius: 8px;
  cursor: pointer;
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
