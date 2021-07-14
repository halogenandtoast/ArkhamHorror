<template>
  <div class="act-container">
    <img
      :class="{ 'act--can-progress': interactAction !== -1 }"
      class="card card--sideways"
      @click="$emit('choose', interactAction)"
      :src="image"
    />
    <Treachery
      v-for="treacheryId in act.contents.treacheries"
      :key="treacheryId"
      :treachery="game.treacheries[treacheryId]"
      :game="game"
      :investigatorId="investigatorId"
      @choose="$emit('choose', $event)"
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
import Treachery from '@/arkham/components/Treachery.vue';
import * as ArkhamGame from '@/arkham/types/Game'
import { Message, MessageType } from '@/arkham/types/Message'
import * as Arkham from '@/arkham/types/Act'

export default defineComponent({
  components: { Treachery },
  props: {
    act: { type: Object as () => Arkham.Act, required: true },
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true }
  },

  setup(props) {
    const id = computed(() => props.act.contents.id)
    const image = computed(() => {
      const side = props.act.contents.sequence.side.toLowerCase().replace('a', '')
      return `/img/arkham/cards/${id.value}${side}.jpg`
    })

    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    function canInteract(c: Message): boolean {
      switch (c.tag) {
        case MessageType.ADVANCE_ACT:
          return true;
        case MessageType.NEXT_ACT:
          return true;
        case MessageType.ATTACH_TREACHERY:
          return c.contents[1].contents == id.value;
        case MessageType.ACTIVATE_ABILITY:
          return c.contents[1].source.contents === id.value
            && (c.contents[1].type.tag === 'ReactionAbility')
        case MessageType.TARGET_LABEL:
          return c.contents[0].tag === "ActTarget" && c.contents[0].contents === id.value
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canInteract(c1));
        default:
          return false;
      }
    }

    const interactAction = computed(() => choices.value.findIndex(canInteract));

    function abilityLabel(idx: number) {
      return choices.value[idx].label
    }

    const abilities = computed(() => {
      return choices.value
        .reduce<number[]>((acc, v, i) => {
          if (v.tag === 'UseAbility' && v.contents[1].source.tag === 'ActSource' && v.contents[1].source.contents === id.value) {
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
