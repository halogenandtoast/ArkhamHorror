<template>
  <div class="agenda-container">
    <img
      :class="{ 'agenda--can-progress': interactAction !== -1 }"
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
    <div class="pool">
      <PoolItem
        type="doom"
        :amount="agenda.contents.doom"
      />
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';
import PoolItem from '@/arkham/components/PoolItem.vue';
import * as Arkham from '@/arkham/types/Agenda';

export default defineComponent({
  components: { PoolItem },
  props: {
    agenda: { type: Object as () => Arkham.Agenda, required: true },
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true }
  },
  setup(props) {
    const id = computed(() => props.agenda.contents.id)
    const image = computed(() => {
      if (props.agenda.contents.flipped) {
        return `/img/arkham/cards/${id.value}b.jpg`;
      }

      return `/img/arkham/cards/${id.value}.jpg`;
    })

    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    function canInteract(c: Message): boolean {
      switch (c.tag) {
        case MessageType.ADVANCE_AGENDA:
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
      return choices
        .value
        .reduce<number[]>((acc, v, i) => {
          if (v.tag === 'ActivateCardAbilityAction' && v.contents[1].source.tag === 'AgendaSource' && v.contents[1].source.contents === id.value) {
            return [...acc, i];
          }

          return acc;
        }, [])
    })

    return { abilities, abilityLabel, interactAction, image, id }
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

.card--sideways {
  width: auto;
  height: 100px;
}

.agenda-container {
  display: flex;
  flex-direction: column;
}

.agenda--can-progress {
  border: 3px solid #ff00ff;
  border-radius: 8px;
  cursor: pointer;
}

.pool {
  display: flex;
  flex-direction: row;
  height: 2em;
  justify-content: flex-start;
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
