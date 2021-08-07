<template>
  <div class="event">
    <img
      :src="image"
      :class="{ 'event--can-interact': cardAction !== -1 }"
      class="card event"
      @click="$emit('choose', cardAction)"
    />
    <AbilityButton
      v-for="ability in abilities"
      :key="ability"
      :ability="choices[ability]"
      @click="$emit('choose', ability)"
      />
    <div v-if="hasPool" class="pool">
      <PoolItem v-if="event.contents.doom > 0" type="doom" :amount="event.contents.doom" />
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
import * as Arkham from '@/arkham/types/Event';

export default defineComponent({
  components: { PoolItem, AbilityButton },
  props: {
    game: { type: Object as () => Game, required: true },
    event: { type: Object as () => Arkham.Event, required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props) {
    const id = computed(() => props.event.contents.id)
    const hasPool = computed(() => {
      const { doom } = props.event.contents
      return doom > 0
    })

    const cardCode = computed(() => props.event.contents.cardCode)
    const image = computed(() => {
      const baseUrl = process.env.NODE_ENV == 'production' ? "https://events.arkhamhorror.app" : '';
      return `${baseUrl}/img/arkham/cards/${cardCode.value.replace('c', '')}.jpg`
    })
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    function canInteract(c: Message): boolean {
      switch (c.tag) {
        case MessageType.DISCARD:
          return c.contents.contents === id.value
        case MessageType.READY:
          return c.contents.contents === id.value
        case MessageType.REMOVE_DOOM:
          return c.contents[0].contents === id.value
        case MessageType.USE_CARD_ABILITY:
          return c.contents[1].contents === id.value
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canInteract(c1))
        case MessageType.TARGET_LABEL:
          return c.contents[0].tag === "EventTarget" && c.contents[0].contents === id.value
        default:
          return false;
      }
    }

    const cardAction = computed(() => choices.value.findIndex(canInteract))

    function isActivate(v: Message) {
      if (v.tag !== 'UseAbility') {
        return false
      }

      const { tag, contents } = v.contents[1].source;

      if (tag === 'EventSource' && contents === id.value) {
        return true
      }

      if (tag === 'ProxySource' && contents[0].tag === 'EventSource' && contents[0].contents === id.value) {
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

    return { debug, debugChoose, id, hasPool, image, abilities, cardAction, choices }
  }
})
</script>

<style lang="scss" scoped>
.card {
  width: $card-width;
  max-width: $card-width;
  border-radius: 5px;
}

.event {
  display: flex;
  flex-direction: column;
}

.event--can-interact {
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
