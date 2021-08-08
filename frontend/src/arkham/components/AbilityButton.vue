<template>
    <button
      class="button"
      :class="{ 'ability-button': isSingleActionAbility, 'double-ability-button': isDoubleActionAbility, 'fast-ability-button': isFastActionAbility, 'reaction-ability-button': isReactionAbility, 'forced-ability-button': isForcedAbility }"
      @click="$emit('choose', ability)"
      >{{abilityLabel}}</button>
</template>

<script lang="ts">
import { defineComponent, computed, ComputedRef } from 'vue';
import { Cost } from '@/arkham/types/Cost';
import { Message } from '@/arkham/types/Message';

export default defineComponent({
  props: {
    ability: { type: Object as () => Message, required: true }
  },

  setup(props) {
    const ability: ComputedRef<Message> = computed(() => props.ability.tag == 'Run' ? props.ability.contents[0] : props.ability)

    const abilityLabel = computed(() => {
      const label = ability.value.tag === 'Run'
        ? (ability.value.contents[1]?.type?.tag === "ForcedAbility"
          ? "Forced"
          : ability.value.contents[0].contents[1].type.contents[0]
          )
        : (ability.value.contents[1]?.type?.tag === "ForcedAbility"
          ? "Forced"
          : ability.value.contents[1].type.contents[0]
          )
      if (label) {
        return typeof label === "string" ? label : label.contents
      }

      return ""
    })

    const isSingleActionAbility = computed(() => {
      if (ability.value.contents[1].type.tag !== "ActionAbility") {
        return false
      }
      const { contents } = ability.value.contents[1].type.contents[1]
      if (typeof contents?.some == 'function') {
        return contents.some((cost: Cost) => cost.tag == "ActionCost" && cost.contents == 1)
      } else {
        return contents === 1
      }
    })
    const isDoubleActionAbility = computed(() => {
      if (ability.value.contents[1].type.tag !== "ActionAbility") {
        return false
      }
      const { contents } = ability.value.contents[1].type.contents[1]
      if (typeof contents?.some == 'function') {
        return contents.some((cost: Cost) => cost.tag == "ActionCost" && cost.contents == 2)
      } else {
        return contents === 2
      }
    })
    const isFastActionAbility = computed(() => ability.value.contents[1].type.tag === "FastAbility")
    const isReactionAbility = computed(() => ability.value.contents[1].type.tag === "ReactionAbility")
    const isForcedAbility = computed(() => ability.value.contents[1].type.tag === "ForcedAbility")

    return {
      abilityLabel,
      isSingleActionAbility,
      isDoubleActionAbility,
      isFastActionAbility,
      isReactionAbility,
      isForcedAbility,
    }
  }
})
</script>

<style lang="scss" scoped>
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

.forced-ability-button {
  background-color: #222;
  color: #fff;
}

.reaction-ability-button {
  background-color: #A02ECB;
  &:before {
    font-family: "arkham";
    content: "\0059";
    margin-right: 5px;
  }
}
</style>
