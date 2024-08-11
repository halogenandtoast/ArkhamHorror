<script lang="ts" setup>
import { computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import Token from '@/arkham/components/Token.vue';
import * as ArkhamGame from '@/arkham/types/Game';
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message';
import { imgsrc } from '@/arkham/helpers';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import * as Arkham from '@/arkham/types/Skill';

export interface Props {
  game: Game
  skill: Arkham.Skill
  playerId: string
  attached?: boolean
}

const props = withDefaults(defineProps<Props>(), { attached: false })

const emits = defineEmits<{
  choose: [value: number]
}>()

const id = computed(() => props.skill.id)

const cardCode = computed(() => props.skill.cardCode)
const image = computed(() => {
  return imgsrc(`cards/${cardCode.value.replace('c', '')}.jpg`)
})
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL) {
    return c.target.contents === id.value || c.target.contents === props.skill.cardId
  }
  return false
}

const cardAction = computed(() => choices.value.findIndex(canInteract))

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'SkillSource') {
    return source.contents === id.value
  }

  return false
}

const abilities = computed(() => {
  return choices.value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i }];
      }

      return acc;
    }, []);
})

const hasPool = computed(() => {
  const { sealedChaosTokens } = props.skill;

  return sealedChaosTokens.length > 0
})

const choose = (index: number) => emits('choose', index)
</script>

<template>
  <div class="skill" :class="{ attached }">
    <img
      :src="image"
      :class="{ 'skill--can-interact': cardAction !== -1 }"
      class="card skill"
      @click="choose(cardAction)"
      :data-customizations="JSON.stringify(skill.customizations)"
    />
    <div v-if="hasPool" class="pool">
      <Token v-for="(sealedToken, index) in skill.sealedChaosTokens" :key="index" :token="sealedToken" :playerId="playerId" :game="game" @choose="choose" />
    </div>
    <AbilityButton
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      @click="choose(ability.index)"
      />
  </div>
</template>

<style lang="scss" scoped>
.card {
  width: var(--card-width);
  max-width: var(--card-width);
  border-radius: 5px;
}

.skill {
  display: flex;
  flex-direction: column;
  position: relative;
}

.skill--can-interact {
  border: 2px solid $select;
  cursor:pointer;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

:deep(.token) {
  width: 40px;
}

.pool {
  position: absolute;
  top: 50%;
  align-items: center;
  display: flex;
  align-self: flex-start;
  align-items: flex-end;
  * {
    transform: scale(0.6);
  }
  :deep(.token-container) {
    transform: scale(1);
    width: unset;
  }
  :deep(img) {
    width: 20px;
    height: auto;
  }
  z-index: 1;
  pointer-events: none;
}

.attached .card {
  object-fit: cover;
  object-position: left bottom;
  height: calc(var(--card-width)*0.6);
}

</style>
