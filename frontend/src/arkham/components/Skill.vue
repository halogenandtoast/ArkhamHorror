<script lang="ts" setup>
import { computed, ref } from 'vue';
import { useDebug } from '@/arkham/debug';
import * as DebugInPlay from '@/arkham/debugInPlay';
import CardDebugWindow from '@/arkham/components/debug/CardDebugWindow.vue';
import MissingCardBadge from '@/arkham/components/MissingCardBadge.vue';
import { Game } from '@/arkham/types/Game';
import Token from '@/arkham/components/Token.vue';
import * as ArkhamGame from '@/arkham/types/Game';
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message';
import { cardImg } from '@/arkham/helpers';
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
const debug = useDebug()
const debugging = ref(false)

const cardCode = computed(() => props.skill.cardCode)
const image = computed(() => {
  const mutated = props.skill.mutated ? `_${props.skill.mutated}` : ''
  return cardImg(`${cardCode.value.replace(/^c/, '')}${mutated}`)
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

const target = computed(() => DebugInPlay.skillTarget(id.value))

/* A skill carries no owner to the client, so the debug button returns it to the
 * hand of the investigator the debugging player controls. A drag says where it
 * is going by where it is dropped, so it does not go through this. */
const returnToHandInvestigator = computed(
  () => Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)?.id
)

function returnToHand() {
  const iid = returnToHandInvestigator.value
  if (iid) debug.send(props.game.id, DebugInPlay.returnToHandMessage(iid, target.value))
}

function startDrag(event: DragEvent) {
  if (!debug.active) {
    event.preventDefault()
    return
  }
  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'copyMove'
    event.dataTransfer.setData('text/plain', JSON.stringify(target.value))
    DebugInPlay.beginInPlayDrag(target.value)
  }
}

const choose = (index: number) => emits('choose', index)
</script>

<template>
  <div class="skill" :class="{ attached }">
    <MissingCardBadge :card-code="cardCode" />
    <img
      :src="image"
      :class="{ 'skill--can-interact': cardAction !== -1 }"
      class="card skill"
      :draggable="debug.active"
      @dragstart="startDrag"
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
      :game="game"
      @click="choose(ability.index)"
      />
    <template v-if="debug.active">
      <button class="debug-open" @click="debugging = true">{{ $t('enemy.debug') }}</button>
    </template>
    <CardDebugWindow
      v-if="debugging"
      :game="game"
      :title="$t('debug.card.skillTitle')"
      :image="image"
      :sealedTokens="skill.sealedChaosTokens"
      :target="target"
      @close="debugging = false"
    >
      <div class="debug-actions">
        <button type="button" :disabled="!returnToHandInvestigator" @click="returnToHand">
          {{ $t('debug.common.returnToHand') }}
        </button>
      </div>
    </CardDebugWindow>
  </div>
</template>

<style scoped>
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
  border: 2px solid var(--select);
  cursor:pointer;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid var(--select);
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
  z-index: var(--z-index-1);
  pointer-events: none;
  & :deep(.token-container) {
    width: unset;
  }
  & :deep(img) {
    width: var(--card-token-width);
    height: auto;
  }
}

.attached .card {
  object-fit: cover;
  object-position: left bottom;
  height: calc(var(--card-width)*0.6);
}

</style>
