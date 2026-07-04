<script lang="ts" setup>
import type { Game } from '@/arkham/types/Game';
import type { Card as ArkhamCard, CardContents } from '@/arkham/types/Card';
import type { Source } from '@/arkham/types/Source';
import * as CardT from '@/arkham/types/Card';
import Card from '@/arkham/components/Card.vue';
import Draggable from '@/components/Draggable.vue';
import { useDebug } from '@/arkham/debug';
import { computed } from 'vue';
import * as ArkhamGame from '@/arkham/types/Game';

const debug = useDebug()

const props = withDefaults(defineProps<{
  game: Game
  cards: (ArkhamCard | CardContents)[]
  playerId: string
  isDiscards?: boolean
  title: string
  revealed?: boolean
}>(), { isDiscards: false, revealed: false })

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
function sourceMatchesCard(source: Source, cardId: string): boolean {
  switch (source.sourceTag) {
    case 'ProxySource':
      return sourceMatchesCard(source.source, cardId) || sourceMatchesCard(source.originalSource, cardId)
    case 'IndexedSource':
      return source.contents ? sourceMatchesCard(source.contents[1], cardId) : false
    case 'AbilitySource':
      return sourceMatchesCard(source.contents[0], cardId)
    case 'UseAbilitySource':
      return sourceMatchesCard(source.contents[1], cardId)
    case 'PaymentSource':
      return sourceMatchesCard(source.contents, cardId)
    case 'BothSource':
      return sourceMatchesCard(source.contents[0], cardId) || sourceMatchesCard(source.contents[1], cardId)
    case 'OtherSource':
      return source.contents === cardId || (source.tag === 'AssetSource' && props.game.assets[source.contents ?? '']?.cardId === cardId)
    default:
      return false
  }
}

function isCardInChoices(card: ArkhamCard | CardContents): boolean {
  const cardId = CardT.toCardContents(card).id
  return choices.value.some(choice => {
    if (choice.tag === 'TargetLabel') return cardId === choice.target.contents
    if (choice.tag === 'AbilityLabel') return sourceMatchesCard(choice.ability.source, cardId)
    return false
  })
}

const emit = defineEmits<{
  choose: [value: number]
  close: []
}>()

function startDrag(event: DragEvent, card: (CardContents | CardT.Card)) {
  if (!debug.active) {
    event.preventDefault()
    return
  }
  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'copy'
    const cardId = CardT.toCardContents(card).id
    event.dataTransfer.setData('text/plain', JSON.stringify({ "tag": "CardTarget", "contents": cardId }))
  }
}
</script>

<template>
  <Draggable>
    <template #handle>
      <h2>{{title}}</h2>
    </template>
    <div class="card-row-container">
      <div class="card-row-cards">
        <div v-for="card in cards" :key="CardT.toCardContents(card).id" class="card-row-card" :class="{ discard: isDiscards && !isCardInChoices(card)}">
          <Card 
            :draggable="debug.active"
            @dragstart="startDrag($event, card)"
            :game="game" :card="card" :playerId="playerId" :revealed="revealed" @choose="emit('choose', $event)" />
        </div>
      </div>
      <button class="button close" @click="emit('close')">{{ $t('close') }}</button>
    </div>
  </Draggable>
</template>

<style scoped>
.card-row {
  background: #759686;
  width: 100%;
  overflow-x: auto;
  text-align: center;
}

.card-row-cards {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 10px;
  gap: 2px;
  flex-wrap: wrap;
}

.card-row-card {
  position: relative;
}

.discard {
  filter: grayscale(0.85);
}

.card {
  width: var(--card-width);
  border-radius: 6px;
  margin: 2px;
}

button {
  border: 0;
  padding: 10px;
  text-transform: uppercase;
  background-color: var(--button-2);
  font-weight: bold;
  border-radius: 0.6em;
  border-top-left-radius: 0;
  border-top-right-radius: 0;
  color: #EEE;
  font: Arial, sans-serif;
  width: 100%;
  &:hover {
    background-color: var(--button-2-highlight);
  }
}

.card-row {
  position: absolute;
  width: fit-content;
  max-width: 80%;
  top: 50%;
  left: 50%;
  background: hsl(150.9 13.6% 52.4% / 80%);
  transform: translateX(-50%) translateY(-50%);

  background: rgba(94,123,115,0.5);
  border-radius: 16px;
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
  backdrop-filter: blur(5px);
  -webkit-backdrop-filter: blur(5px);
  border: 1px solid rgba(255, 255, 255, 0.3);
  z-index: var(--z-index-9998);
}

</style>
