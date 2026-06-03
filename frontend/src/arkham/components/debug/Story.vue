<script lang="ts" setup>
import Draggable from '@/components/Draggable.vue'
import { computed } from 'vue'
import * as Arkham from '@/arkham/types/Story'
import { ChaosToken, chaosTokenImage } from '@/arkham/types/ChaosToken'

const props = defineProps<{
  story: Arkham.Story
}>()

const emit = defineEmits<{ close: [] }>()

type BagSection = { label: string, tokens: ChaosToken[] }

const tokenOrder = ['PlusOne', 'Zero', 'MinusOne', 'MinusTwo', 'MinusThree', 'MinusFour', 'MinusFive', 'MinusSix', 'MinusSeven', 'MinusEight', 'Skull', 'Cultist', 'Tablet', 'ElderThing', 'AutoFail', 'ElderSign', 'CurseToken', 'BlessToken', 'FrostToken']

function sortTokens(a: ChaosToken, b: ChaosToken) {
  return tokenOrder.indexOf(a.face) - tokenOrder.indexOf(b.face)
}

const sections = computed<BagSection[]>(() => {
  const meta = props.story.meta
  if (!meta) return []

  const result: BagSection[] = []

  if (meta.predationTokens || meta.predationSetAside || meta.predationCurrentToken) {
    if (meta.predationTokens && meta.predationTokens.length > 0) {
      result.push({
        label: 'In Bag',
        tokens: meta.predationTokens.map(Arkham.predationAsChaosToken).slice().sort(sortTokens),
      })
    }
    if (meta.predationCurrentToken) {
      result.push({
        label: 'Current Reveal',
        tokens: [Arkham.predationAsChaosToken(meta.predationCurrentToken)],
      })
    }
    if (meta.predationSetAside && meta.predationSetAside.length > 0) {
      result.push({
        label: 'Set Aside',
        tokens: meta.predationSetAside.map(Arkham.predationAsChaosToken).slice().sort(sortTokens),
      })
    }
  }

  if (meta.infestationTokens || meta.infestationSetAside || meta.infestationCurrentToken) {
    if (meta.infestationTokens && meta.infestationTokens.length > 0) {
      result.push({
        label: 'In Bag',
        tokens: meta.infestationTokens.map(Arkham.infestationAsChaosToken).slice().sort(sortTokens),
      })
    }
    if (meta.infestationCurrentToken) {
      result.push({
        label: 'Current Reveal',
        tokens: [Arkham.infestationAsChaosToken(meta.infestationCurrentToken)],
      })
    }
    if (meta.infestationSetAside && meta.infestationSetAside.length > 0) {
      result.push({
        label: 'Set Aside',
        tokens: meta.infestationSetAside.map(Arkham.infestationAsChaosToken).slice().sort(sortTokens),
      })
    }
  }

  return result
})
</script>

<template>
  <Draggable>
    <template #handle><h2>{{ $t('debug.story.title') }}</h2></template>
    <div class="bag-container">
      <div class="bag">
        <p v-if="sections.length === 0" class="empty">{{ $t('debug.story.empty') }}</p>
        <div v-for="section in sections" :key="section.label" class="section">
          <h3>{{ section.label }} ({{ section.tokens.length }})</h3>
          <div class="token-preview">
            <img
              v-for="token in section.tokens"
              :key="token.id"
              class="token"
              :src="chaosTokenImage(token.face)"
              :title="token.face"
            />
          </div>
        </div>
      </div>
      <button class="button close" @click="emit('close')">{{ $t('debug.common.close') }}</button>
    </div>
  </Draggable>
</template>

<style scoped>
.bag-container {
  display: flex;
  flex-direction: column;
  min-width: 280px;
}

.bag {
  padding: 10px;
  background: rgba(0, 0, 0, 0.5);
  display: flex;
  flex-direction: column;
  gap: 10px;
  flex: 1;
}

button.close {
  border: 0;
  padding: 10px;
  text-transform: uppercase;
  background-color: #532e61;
  font-weight: bold;
  border-radius: 0.6em;
  border-top-left-radius: 0;
  border-top-right-radius: 0;
  color: #EEE;
  width: 100%;
  cursor: pointer;
  &:hover {
    background-color: #4d2b61;
  }
}

.section {
  display: flex;
  flex-direction: column;
  gap: 6px;
}

.section h3 {
  margin: 0;
  font-size: 0.9em;
  color: white;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  border-bottom: 1px solid rgba(255, 255, 255, 0.3);
  padding-bottom: 4px;
}

.token-preview {
  display: flex;
  gap: 5px;
  flex-wrap: wrap;
  flex-direction: row;
  justify-content: center;

  img {
    width: 30px;
    height: auto;
    transition: transform 0.2s;
    border: 1px solid rgba(255, 255, 255, 0.4);
    border-radius: 30px;
    box-shadow: 0 4px 4px rgba(0, 0, 0, 0.5);
    &:hover {
      transform: scale(1.2);
    }
  }
}

.empty {
  margin: 0;
  font-style: italic;
  color: white;
  opacity: 0.8;
}
</style>
