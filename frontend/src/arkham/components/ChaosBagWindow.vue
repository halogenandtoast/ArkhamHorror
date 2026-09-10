<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import Draggable from '@/components/Draggable.vue'
import SegmentedToggle from '@/components/SegmentedToggle.vue'
import { chaosTokenImage, compareTokenFaces } from '@/arkham/types/ChaosToken'
import { readTokenBag } from '@/arkham/types/TokenBag'
import type { Game } from '@/arkham/types/Game'
import { fullName } from '@/arkham/types/Name'
import { asCardCode } from '@/arkham/types/Card'
import { useCardStore } from '@/stores/cards'
import { scenarioTokenBagsFor, useDebug } from '@/arkham/debug'
import TokenBagDebug from '@/arkham/components/debug/TokenBag.vue'

const props = defineProps<{ game: Game }>()
const emit = defineEmits<{ close: [] }>()
const { t } = useI18n()
const cards = useCardStore()
const debug = useDebug()
const selected = ref('chaos')
const bags = computed(() => [
  ...(props.game.scenario ? scenarioTokenBagsFor(props.game.scenario).map(entry => ({
    id: `scenario:${entry.key}`, label: entry.label, bag: entry.bag, bagKey: entry.key, storyId: undefined,
  })) : []),
  ...Object.values(props.game.stories).flatMap(story => {
    const bag = readTokenBag(story.meta)
    if (!bag) return []
    const instance = props.game.cards[story.cardId]
    const code = instance ? asCardCode(instance) : story.art
    const card = cards.cards.find(card => card.cardCode === code)
    const label = code === '06078' || code === 'c06078' ? 'Infestation Bag' : card ? fullName(card.name) : code
    return [{ id: `story:${story.id}`, label, bag, bagKey: undefined, storyId: story.id }]
  }),
])
const active = computed(() => bags.value.find(bag => bag.id === selected.value))
const displayedTokens = computed(() => [...(active.value?.bag.tokens ?? [])].sort((a, b) => compareTokenFaces(a.face, b.face)))
const regularCount = computed(() => props.game.scenario?.chaosBag.chaosTokens.length ?? 0)
const count = computed(() => active.value?.bag.tokens.length ?? regularCount.value)
const title = computed(() => active.value?.label ?? t('gameBar.chaosBag'))
const bagOptions = computed(() => [
  { value: 'chaos', label: t('gameBar.chaosBag'), count: regularCount.value },
  ...bags.value.map(entry => ({ value: entry.id, label: entry.label, count: entry.bag.tokens.length })),
])
// A bag can disappear during play or undo; never leave an orphaned selection.
watch(bags, entries => {
  if (selected.value !== 'chaos' && !entries.some(entry => entry.id === selected.value)) selected.value = 'chaos'
})
watch(() => props.game.id, () => { selected.value = 'chaos' })
</script>

<template>
  <Draggable preserve-width preserve-position>
    <template #handle><header><h2>{{ title }} <span class="count-pill">{{ count }}</span></h2></header></template>
    <div class="bag-window">
      <div v-if="bagOptions.length > 1" class="bag-switcher">
        <SegmentedToggle v-model="selected" :options="bagOptions" :label="t('gameBar.chaosBag')" />
      </div>
      <div v-if="active" class="custom-bag">
        <img v-for="token in displayedTokens" :key="token.id" class="bag-token"
          :src="chaosTokenImage(token.face)" :alt="token.face" :title="token.face" />
      </div>
      <slot v-else />
      <section v-if="active && debug.active" :key="active.id" class="bag-debug">
        <h3>Debug bag controls</h3>
        <TokenBagDebug :game-id="game.id" :bag="active.bag" :bag-key="active.bagKey" :story-id="active.storyId" />
      </section>
    </div>
    <button class="button close-button" @click="emit('close')">{{ t('close') }}</button>
  </Draggable>
</template>

<style scoped>
.bag-window { max-height: 70dvh; overflow-y: auto; scrollbar-gutter: stable; }
.bag-debug { padding: 12px; background: var(--box-background); border-top: 1px solid var(--box-border); }
.bag-debug h3 { margin: 0 0 14px; color: var(--title); font-size: .8rem; font-weight: 600; }
.bag-switcher { padding: 10px; background: var(--background-dark); border-bottom: 1px solid var(--box-border); }
.custom-bag { display: flex; justify-content: center; align-items: center; flex-wrap: wrap; gap: 5px; padding: 10px; min-height: 70px; background: rgba(0, 0, 0, .5); }
.bag-token { width: 50px; height: 50px; margin-bottom: 10px; border: 1px solid rgba(255, 255, 255, .4); border-radius: 50%; box-shadow: 0 4px 4px rgba(0, 0, 0, .5); transition: transform .2s; }
.bag-token:hover { transform: scale(1.2); }
.close-button { display: block; width: 100%; min-height: 44px; padding: 12px 20px; margin: 0; border: 0; border-top: 1px solid var(--box-border); border-radius: 0; background: var(--button); color: var(--text); font: inherit; font-size: .85rem; font-weight: 600; text-transform: uppercase; letter-spacing: .06em; cursor: pointer; transition: background .15s; }
.close-button:hover { background: var(--button-highlight); }
.close-button:focus-visible { outline: 2px solid var(--spooky-green); outline-offset: -3px; }
</style>
