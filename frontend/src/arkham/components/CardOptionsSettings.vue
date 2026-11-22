<script lang="ts" setup>
import { computed, nextTick, onMounted, onUnmounted, ref, watch } from 'vue';
import CardOptionGroups from '@/arkham/components/CardOptionGroups.vue';
import {
  cardOptionName,
  normalizeForSearch,
  useCardOptions,
  useConfigurableCards,
} from '@/arkham/composables/useCardOptions';
import type { Game } from '@/arkham/types/Game';

/* The "Card Options" section of the Settings pane. Deliberately fixed-height:
 * one card is configured at a time and the suggestion list floats, so the
 * section costs the same whether two cards have options or two hundred. */
const props = defineProps<{
  game: Game
  playerId: string
}>();

const cards = useConfigurableCards();
const selected = ref<string | null>(null);
const filter = ref('');
const focused = ref(false);
const highlighted = ref(0);

const matches = computed(() => {
  const term = normalizeForSearch(filter.value.trim());
  const pool = term
    ? cards.value.filter((c) => normalizeForSearch(c.name).includes(term))
    : cards.value;
  return pool.slice(0, 40);
});

const showSuggestions = computed(() => focused.value && matches.value.length > 0);

/* The list is teleported to body: anchored inside the pane it is clipped by
 * .settings-body's `overflow-y: auto`, which buried most of the suggestions
 * behind the scroll edge. Fixed coordinates are re-measured whenever the thing
 * it is glued to can have moved.
 *
 * Open state is just "the input has focus" -- no outside-click watcher. One
 * mounted mid-press raced the press that opened it, and clicking the Settings
 * header to drag the pane blurs the input anyway, so the list never detaches
 * from a window that moves. */
const inputRef = ref<HTMLElement | null>(null);
const listRef = ref<HTMLElement | null>(null);
const listPosition = ref<Record<string, string>>({});

const LIST_MAX_HEIGHT = 190;

function calculatePosition() {
  if (!inputRef.value) return;
  const rect = inputRef.value.getBoundingClientRect();
  const margin = 8;
  const height = Math.min(listRef.value?.scrollHeight ?? LIST_MAX_HEIGHT, LIST_MAX_HEIGHT);
  /* Prefer below the input; flip above when the viewport bottom would cut it off
   * and there is more room up there. */
  const below = window.innerHeight - rect.bottom - margin;
  const above = rect.top - margin;
  const flip = below < height && above > below;
  listPosition.value = {
    left: `${rect.left}px`,
    width: `${rect.width}px`,
    maxHeight: `${Math.max(80, Math.min(LIST_MAX_HEIGHT, flip ? above : below))}px`,
    ...(flip
      ? { bottom: `${window.innerHeight - rect.top + 4}px` }
      : { top: `${rect.bottom + 4}px` }),
  };
}

function reposition() {
  if (showSuggestions.value) calculatePosition();
}

watch(showSuggestions, (shown) => { if (shown) nextTick(calculatePosition) });

/* Typing changes how many rows there are, so the flip decision has to be redone. */
watch(matches, () => {
  highlighted.value = 0;
  nextTick(reposition);
});

onMounted(() => {
  window.addEventListener('resize', reposition);
  /* Capture, so scrolling .settings-body (not the window) is seen too. */
  window.addEventListener('scroll', reposition, true);
});

onUnmounted(() => {
  window.removeEventListener('resize', reposition);
  window.removeEventListener('scroll', reposition, true);
});

const selectedName = computed(() => (selected.value ? cardOptionName(selected.value) : ''));

function choose(cardCode: string) {
  selected.value = cardCode;
  filter.value = '';
  focused.value = false;
}

function clear() {
  selected.value = null;
  filter.value = '';
}

function onEnter() {
  const match = matches.value[highlighted.value];
  if (match) choose(match.cardCode);
}

function move(delta: number) {
  if (!showSuggestions.value) { focused.value = true; return }
  const n = matches.value.length;
  highlighted.value = (highlighted.value + delta + n) % n;
}

const { groups, label, isOn, valueOf, valuesOf, valueLabel, inputId, set } =
  useCardOptions(computed(() => props.game), computed(() => props.playerId), selected);
</script>

<template>
  <section v-if="cards.length > 0" class="settings-section">
    <h3 class="section-title">{{ $t('cardOption.title') }}</h3>

    <div class="card-options-filter">
      <input
        ref="inputRef"
        v-model="filter"
        type="text"
        class="card-options-input"
        :placeholder="$t('cardOption.filterPlaceholder')"
        role="combobox"
        aria-controls="card-options-suggestions"
        aria-autocomplete="list"
        :aria-expanded="showSuggestions"
        :aria-activedescendant="showSuggestions ? `card-options-suggestion-${highlighted}` : undefined"
        @focus="focused = true"
        @blur="focused = false"
        @input="focused = true"
        @keydown.down.prevent="move(1)"
        @keydown.up.prevent="move(-1)"
        @keydown.enter.prevent="onEnter"
        @keydown.esc.prevent="focused = false"
      />
    </div>

    <Teleport to="body">
      <!-- Buttons directly, no ul/li: a list's marker box and start padding are
           an indent to fight on an element teleported out of this component's
           scoped styles, and it buys nothing a listbox role doesn't say better. -->
      <div
        v-if="showSuggestions"
        id="card-options-suggestions"
        ref="listRef"
        class="card-options-suggestions"
        role="listbox"
        :style="listPosition"
      >
        <!-- mousedown.prevent: the press must not pull focus off the input, or
             the input blurs and this list unmounts before the click that would
             have chosen the card ever lands. -->
        <button
          v-for="(card, index) in matches"
          :key="card.cardCode"
          :id="`card-options-suggestion-${index}`"
          type="button"
          role="option"
          :aria-selected="index === highlighted"
          :class="{ highlighted: index === highlighted }"
          @mouseenter="highlighted = index"
          @mousedown.prevent
          @click="choose(card.cardCode)"
        >
          {{ card.name }}
        </button>
      </div>
    </Teleport>

    <div v-if="selected" class="card-options-selection">
      <div class="card-options-selection__head">
        <span class="card-options-selection__name">{{ selectedName }}</span>
        <button type="button" class="card-options-clear" @click="clear">
          {{ $t('cardOption.clear') }}
        </button>
      </div>
      <CardOptionGroups
        :groups="groups"
        :label="label"
        :isOn="isOn"
        :valueOf="valueOf"
        :valuesOf="valuesOf"
        :valueLabel="valueLabel"
        :inputId="inputId"
        :set="set"
      />
    </div>
  </section>
</template>

<style scoped>
/* Settings.vue's styles are scoped, so they don't reach a child component's
   elements — this section carries its own copies of the two it borrows. */
.settings-section {
  display: flex;
  flex-direction: column;
}

.section-title {
  margin: 0 0 10px;
  padding-bottom: 6px;
  font-family: Teutonic, serif;
  font-size: 13px;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--title);
  border-bottom: 1px solid var(--box-border);
}

.card-options-filter {
  position: relative;
}

.card-options-input {
  width: 100%;
  padding: 8px 12px;
  font: inherit;
  font-size: 13px;
  color: var(--text);
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
}

.card-options-input::placeholder {
  color: var(--background-light);
  opacity: 0.7;
}

.card-options-input:focus {
  outline: none;
  border-color: var(--background-mid);
}

.card-options-selection {
  margin-top: 10px;
}

.card-options-selection__head {
  display: flex;
  align-items: baseline;
  gap: 10px;
  margin-bottom: 6px;
}

.card-options-selection__name {
  font-family: Teutonic, serif;
  font-size: 14px;
  color: var(--title);
  letter-spacing: 0.02em;
}

.card-options-clear {
  margin-left: auto;
  padding: 0;
  border: 0;
  background: none;
  color: var(--background-light);
  font: inherit;
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  cursor: pointer;
}

.card-options-clear:hover {
  color: var(--text);
}
</style>

<style>
/* Unscoped: the list is teleported to body. Fixed, and above the Draggable the
   Settings pane lives in, so .settings-body's scroll clip can't swallow it.
   Position/width/max-height come from calculatePosition(). */
.card-options-suggestions {
  position: fixed;
  z-index: var(--z-modal-overlay);
  display: flex;
  flex-direction: column;
  gap: 2px;
  margin: 0;
  padding: 4px;
  overflow-y: auto;
  overscroll-behavior: contain;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  box-shadow: 0 8px 24px rgba(0, 0, 0, 0.5);
}

.card-options-suggestions button {
  display: block;
  width: 100%;
  padding: 7px 10px;
  border: 0;
  border-radius: 4px;
  background: none;
  color: var(--text);
  font: inherit;
  font-size: 13px;
  text-align: left;
  cursor: pointer;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.card-options-suggestions button.highlighted {
  background: var(--button-1);
  color: var(--text);
}
</style>
