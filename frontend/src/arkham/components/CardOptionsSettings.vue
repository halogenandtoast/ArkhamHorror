<script lang="ts" setup>
import { computed, ref, watch } from 'vue';
import { OnClickOutside } from '@vueuse/components';
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

watch(matches, () => { highlighted.value = 0 });

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

    <OnClickOutside @trigger="focused = false">
      <div class="card-options-filter">
        <input
          v-model="filter"
          type="text"
          class="card-options-input"
          :placeholder="$t('cardOption.filterPlaceholder')"
          @focus="focused = true"
          @keydown.down.prevent="move(1)"
          @keydown.up.prevent="move(-1)"
          @keydown.enter.prevent="onEnter"
          @keydown.esc.prevent="focused = false"
        />

        <ul v-if="showSuggestions" class="card-options-suggestions">
          <li v-for="(card, index) in matches" :key="card.cardCode">
            <button
              type="button"
              :class="{ highlighted: index === highlighted }"
              @mouseenter="highlighted = index"
              @click="choose(card.cardCode)"
            >
              {{ card.name }}
            </button>
          </li>
        </ul>
      </div>
    </OnClickOutside>

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

/* Floats over the pane rather than pushing it taller, so the section's height
   never depends on how many cards declare options. */
.card-options-suggestions {
  position: absolute;
  z-index: 2;
  top: calc(100% + 4px);
  left: 0;
  right: 0;
  display: flex;
  flex-direction: column;
  gap: 2px;
  margin: 0;
  padding: 4px;
  list-style: none;
  max-height: 190px;
  overflow-y: auto;
  overscroll-behavior: contain;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  box-shadow: 0 8px 24px rgba(0, 0, 0, 0.5);
}

/* The app styles `li` globally with a left padding and margin; without this the
   highlight sits lopsided in the list. */
.card-options-suggestions li {
  margin: 0;
  padding: 0;
  list-style: none;
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
