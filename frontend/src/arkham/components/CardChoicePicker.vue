<script setup lang="ts">
import { computed, nextTick, reactive, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { useDbCardStore } from '@/stores/dbCards'
import { useCardStore } from '@/stores/cards'
import { cardImage, cardArt } from '@/arkham/cardImages'
import { customCardDef, isCustomCardCode } from '@/arkham/customCards'
import { cardText } from '@/arkham/customText'
import {
  filterCardChoices,
  type CardChoiceCandidate,
  type CardChoiceFilters,
} from '@/arkham/cardChoiceFilter'
import CardListView from '@/arkham/components/CardListView.vue'
import type { CardDef } from '@/arkham/types/CardDef'
import type { CardLabel } from '@/arkham/types/Message'

const props = defineProps<{
  entries: { choice: CardLabel; index: number }[]
  collection?: boolean
}>()
const emit = defineEmits<{ choose: [index: number] }>()
const { t, locale } = useI18n()
const db = useDbCardStore()
const cards = useCardStore()
const grid = ref<HTMLElement | null>(null)
const view = ref<'grid' | 'list'>('grid')
const defaults = (): CardChoiceFilters => ({
  query: '',
  className: '',
  type: '',
  level: '',
  trait: '',
  pack: '',
  includeCustom: !props.collection,
  sort: 'name',
})
const filters = reactive(defaults())
const defsByCode = computed(() => new Map(cards.cards.map((c) => [cardArt(c.cardCode), c])))
const candidates = computed<CardChoiceCandidate[]>(() =>
  props.entries.map(({ choice, index }) => {
    const code = cardArt(choice.cardCode)
    const printed = db.getDbCard(code)
    const def = customCardDef(code) ?? defsByCode.value.get(code)
    return {
      index,
      code,
      name: printed?.name ?? def?.name.title ?? code,
      subtitle: printed?.subname ?? def?.name.subtitle ?? '',
      classes: printed
        ? [printed.faction_name, printed.faction2_name, printed.faction3_name]
            .filter((v): v is string => Boolean(v))
            .map((v) => v.toLowerCase())
        : (def?.classSymbols ?? []).map((v) => v.toLowerCase()),
      type: printed?.type_code ?? def?.cardType.replace(/Type$/, '').toLowerCase() ?? '',
      level: def?.level ?? printed?.xp ?? null,
      traits: printed
        ? (printed.traits ?? printed.real_traits ?? '')
            .split('.')
            .map((v) => v.trim())
            .filter(Boolean)
        : (def?.cardTraits ?? []),
      text:
        printed?.text ??
        (def ? Object.values({ ...cardText(def), ...cardText(def, locale.value) }).join(' ') : ''),
      pack: printed?.pack_name ?? (isCustomCardCode(code) ? t('questionFilter.customCards') : ''),
      custom: isCustomCardCode(code),
    }
  }),
)
const options = (field: 'classes' | 'type' | 'traits' | 'pack') =>
  computed(() =>
    [...new Set(candidates.value.flatMap((c) => c[field]).filter(Boolean))].sort((a, b) =>
      a.localeCompare(b),
    ),
  )
const classes = options('classes')
const types = options('type')
const traits = options('traits')
const packs = options('pack')
const levels = computed(() =>
  [...new Set(candidates.value.map((c) => c.level).filter((v): v is number => v !== null))].sort(
    (a, b) => a - b,
  ),
)
const filtered = computed(() => filterCardChoices(candidates.value, filters))
const listCards = computed<CardDef[]>(() =>
  filtered.value.map((card) => {
    const def = customCardDef(card.code) ?? defsByCode.value.get(card.code)
    return def
      ? { ...def, name: { title: card.name, subtitle: card.subtitle || null } }
      : {
          cardCode: card.code,
          art: card.code,
          name: { title: card.name, subtitle: card.subtitle || null },
          classSymbols: card.classes,
          cardType: `${card.type.charAt(0).toUpperCase()}${card.type.slice(1)}Type`,
          level: card.level,
          cardTraits: card.traits,
          skills: [],
          cost: null,
          doubleSided: false,
          otherSide: null,
          meta: {},
          errata: null,
        }
  }),
)
function chooseListCard(index: number) {
  const card = filtered.value[index]
  if (card) emit('choose', card.index)
}
const hasFilters = computed(
  () =>
    filters.query ||
    filters.className ||
    filters.type ||
    filters.level ||
    filters.trait ||
    filters.pack,
)
const customCount = computed(() => candidates.value.filter((c) => c.custom).length)
watch(filtered, () => {
  void nextTick(() => grid.value?.scrollTo(0, 0))
})
watch(
  () => props.entries,
  () => {
    Object.assign(filters, defaults())
  },
)
function reset() {
  Object.assign(filters, defaults())
}
</script>

<template>
  <section
    class="card-picker"
    :aria-label="t(props.collection ? 'questionFilter.collectionTitle' : 'questionFilter.title')"
    @keydown.stop
  >
    <header class="picker-heading">
      <div>
        <h3>
          {{ t(props.collection ? 'questionFilter.collectionTitle' : 'questionFilter.title') }}
        </h3>
        <p aria-live="polite">
          {{ t('questionFilter.results', { count: filtered.length, total: candidates.length }) }}
        </p>
      </div>
      <button v-if="hasFilters" type="button" class="text-button" @click="reset">
        {{ t('questionFilter.clear') }}
      </button>
    </header>
    <div class="picker-tools">
      <label class="picker-search"
        ><span>{{ t('questionFilter.search') }}</span
        ><input
          v-model="filters.query"
          type="search"
          :placeholder="t('questionFilter.searchPlaceholder')"
      /></label>
      <label
        ><span>{{ t('questionFilter.sort') }}</span
        ><select v-model="filters.sort">
          <option value="name">{{ t('questionFilter.name') }}</option>
          <option value="level">{{ t('questionFilter.level') }}</option>
          <option value="original">{{ t('questionFilter.original') }}</option>
        </select></label
      >
    </div>
    <div class="picker-filters">
      <label
        ><span>{{ t('questionFilter.class') }}</span
        ><select v-model="filters.className">
          <option value="">{{ t('questionFilter.allClasses') }}</option>
          <option v-for="value in classes" :key="value" :value="value">{{ value }}</option>
        </select></label
      >
      <label v-if="types.length > 1"
        ><span>{{ t('questionFilter.type') }}</span
        ><select v-model="filters.type">
          <option value="">{{ t('questionFilter.allTypes') }}</option>
          <option v-for="value in types" :key="value" :value="value">{{ value }}</option>
        </select></label
      >
      <label v-if="levels.length > 1"
        ><span>{{ t('questionFilter.level') }}</span
        ><select v-model="filters.level">
          <option value="">{{ t('questionFilter.allLevels') }}</option>
          <option v-for="value in levels" :key="value" :value="String(value)">{{ value }}</option>
        </select></label
      >
      <label
        ><span>{{ t('questionFilter.trait') }}</span
        ><select v-model="filters.trait">
          <option value="">{{ t('questionFilter.allTraits') }}</option>
          <option v-for="value in traits" :key="value" :value="value">{{ value }}</option>
        </select></label
      >
      <label
        ><span>{{ t('questionFilter.pack') }}</span
        ><select v-model="filters.pack">
          <option value="">{{ t('questionFilter.allPacks') }}</option>
          <option v-for="value in packs" :key="value" :value="value">{{ value }}</option>
        </select></label
      >
    </div>
    <div class="picker-source">
      <label class="custom-toggle"
        ><input v-model="filters.includeCustom" type="checkbox" /><span>{{
          t('questionFilter.includeCustom')
        }}</span
        ><span class="source-count">{{ customCount }}</span></label
      >
      <div class="view-toggle" role="group" :aria-label="t('questionFilter.view')">
        <button
          type="button"
          :aria-label="t('questionFilter.gridView')"
          :title="t('questionFilter.gridView')"
          :aria-pressed="view === 'grid'"
          @click="view = 'grid'"
        >
          <svg width="16" height="16" viewBox="0 0 20 20" fill="currentColor" aria-hidden="true">
            <path d="M2 2h6v6H2zm10 0h6v6h-6zM2 12h6v6H2zm10 0h6v6h-6z" />
          </svg>
        </button>
        <button
          type="button"
          :aria-label="t('questionFilter.listView')"
          :title="t('questionFilter.listView')"
          :aria-pressed="view === 'list'"
          @click="view = 'list'"
        >
          <svg width="16" height="16" viewBox="0 0 20 20" fill="currentColor" aria-hidden="true">
            <path d="M2 3h3v3H2zm5 0h11v3H7zM2 9h3v3H2zm5 0h11v3H7zM2 15h3v3H2zm5 0h11v3H7z" />
          </svg>
        </button>
      </div>
    </div>
    <div
      v-if="filtered.length"
      ref="grid"
      class="picker-results"
      :class="{ 'picker-grid': view === 'grid' }"
    >
      <CardListView
        v-if="view === 'list'"
        :cards="listCards"
        :show-counts="false"
        selectable
        @choose="chooseListCard"
      />
      <template v-else>
        <button
          v-for="card in filtered"
          :key="card.index"
          type="button"
          class="picker-card"
          :aria-label="t('questionFilter.choose', { name: card.name })"
          @click="emit('choose', card.index)"
        >
          <img class="card" :src="cardImage(card.code)" :alt="card.name" loading="lazy" />
        </button>
      </template>
    </div>
    <div v-else class="picker-empty">
      <strong>{{ t('questionFilter.noResults') }}</strong>
      <p>{{ t('questionFilter.tryFilters') }}</p>
      <button type="button" class="text-button" @click="reset">
        {{ t('questionFilter.clear') }}
      </button>
    </div>
  </section>
</template>

<style scoped lang="scss">
.card-picker {
  display: block;
  width: 100%;
  max-width: 100%;
  padding: 12px;
  box-sizing: border-box;
  color: var(--text);
  text-align: left;
  font-family: 'Noto Sans', sans-serif;
  background: rgba(24, 31, 35, 0.93);
  color: #eee8d8;
}
.picker-heading {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 16px;
  margin-bottom: 8px;
  > div {
    display: flex;
    align-items: baseline;
    gap: 12px;
    flex-wrap: wrap;
  }
  h3 {
    color: var(--title);
    font-family: 'Teutonic', serif;
    font-size: 1.25rem;
    font-weight: normal;
    margin: 0;
  }
  p {
    margin: 0;
    opacity: 0.7;
    font-size: 0.8rem;
    font-variant-numeric: tabular-nums;
  }
}
.picker-tools {
  display: grid;
  grid-template-columns: minmax(0, 1fr) 150px;
  gap: 8px;
}
.picker-filters {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(110px, 1fr));
  gap: 8px;
  margin-top: 8px;
}
.picker-tools label > span,
.picker-filters label > span {
  position: absolute;
  width: 1px;
  height: 1px;
  overflow: hidden;
  clip-path: inset(50%);
  white-space: nowrap;
}
label {
  display: flex;
  flex-direction: column;
  gap: 5px;
  min-width: 0;
  > span {
    font-size: 0.72rem;
    opacity: 0.85;
  }
}
input[type='search'],
select {
  width: 100%;
  min-width: 0;
  min-height: 32px;
  box-sizing: border-box;
  margin: 0;
  padding: 6px 8px;
  border: 1px solid rgba(214, 205, 174, 0.3);
  border-radius: 4px;
  background: #20252a;
  color: #eee8d8;
  font: inherit;
  font-size: 0.82rem;
}
select option {
  background: #20252a;
  color: #eee8d8;
}
input:focus-visible,
select:focus-visible,
button:focus-visible {
  outline: 2px solid var(--select);
  outline-offset: 3px;
}
.picker-source {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  padding: 8px 0;
  margin-bottom: 8px;
  border-bottom: 1px solid rgba(214, 205, 174, 0.2);
  > span {
    font-size: 0.72rem;
    opacity: 0.65;
  }
}
.custom-toggle {
  flex-direction: row;
  align-items: center;
  cursor: pointer;
  gap: 8px;
  input {
    accent-color: var(--select);
    width: 16px;
    height: 16px;
    margin: 0;
  }
}
.source-count {
  font-variant-numeric: tabular-nums;
  opacity: 0.55;
}
.picker-results {
  max-height: 50dvh;
  overflow: auto;
}
.picker-grid {
  padding: 4px;
  display: grid;
  grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: 20px 14px;
}
.picker-card {
  display: flex;
  flex-direction: column;
  align-items: stretch;
  text-align: left;
  padding: 0;
  margin: 0;
  color: inherit;
  background: transparent;
  border: 0;
  cursor: pointer;
  min-width: 0;
  transition: transform 0.15s ease;
  &:hover {
    transform: translateY(-3px);
  }
  &:active {
    transform: translateY(0);
  }
  img {
    width: 100%;
    height: auto;
    aspect-ratio: 419 / 595;
    object-fit: contain;
    border-radius: 7px;
  }
}
.view-toggle {
  display: flex;
  gap: 2px;
  button {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 32px;
    height: 30px;
    padding: 0;
    color: #eee8d8;
    background: transparent;
    border: 1px solid transparent;
    border-radius: 3px;
    cursor: pointer;
    opacity: 0.6;
    &[aria-pressed='true'] {
      background: rgba(214, 205, 174, 0.12);
      border-color: rgba(214, 205, 174, 0.3);
      opacity: 1;
    }
    &:hover {
      opacity: 1;
    }
  }
}
.text-button {
  padding: 5px 0;
  border: 0;
  background: transparent;
  color: var(--title);
  text-decoration: underline;
  text-underline-offset: 3px;
  cursor: pointer;
}
.picker-empty {
  padding: 32px 12px;
  text-align: center;
  p {
    font-size: 0.85rem;
    opacity: 0.7;
  }
}

@media (max-width: 650px) {
  .card-picker {
    padding: 12px;
    width: 100%;
  }
  .picker-tools {
    grid-template-columns: minmax(0, 1fr) 120px;
  }
  .picker-filters {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
  .picker-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 16px 10px;
  }
  .picker-source {
    align-items: center;
    flex-direction: row;
    gap: 6px;
  }
}
</style>
