<script lang="ts" setup>
/* The search box and the chips under it, over whatever pile of cards it is
 * given.
 *
 * The chips are built from the cards themselves rather than from a fixed list,
 * so a set of locations is not asked whether you meant investigators, and the
 * count on each chip is what picking it would leave -- a chip that would empty
 * the list says 0 before you click it.
 *
 * Two shapes, because it is used in two widths: the page above a list of sets,
 * where the chips can lie open; and the 268px panel inside a set, where they
 * live behind a button that says how many are lit.
 */
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import FilterBar from '@/arkham/components/FilterBar.vue'
import type { CustomCard } from '@/arkham/customCards'
import {
  chosenFacetCount,
  clearFacets,
  facetsFor,
  toggleFacet,
  type CardFilter,
  type Facet,
  type FacetKey,
} from '@/arkham/customCardFilters'

const { t } = useI18n()
const K = 'customCardSets.'

const filter = defineModel<CardFilter>({ required: true })

const props = withDefaults(
  defineProps<{
    /** The cards the chips are built from, and counted against. */
    cards: CustomCard[]
    placeholder: string
    /** The narrow shape: chips behind a button, stacked one group per row. */
    compact?: boolean
  }>(),
  { compact: false },
)

const facets = computed(() => facetsFor(props.cards, filter.value))
const chosen = computed(() => chosenFacetCount(filter.value))

/* Open on its own once something is picked, so a filter carried in from the
 * other half of the builder is never doing its work out of sight. */
const open = ref(chosen.value > 0)
watch(chosen, (n) => {
  if (n > 0) open.value = true
})

const isOn = (key: FacetKey, value: string) => filter.value[key].includes(value)
const toggle = (key: FacetKey, value: string) => toggleFacet(filter.value, key, value)

/* Traits run to the dozens once a library has a few sets in it, and five rows
 * of chips push the thing you are filtering off the screen. Collapsed shows the
 * ones most of the cards actually have -- picked by count, then listed in the
 * facet's own order, so expanding adds chips rather than reshuffling them. A
 * chip that is lit is always among them, since a filter you cannot see is a
 * filter you cannot turn off. */
const CAP = computed(() => (props.compact ? 10 : 18))
const expanded = ref(new Set<FacetKey>())

function shownValues(facet: Facet) {
  if (expanded.value.has(facet.key) || facet.values.length <= CAP.value) return facet.values
  const keep = new Set(
    [...facet.values]
      .sort((a, b) => b.count - a.count)
      .slice(0, CAP.value)
      .map((v) => v.value),
  )
  return facet.values.filter((v) => keep.has(v.value) || isOn(facet.key, v.value))
}

function toggleExpanded(key: FacetKey) {
  const next = new Set(expanded.value)
  if (next.has(key)) next.delete(key)
  else next.add(key)
  expanded.value = next
}

/* The class chips are coloured and iconed the way the game colours them; the
 * other facets are plain. A class the list does not know gets neither. */
const CLASSES = ['Guardian', 'Seeker', 'Rogue', 'Mystic', 'Survivor', 'Neutral']
const classKey = (value: string) =>
  CLASSES.includes(value) ? value.toLowerCase() : null
</script>

<template>
  <div class="card-filters" :class="{ compact: props.compact }">
    <FilterBar
      v-model="filter.query"
      :placeholder="props.placeholder"
      :clear-label="t(`${K}clearFilter`)"
    >
      <slot />
      <!-- Shown for as long as there are cards to filter, rather than only
           while there is something to filter by: a button that comes and goes
           as you type is one you cannot aim at. Nothing left to narrow -- a
           search that found nothing -- leaves it there and dead. -->
      <button
        v-if="props.cards.length"
        type="button"
        class="facet-toggle"
        :class="{ on: open || chosen > 0 }"
        :disabled="!facets.length"
        :aria-expanded="open"
        @click="open = !open"
      >
        <font-awesome-icon icon="filter" />
        <span>{{ t(`${K}facetsLabel`) }}</span>
        <small v-if="chosen">{{ chosen }}</small>
      </button>
    </FilterBar>

    <div v-if="open && facets.length" class="facets">
      <div v-for="facet in facets" :key="facet.key" class="facet">
        <h4>{{ t(`${K}facet.${facet.key}`) }}</h4>
        <div class="chips">
          <button
            v-for="value in shownValues(facet)"
            :key="value.value"
            type="button"
            class="chip"
            :class="[
              { on: isOn(facet.key, value.value), empty: !value.count },
              facet.key === 'classes' ? classKey(value.value) : null,
            ]"
            :aria-pressed="isOn(facet.key, value.value)"
            @click="toggle(facet.key, value.value)"
          >
            <span
              v-if="facet.key === 'classes' && classKey(value.value)"
              :class="`${classKey(value.value)}-icon`"
              aria-hidden="true"
            ></span>
            <span class="chip-label">{{ value.label }}</span>
            <small>{{ value.count }}</small>
          </button>
          <button
            v-if="facet.values.length > CAP"
            type="button"
            class="chip more"
            @click="toggleExpanded(facet.key)"
          >
            {{ expanded.has(facet.key)
              ? t(`${K}facetShowFewer`)
              : t(`${K}facetShowAll`, { n: facet.values.length }) }}
          </button>
        </div>
      </div>

      <button v-if="chosen" type="button" class="clear-facets" @click="clearFacets(filter)">
        <font-awesome-icon icon="times" />
        {{ t(`${K}clearFacets`) }}
      </button>
    </div>
  </div>
</template>

<style scoped lang="scss">
/* FilterBar carries its own bottom margin, which is the gap to whatever comes
   next. With the chips open that gap belongs under them instead. */
.card-filters:has(.facets) :deep(.filter-bar) {
  margin-bottom: 0.5rem;
}

.facet-toggle {
  align-items: center;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  color: var(--title);
  cursor: pointer;
  display: flex;
  font-size: 0.75rem;
  font-weight: 600;
  gap: 0.35rem;
  letter-spacing: 0.04em;
  padding: 0 0.6rem;
  text-transform: uppercase;
  /* Matched to the search field beside it, which is 40px tall. */
  min-height: 40px;
  white-space: nowrap;

  &:hover {
    border-color: var(--spooky-green);
  }

  &.on {
    border-color: var(--spooky-green);
    color: var(--spooky-green);
  }

  &:disabled {
    cursor: default;
    opacity: 0.4;

    &:hover {
      border-color: var(--box-border);
    }
  }

  small {
    background: var(--spooky-green);
    border-radius: 999px;
    color: var(--background-dark);
    font-size: 0.65rem;
    font-variant-numeric: tabular-nums;
    padding: 0.05rem 0.35rem;
  }
}

.facets {
  background: rgba(0, 0, 0, 0.18);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem 1.2rem;
  margin-bottom: 0.9rem;
  padding: 0.6rem 0.7rem;
}

.facet {
  min-width: 0;

  h4 {
    font-size: 0.65rem;
    letter-spacing: 0.08em;
    margin: 0 0 0.3rem;
    opacity: 0.55;
    text-transform: uppercase;
  }
}

.chips {
  display: flex;
  flex-wrap: wrap;
  gap: 0.3rem;
}

/* `padding: 0 .5rem` rather than the global button padding, which is 11px a
   side and makes a row of these twice as wide as it needs to be. */
.chip {
  align-items: center;
  background: rgba(0, 0, 0, 0.3);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 4px;
  color: rgba(255, 255, 255, 0.72);
  cursor: pointer;
  display: flex;
  font-size: 0.75rem;
  font-weight: 600;
  gap: 0.3rem;
  padding: 0.25rem 0.5rem;

  &:hover {
    border-color: rgba(255, 255, 255, 0.25);
    color: #fff;
  }

  /* Nothing left to find down this road. Still clickable -- it is how a chip
     that is already on gets turned off again. */
  &.empty:not(.on) {
    opacity: 0.45;
  }

  &.on {
    background: var(--button-1);
    border-color: var(--spooky-green);
    color: #fff;
  }

  &.on.guardian { background: var(--guardian-extra-dark); border-color: var(--guardian-dark); }
  &.on.seeker   { background: var(--seeker-extra-dark);   border-color: var(--seeker-dark); }
  &.on.rogue    { background: var(--rogue-extra-dark);    border-color: var(--rogue-dark); }
  &.on.mystic   { background: var(--mystic-extra-dark);   border-color: var(--mystic-dark); }
  &.on.survivor { background: var(--survivor-extra-dark); border-color: var(--survivor-dark); }
  &.on.neutral  { background: var(--neutral-extra-dark);  border-color: var(--neutral-dark); }

  .chip-label {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  small {
    font-size: 0.65rem;
    font-variant-numeric: tabular-nums;
    opacity: 0.6;
  }

  /* The way into the rest of them, not one of them: no border, so it does not
     read as a trait called "show all 58". */
  &.more {
    background: none;
    border-color: transparent;
    opacity: 0.75;
    text-decoration: underline;
  }

  span[class$='-icon'] {
    font-size: 0.9em;
  }
}

.clear-facets {
  align-self: flex-end;
  background: none;
  border: none;
  color: var(--title);
  cursor: pointer;
  font-size: 0.72rem;
  margin-left: auto;
  opacity: 0.7;
  padding: 0.2rem 0;

  &:hover {
    opacity: 1;
  }
}

/* The panel inside a set: one group per row, and the whole thing capped so a
   set with two hundred traits does not push the card list off the screen. */
.card-filters.compact {
  /* The search and the button that narrows it are one control, so they share a
     row. The bar wraps by default, which in a panel this width put the toggle
     on a line of its own -- a second row of chrome above a list that is the
     whole point of the panel. */
  :deep(.filter-bar) {
    flex-wrap: nowrap;
    gap: 0.4rem;
  }

  :deep(.field) {
    flex: 1 1 6rem;
  }

  .facets {
    flex-direction: column;
    /* Column + wrap + a capped height is a multi-column layout, which in a
       268px panel put LEVEL in a second column off the right-hand edge. */
    flex-wrap: nowrap;
    gap: 0.5rem;
    margin-bottom: 0.6rem;
    /* No scroller of its own: the panel around it already has one, and a box
       that scrolls inside a box that scrolls inside a list that scrolls is
       three ways to lose your place. The per-facet cap is what keeps it
       short. */
  }

  .facet-toggle {
    min-height: 32px;
    padding: 0 0.5rem;
  }

  .clear-facets {
    align-self: flex-start;
    margin-left: 0;
  }
}
</style>
