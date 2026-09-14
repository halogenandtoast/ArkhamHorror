<script lang="ts" setup>
import type { CardOption } from '@/arkham/types/CardDef';
import type { OptionGroup } from '@/arkham/composables/useCardOptions';

/* The settings themselves, grouped by ability. Shared by the on-card popover
 * and the Settings pane so the two can never drift. */
defineProps<{
  groups: OptionGroup[]
  /* The on-card popover is much narrower than the Settings pane, so it asks for
   * tighter rows. Everywhere else matches Settings.vue exactly. */
  compact?: boolean
  label: (option: CardOption) => string
  isOn: (option: CardOption) => boolean
  valueOf: (option: CardOption) => boolean | string
  valuesOf: (option: CardOption) => string[]
  valueLabel: (option: CardOption, value: string) => string
  inputId: (option: CardOption, value: string) => string
  set: (option: CardOption, value: boolean | string) => void
}>();
</script>

<template>
  <div
    v-for="group in groups"
    :key="group.ability ?? 'card'"
    class="card-option-group"
    :class="{ 'card-option-group--compact': compact }"
  >
    <div v-if="group.text" class="card-option-ability" v-html="group.text" />
    <div class="toggle-list">
      <div v-for="option in group.options" :key="option.key" class="toggle-row">
        <div class="toggle-text">
          <div class="toggle-name">{{ $t(label(option)) }}</div>
        </div>

        <div v-if="option.type.tag === 'toggle'" class="segmented toggle-control">
          <input
            type="radio"
            :id="inputId(option, 'on')"
            :name="inputId(option, 'group')"
            :checked="isOn(option)"
            @change="set(option, true)"
          />
          <label :for="inputId(option, 'on')">{{ $t('On') }}</label>
          <input
            type="radio"
            :id="inputId(option, 'off')"
            :name="inputId(option, 'group')"
            :checked="!isOn(option)"
            @change="set(option, false)"
          />
          <label :for="inputId(option, 'off')">{{ $t('Off') }}</label>
        </div>

        <div v-else class="segmented toggle-control">
          <template v-for="value in valuesOf(option)" :key="value">
            <input
              type="radio"
              :id="inputId(option, value)"
              :name="inputId(option, 'group')"
              :checked="valueOf(option) === value"
              @change="set(option, value)"
            />
            <label :for="inputId(option, value)">{{ valueLabel(option, value) }}</label>
          </template>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* Mirrors arkham/components/Settings.vue's toggle vocabulary so card options
   read as the same kind of thing as game settings, wherever they are shown.
   Scoped here rather than in either host: a parent's scoped styles don't reach
   a child component's inner elements, so the component has to carry its own. */
.card-option-group + .card-option-group {
  margin-top: 6px;
}

.toggle-list {
  display: flex;
  flex-direction: column;
  gap: 6px;
}


.toggle-row {
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 16px;
  align-items: center;
  padding: 10px 14px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
}


.toggle-row:hover {
  background: var(--background-mid);
}


.toggle-text {
  min-width: 0;
}


.toggle-name {
  font-size: 14px;
  font-weight: 500;
  color: var(--text);
}


.toggle-control {
  min-width: 150px;
  flex-shrink: 0;
  justify-self: end;
}


.segmented {
  display: grid;
  grid-auto-flow: column;
  grid-auto-columns: 1fr;
  border-radius: 5px;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  padding: 2px;
  gap: 2px;
}


.segmented input[type='radio'] {
  display: none;
}


.segmented label {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 6px 8px;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  font-size: 11px;
  font-weight: 600;
  white-space: nowrap;
  user-select: none;
  cursor: pointer;
  border-radius: 3px;
  color: var(--background-light);
  margin: 0;
}


.segmented label:hover {
  color: var(--text);
}


.segmented input[type='radio']:checked + label {
  background: var(--button-1);
  color: var(--text);
}


.segmented input[type='radio']:checked + label:hover {
  background: var(--button-1-highlight);
}


.card-option-ability {
  margin: 0 0 8px;
  padding-bottom: 7px;
  font-size: 12px;
  line-height: 1.45;
  color: var(--title);
  border-bottom: 1px solid var(--box-border);
}

.card-option-group--compact .toggle-row {
  gap: 12px;
  padding: 8px 12px;
}

.card-option-group--compact .toggle-name {
  font-size: 13px;
}

.card-option-group--compact .toggle-control {
  min-width: 110px;
}

.card-option-group--compact .segmented label {
  padding: 5px 8px;
}

@media (max-width: 700px) {
  .toggle-row {
    grid-template-columns: 1fr;
    gap: 10px;
  }
  .toggle-control {
    width: 100%;
  }
}
</style>
