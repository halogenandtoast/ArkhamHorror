<script lang="ts" setup>
import { computed } from 'vue'

/* A preference that exists at two scopes at once: what you want everywhere, and
 * what you want in this one game.
 *
 * These used to be two sibling rows -- "Extra Animations" and "Extra Animations
 * (this scenario)" -- which read as two settings that happened to share a name,
 * and left the reader to work out that one silently overrode the other. It is
 * one setting asked twice, so it is one row: a single name and description, and
 * two labelled controls underneath that say which scope each answers.
 *
 * The override is a tri-state, not a boolean: "not in this scenario" and "not
 * ever" are different wishes, and `Default` is how you take the answer back.
 */
const props = defineProps<{
  name: string
  description?: string
  // Extra sentence appended to the description, e.g. when an OS preference is
  // overriding both scopes anyway.
  note?: string
  options: { value: string; label: string }[]
  global: string
  // null means "follow whatever the global says".
  override: string | null
  // Radio groups are matched by `name`, so two of these on one panel need
  // different keys or they fight over the same selection.
  settingKey: string
  highlighted?: boolean
}>()

const emit = defineEmits<{
  'update:global': [value: string]
  'update:override': [value: string | null]
}>()

const DEFAULT = '__default'
const overrideValue = computed(() => props.override ?? DEFAULT)
</script>

<template>
  <div class="toggle-row scoped-setting" :class="{ 'toggle-row--highlighted': highlighted }">
    <div class="toggle-text">
      <div class="toggle-name">{{ name }}</div>
      <div class="toggle-desc" v-if="description || note">
        {{ description }}
        <template v-if="note"> {{ note }}</template>
      </div>
    </div>

    <div class="scoped-setting__scopes">
      <div class="scoped-setting__scope">
        <span class="scoped-setting__scope-label">{{ $t('gameBar.settings.scopeEverywhere') }}</span>
        <div class="segmented scoped-setting__control">
          <template v-for="o in options" :key="o.value">
            <input
              type="radio"
              :id="`opt-${settingKey}-global-${o.value}`"
              :name="`opt-${settingKey}-global`"
              :checked="global === o.value"
              @change="emit('update:global', o.value)"
            />
            <label :for="`opt-${settingKey}-global-${o.value}`">{{ o.label }}</label>
          </template>
        </div>
      </div>

      <div class="scoped-setting__scope">
        <span class="scoped-setting__scope-label">{{ $t('gameBar.settings.scopeThisGame') }}</span>
        <div class="segmented scoped-setting__control">
          <input
            type="radio"
            :id="`opt-${settingKey}-game-default`"
            :name="`opt-${settingKey}-game`"
            :checked="overrideValue === DEFAULT"
            @change="emit('update:override', null)"
          />
          <label :for="`opt-${settingKey}-game-default`">{{ $t('gameBar.settings.default') }}</label>
          <template v-for="o in options" :key="o.value">
            <input
              type="radio"
              :id="`opt-${settingKey}-game-${o.value}`"
              :name="`opt-${settingKey}-game`"
              :checked="overrideValue === o.value"
              @change="emit('update:override', o.value)"
            />
            <label :for="`opt-${settingKey}-game-${o.value}`">{{ o.label }}</label>
          </template>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* Mirrors the .toggle-row vocabulary in arkham/components/Settings.vue -- scoped
   styles do not reach a child component, so the chrome has to be restated here.
   Keep the two in step. */
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

.toggle-row--highlighted {
  border-color: var(--select);
  box-shadow: 0 0 0 1px var(--select), 0 0 12px rgba(255, 0, 255, 0.6);
}

.toggle-text {
  min-width: 0;
}

.toggle-name {
  font-size: 14px;
  font-weight: 500;
  color: var(--text);
}

.toggle-desc {
  margin-top: 4px;
  font-size: 12px;
  line-height: 1.4;
  color: var(--background-light);
}

.scoped-setting__scopes {
  display: flex;
  flex-direction: column;
  gap: 6px;
  justify-self: end;
}

/* The scope label sits with its control rather than above the pair, so there is
   never a question about which row it governs. */
.scoped-setting__scope {
  display: grid;
  grid-template-columns: auto 1fr;
  align-items: center;
  gap: 10px;
}

.scoped-setting__scope-label {
  font-size: 10px;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--background-light);
  opacity: 0.75;
  text-align: right;
}

/* Both controls share a width so the segments line up between the two rows --
   the per-game one has one extra option (Default) and would otherwise sit
   ragged against the one above it. */
.scoped-setting__control {
  min-width: 300px;
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
  text-align: center;
  padding: 5px 8px;
  border-radius: 3px;
  font-size: 11px;
  color: var(--background-light);
  cursor: pointer;
  user-select: none;
  white-space: nowrap;
}

.segmented label:hover {
  color: var(--text);
}

.segmented input[type='radio']:checked + label {
  background: var(--button-1);
  color: var(--text);
}

@media (max-width: 700px) {
  .toggle-row {
    grid-template-columns: 1fr;
  }

  .scoped-setting__scopes {
    justify-self: stretch;
  }

  .scoped-setting__scope {
    grid-template-columns: 1fr;
    gap: 2px;
  }

  .scoped-setting__scope-label {
    text-align: left;
  }

  .scoped-setting__control {
    min-width: 0;
  }
}
</style>
