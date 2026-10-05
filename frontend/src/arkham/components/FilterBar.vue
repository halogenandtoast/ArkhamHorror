<script lang="ts" setup>
/* The search box every one of these pages has, and whatever toggles sit beside
 * it.
 *
 * It used to be right-aligned with the toggles crowding it, which on a phone
 * put the search field and the thing that sorts its results on two different
 * lines at opposite edges. The field leads now and takes the slack; the
 * toggles follow and keep their natural width.
 */
const model = defineModel<string>({ required: true })

defineProps<{ placeholder: string; clearLabel: string }>()
</script>

<template>
  <div class="filter-bar">
    <div class="field">
      <font-awesome-icon icon="search" />
      <input
        v-model="model"
        type="search"
        :placeholder="placeholder"
        :aria-label="placeholder"
        @keydown.stop
      />
      <button
        v-if="model"
        type="button"
        class="clear"
        v-tooltip="clearLabel"
        :aria-label="clearLabel"
        @click="model = ''"
      >
        <font-awesome-icon icon="times" />
      </button>
    </div>
    <div v-if="$slots.default" class="toggles">
      <slot />
    </div>
  </div>
</template>

<style scoped lang="scss">
.filter-bar {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.6rem;
  margin-bottom: 0.9rem;
}

.field {
  align-items: center;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  display: flex;
  flex: 1 1 16rem;
  gap: 0.5rem;
  min-width: 0;
  padding: 0 0.6rem;

  &:focus-within {
    border-color: var(--spooky-green);
  }

  > svg {
    flex: none;
    font-size: 0.8rem;
    opacity: 0.5;
  }

  input {
    background: none;
    border: none;
    color: var(--title);
    flex: 1 1 auto;
    font-size: 0.9rem;
    min-width: 0;
    /* 40px so the row is a comfortable target on a phone, and so it lines up
       with the toggles beside it rather than sitting a few pixels shorter. */
    padding: 0.55rem 0;

    &:focus {
      outline: none;
    }

    &::-webkit-search-cancel-button {
      display: none;
    }
  }
}

/* Both resets matter: the global button rule gives every button 11px of side
   padding, which on a 28px border-box square leaves a 6px content box that the
   icon is squeezed into -- and the icon is sized in em, so without a font-size
   of its own it inherits that rule's 12px as well. */
.clear {
  background: none;
  border: none;
  color: var(--title);
  cursor: pointer;
  display: grid;
  flex: none;
  font-size: 0.9rem;
  height: 28px;
  opacity: 0.7;
  padding: 0;
  place-items: center;
  width: 28px;

  &:hover {
    opacity: 1;
  }
}

.toggles {
  display: flex;
  flex: 0 1 auto;
  gap: 0.5rem;

  /* Full width on a phone, where a 2-up toggle squeezed next to a search field
     is two unreadable labels. */
  @media (max-width: 560px) {
    flex: 1 1 100%;

    > * {
      flex: 1 1 0;
    }
  }
}
</style>
