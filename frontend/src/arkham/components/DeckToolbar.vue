<script lang="ts" setup>
import { capitalize } from '@/arkham/helpers'

withDefaults(defineProps<{
  compact?: boolean
  searchPlaceholder?: string
}>(), { compact: false, searchPlaceholder: 'Search decks…' })

const allClasses = ["guardian", "seeker", "rogue", "mystic", "survivor", "neutral"]

const search = defineModel<string>('search', { default: '' })
const filterClasses = defineModel<string[]>('filterClasses', { default: () => [] })
const sortBy = defineModel<'name' | 'class'>('sortBy', { default: 'name' })

function toggleClass(c: string) {
  const idx = filterClasses.value.indexOf(c)
  filterClasses.value = idx === -1
    ? [...filterClasses.value, c]
    : filterClasses.value.filter(x => x !== c)
}
</script>

<template>
  <div class="deck-toolbar" :class="{ compact }">
    <div class="class-filters">
      <button
        v-for="iclass in allClasses"
        :key="iclass"
        class="class-pill"
        :class="{ [iclass]: filterClasses.includes(iclass), active: filterClasses.includes(iclass) }"
        :title="capitalize(iclass)"
        @click.prevent="toggleClass(iclass)"
      >
        <span :class="`${iclass}-icon`"></span>
        <span v-if="!compact" class="pill-label">{{ capitalize(iclass) }}</span>
      </button>
    </div>
    <div class="toolbar-right">
      <input
        v-model="search"
        class="search-input"
        :placeholder="searchPlaceholder"
        type="search"
      />
      <select v-model="sortBy" class="sort-select">
        <option value="name">Sort: Name</option>
        <option value="class">Sort: Class</option>
      </select>
    </div>
  </div>
</template>

<style scoped>
.deck-toolbar {
  display: flex;
  align-items: center;
  flex-wrap: wrap;
  gap: 8px;
}

.class-filters {
  display: flex;
  flex-wrap: wrap;
  gap: 4px;
  flex: 1;
}

.class-pill {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 5px;
  padding: 5px 10px;
  font-size: 0.78rem;
  font-weight: 600;
  color: #666;
  background: #1a1a1a;
  border: 1px solid #2a2a2a;
  border-radius: 4px;
  cursor: pointer;
  transition: background 0.12s, color 0.12s, border-color 0.12s;
  user-select: none;

  &:hover { color: #aaa; border-color: #444; }

  &.active.guardian { background: var(--guardian-extra-dark); border-color: var(--guardian-dark); color: #fff; }
  &.active.seeker   { background: var(--seeker-extra-dark);   border-color: var(--seeker-dark);   color: #fff; }
  &.active.rogue    { background: var(--rogue-extra-dark);    border-color: var(--rogue-dark);    color: #fff; }
  &.active.mystic   { background: var(--mystic-extra-dark);   border-color: var(--mystic-dark);   color: #fff; }
  &.active.survivor { background: var(--survivor-extra-dark); border-color: var(--survivor-dark); color: #fff; }
  &.active.neutral  { background: var(--neutral-extra-dark);  border-color: var(--neutral-dark);  color: #fff; }

  span[class$="-icon"] { font-size: 1em; }
}

/* Compact mode: icon-only square buttons */
.compact .class-pill {
  width: 36px;
  height: 36px;
  padding: 0;
  background: rgba(0,0,0,0.3);
  border-color: rgba(255,255,255,0.08);
  color: rgba(255,255,255,0.7);

  &:hover { border-color: rgba(255,255,255,0.2); color: white; }

  span[class$="-icon"] { font-size: 1.15em; }

  &.active.guardian { background: var(--guardian-extra-dark); border-color: var(--guardian-dark); color: #fff; }
  &.active.seeker   { background: var(--seeker-extra-dark);   border-color: var(--seeker-dark);   color: #fff; }
  &.active.rogue    { background: var(--rogue-extra-dark);    border-color: var(--rogue-dark);    color: #fff; }
  &.active.mystic   { background: var(--mystic-extra-dark);   border-color: var(--mystic-dark);   color: #fff; }
  &.active.survivor { background: var(--survivor-extra-dark); border-color: var(--survivor-dark); color: #fff; }
  &.active.neutral  { background: var(--neutral-extra-dark);  border-color: var(--neutral-dark);  color: #fff; }
}

.toolbar-right {
  display: flex;
  align-items: center;
  gap: 8px;
}

.search-input {
  padding: 6px 10px;
  font-size: 0.82rem;
  color: #ccc;
  background: #1a1a1a;
  border: 1px solid #2a2a2a;
  border-radius: 4px;
  outline: none;
  width: 180px;
  transition: border-color 0.12s;

  &::placeholder { color: #444; }
  &:focus { border-color: #444; }
}

.compact .search-input {
  background: rgba(0,0,0,0.3);
  border-color: rgba(255,255,255,0.08);
  color: #ccc;
  width: 140px;

  &::placeholder { color: #555; }
  &:focus { border-color: rgba(255,255,255,0.2); }
}

.sort-select {
  width: max-content;
  padding: 6px 32px 6px 10px;
  font-size: 0.82rem;
  color: #ccc;
  background-color: #1a1a1a;
  background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='10' height='6'%3E%3Cpath d='M0 0l5 6 5-6z' fill='%23888'/%3E%3C/svg%3E");
  background-repeat: no-repeat;
  background-position: right 10px center;
  border: 1px solid #2a2a2a;
  border-radius: 4px;
  outline: none;
  cursor: pointer;
  appearance: none;

  option { background: #1a1a1a; }
}

.compact .sort-select {
  background-color: rgba(0,0,0,0.3);
  border-color: rgba(255,255,255,0.08);

  option { background: #1a1a1a; }
}
</style>
