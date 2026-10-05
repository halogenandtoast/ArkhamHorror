<script lang="ts" setup>
/* One fact about a set, as a pill: how many cards, which version, how many
 * thumbs up, whether it is yours.
 *
 * These were run-on sentences joined by middots -- "by halogen64 · v1 · 34
 * cards" -- which reads as one undifferentiated string and gives the eye nothing
 * to land on. Each fact is its own shape now, and `tone` is the only thing that
 * varies, so a page cannot invent a sixth colour for a seventh fact.
 */
withDefaults(
  defineProps<{
    /** `plain` is a fact, `good` is in the marketplace, `warn` is waiting on
     * someone, `bad` was turned down, `mine` is yours, `gold` is official. */
    tone?: 'plain' | 'good' | 'warn' | 'bad' | 'mine' | 'gold'
    icon?: string
  }>(),
  { tone: 'plain' },
)
</script>

<template>
  <span class="chip" :class="tone">
    <font-awesome-icon v-if="icon" :icon="icon" />
    <slot />
  </span>
</template>

<style scoped lang="scss">
.chip {
  align-items: center;
  border: 1px solid var(--box-border);
  border-radius: 999px;
  color: color-mix(in srgb, var(--title) 80%, transparent);
  display: inline-flex;
  flex: none;
  font-size: 0.72rem;
  gap: 0.3rem;
  line-height: 1.5;
  padding: 0.1rem 0.5rem;
  white-space: nowrap;

  svg {
    font-size: 0.85em;
    opacity: 0.9;
  }
}

.good {
  border-color: color-mix(in srgb, var(--spooky-green) 70%, transparent);
  color: var(--spooky-green);
}

.warn {
  border-color: color-mix(in srgb, var(--important) 55%, transparent);
  color: color-mix(in srgb, var(--important) 85%, white);
}

.bad {
  border-color: color-mix(in srgb, var(--survivor) 55%, transparent);
  color: color-mix(in srgb, var(--survivor) 70%, white);
}

.mine {
  border-color: color-mix(in srgb, var(--guardian) 50%, transparent);
  color: color-mix(in srgb, var(--guardian) 85%, white);
}

/* The project's own set rather than one it let through, so it is the one chip
   that is allowed to be filled in. */
.gold {
  background: color-mix(in srgb, var(--important) 18%, transparent);
  border-color: color-mix(in srgb, var(--important) 60%, transparent);
  color: color-mix(in srgb, var(--important) 90%, white);
}
</style>
