<script setup lang="ts">
/* One titled panel in the home page's right-hand feed.
 *
 * The feed is a column of these. Announcements are the only one today, but the
 * column exists so that the next thing worth telling a player on their way into
 * a game -- an event sign-up, a changelog, a spotlighted scenario -- is a
 * component plus one line in Home.vue, and so the column reads as one piece of
 * furniture rather than a pile of unrelated boxes.
 */
defineProps<{ title: string; icon?: string; count?: number }>()
</script>

<template>
  <section class="feed-panel">
    <header>
      <font-awesome-icon v-if="icon" :icon="icon" />
      <h3>{{ title }}</h3>
      <!-- Zero is not worth a badge: a panel with nothing in it says so in its
           own body, where there is room for a sentence. -->
      <span v-if="count" class="count">{{ count }}</span>
    </header>
    <div class="body">
      <slot />
    </div>
  </section>
</template>

<style scoped>
.feed-panel {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  overflow: hidden;
}

header {
  align-items: center;
  background: var(--background-dark);
  border-bottom: 1px solid var(--box-border);
  color: var(--title);
  display: flex;
  gap: 8px;
  padding: 8px 10px;
}

header svg {
  opacity: 0.75;
}

h3 {
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  letter-spacing: 0.04em;
  margin: 0;
  text-transform: uppercase;
}

.count {
  background: var(--box-background);
  border-radius: 999px;
  font-size: 0.75em;
  font-variant-numeric: tabular-nums;
  font-weight: 700;
  line-height: 1;
  min-width: 1.6em;
  padding: 4px 6px;
  text-align: center;
}

.body {
  padding: 10px;
}
</style>
