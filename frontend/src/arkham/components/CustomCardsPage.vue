<script lang="ts" setup>
/* The chrome every custom-cards page wears: a title, what the page is for, the
 * experimental note, and the one slot where a status or an error lands.
 *
 * Four pages were each carrying their own copy of this, and they had drifted --
 * different paddings, two different shades of error, an experimental banner
 * shouting in a different size on each. Shared so the section reads as one
 * place rather than four pages that happen to link to each other.
 */
import { useI18n } from 'vue-i18n'

const { t } = useI18n()
const K = 'customCardSets.'

withDefaults(
  defineProps<{
    title: string
    lede?: string
    /** Hidden on the one page that is a detail view rather than a section. */
    experimental?: boolean
    status?: string | null
    error?: string | null
    /** Drop the scrolling wrapper, for a page that already has one of its own.
     * Two nested `height: 100%; overflow-y: auto` boxes is a scroll container
     * inside a scroll container, which on iOS traps the page's own scroll. */
    inline?: boolean
  }>(),
  { experimental: true, inline: false },
)
</script>

<template>
  <div :class="inline ? 'contents' : 'page-container'">
    <section class="cc-page" :class="{ inline }">
      <header class="cc-head">
        <div class="cc-titles">
          <h1>{{ title }}</h1>
          <p v-if="lede" class="cc-lede">{{ lede }}</p>
        </div>
        <!-- Whatever this page does at the top level: make a set, import one. -->
        <div v-if="$slots.actions" class="cc-actions">
          <slot name="actions" />
        </div>
      </header>

      <!-- A line, not a billboard. It has to be read once and then live quietly
           at the top of a page people visit every day. -->
      <p v-if="experimental" class="cc-experimental">
        <font-awesome-icon icon="flask" />
        <span>{{ t(`${K}experimental`) }}</span>
      </p>

      <!-- `role="status"` so the result of an import or a publish is announced;
           these are the slowest actions here and the only sign they finished. -->
      <p v-if="status" class="cc-status" role="status">
        <font-awesome-icon icon="circle-check" />
        <span>{{ status }}</span>
      </p>
      <p v-if="error" class="cc-error" role="alert">
        <font-awesome-icon icon="circle-exclamation" />
        <span>{{ error }}</span>
      </p>

      <slot />
    </section>

    <slot name="outside" />
  </div>
</template>

<style scoped lang="scss">
.page-container {
  height: 100%;
  overflow-x: hidden;
  overflow-y: auto;
  width: 100%;
}

/* `inline`: the host page is already the scroller, so this is nothing at all. */
.contents {
  display: contents;
}

.cc-page {
  color: var(--title);
  margin: 0 auto;
  max-width: 1180px;
  padding: 1.75rem 1.25rem 4rem;

  @media (max-width: 700px) {
    padding: 1rem 0.75rem 3rem;
  }

  &.inline {
    padding-bottom: 0;
  }
}

/* Title and page-level actions on one line, stacking on a narrow screen rather
   than squeezing a text field down to nothing beside the heading. */
.cc-head {
  align-items: flex-end;
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem 1.5rem;
  justify-content: space-between;
  margin-bottom: 1rem;
}

.cc-titles {
  min-width: 0;

  h1 {
    font-family: teutonic, sans-serif;
    font-size: 1.9em;
    line-height: 1.1;
    margin: 0;
  }
}

.cc-lede {
  margin: 0.35rem 0 0;
  max-width: 64ch;
  opacity: 0.72;
}

.cc-actions {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;

  @media (max-width: 700px) {
    flex: 1 1 100%;
  }
}

.cc-experimental,
.cc-status,
.cc-error {
  align-items: baseline;
  border-radius: 5px;
  display: flex;
  font-size: 0.8rem;
  gap: 0.55rem;
  margin: 0 0 0.85rem;
  padding: 0.45rem 0.7rem;

  svg {
    flex: none;
  }
}

/* Muted rather than alarming: it is true all the time, so an alert-coloured box
   would train people to ignore the colour everywhere else on the page. */
.cc-experimental {
  background: color-mix(in srgb, var(--important) 10%, transparent);
  border: 1px solid color-mix(in srgb, var(--important) 35%, transparent);
  color: color-mix(in srgb, var(--important) 85%, white);
}

.cc-status {
  background: color-mix(in srgb, var(--spooky-green) 16%, transparent);
  border: 1px solid color-mix(in srgb, var(--spooky-green) 55%, transparent);
}

.cc-error {
  background: color-mix(in srgb, var(--delete) 16%, transparent);
  border: 1px solid color-mix(in srgb, var(--delete) 60%, transparent);
  color: color-mix(in srgb, var(--delete) 45%, white);
}
</style>
