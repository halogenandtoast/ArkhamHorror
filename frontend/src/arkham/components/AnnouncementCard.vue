<script setup lang="ts">
/* One announcement, drawn as a slip of paper.
 *
 * Its own component because the admin panel previews an announcement before
 * publishing it, and a preview is only worth having if it is the same card the
 * players get -- not a copy of its styles that drifts the first time one of
 * them is touched.
 *
 * The card keeps the parchment colouring the home-page banner had: it is the
 * same announcement, moved, and the cream is what makes it read as one on a
 * dark page. A warm gradient with a lit top edge and a shadow under it, and
 * the date set in the typewriter face the game uses for in-world documents,
 * ruled off from the body like a letterhead.
 *
 * A categorised announcement is stamped: a coloured tab down its edge and a
 * stamp beside the date, so what kind of news it is reads before the sentence
 * does. An announcement with no category -- which is every one written before
 * categories existed -- is drawn plain, and so is one filed under a category
 * this build does not know, rather than being dropped or mislabelled.
 */
import { computed } from 'vue'
import type { AppNotification, NotificationCategory } from '@/arkham/api'

const props = defineProps<{ notification: AppNotification }>()

const icons: Record<NotificationCategory, string> = {
  important: 'triangle-exclamation',
  feature: 'wrench',
  content: 'book',
}

const category = computed<NotificationCategory | null>(() => {
  const value = props.notification.category
  return value && value in icons ? value : null
})

/* Day and month, and the year too once it is not this one -- an old
 * announcement left up should not read as having gone up last week. */
const when = computed(() => {
  const date = new Date(props.notification.createdAt)
  if (isNaN(date.getTime())) return null
  const sameYear = date.getFullYear() === new Date().getFullYear()
  return date.toLocaleDateString(undefined, {
    month: 'short',
    day: 'numeric',
    ...(sameYear ? {} : { year: 'numeric' }),
  })
})
</script>

<template>
  <article class="announcement" :class="category ? `is-${category}` : null">
    <div class="meta">
      <span v-if="category" class="stamp">
        <font-awesome-icon :icon="icons[category]" />
        {{ $t(`home.categories.${category}`) }}
      </span>
      <time v-if="when" :datetime="props.notification.createdAt">{{ when }}</time>
    </div>
    <!-- The body is admin-authored HTML, as it was in the banner. -->
    <div class="body" v-html="props.notification.body"></div>
  </article>
</template>

<style scoped>
.announcement {
  /* Ink, not the paper's own brown: the old card used one colour for the date
     and the text, which left the text the fainter of the two. Dark and set a
     step heavier than body copy, because this is print on paper sitting in the
     corner of a dark page, and it has to hold its own there. */
  --ink: #372c0e;
  --ink-soft: #4f411b;
  --rule: rgba(110, 93, 48, 0.32);
  background:
    radial-gradient(120% 85% at 100% 0%, rgba(166, 134, 70, 0.13), transparent 62%),
    linear-gradient(176deg, #FFFCF2 0%, #FDF4DE 55%, #F8EBCF 100%);
  border: 1px solid var(--rule);
  border-radius: 6px;
  /* A lit top edge and a shadow beneath: a slip of paper laid on the panel
     rather than a swatch of cream cut into it. */
  box-shadow:
    inset 0 1px 0 rgba(255, 255, 255, 0.85),
    0 2px 6px rgba(0, 0, 0, 0.3);
  color: var(--ink);
  display: block;
  font-size: 0.95em;
  /* The body face is variable, so 500 is a real weight rather than a synthetic
     one -- a touch more ink per letter without reading as bold. */
  font-weight: 500;
  line-height: 1.5;
  padding: 9px 12px 11px;
}

/* The filing tab. Dark enough to read as ink on paper rather than as a
   highlighter over it, and the only colour on the card besides the type. */
.announcement.is-important { --stamp: #96271c; }
.announcement.is-feature { --stamp: #3c6b33; }
.announcement.is-content { --stamp: #5b3a86; }

.announcement[class*='is-'] {
  border-left: 4px solid var(--stamp);
  padding-left: 10px;
}

.meta {
  align-items: center;
  border-bottom: 1px solid var(--rule);
  color: var(--ink-soft);
  display: flex;
  font-family: "Typewriter", ui-monospace, monospace;
  font-size: 0.76em;
  font-variant-numeric: tabular-nums;
  font-weight: 700;
  gap: 8px;
  letter-spacing: 0.12em;
  margin-bottom: 7px;
  padding-bottom: 5px;
  text-transform: uppercase;
}

.stamp {
  align-items: center;
  background: color-mix(in srgb, var(--stamp) 10%, transparent);
  border: 1px solid color-mix(in srgb, var(--stamp) 45%, transparent);
  border-radius: 3px;
  color: var(--stamp);
  display: inline-flex;
  flex: none;
  gap: 5px;
  line-height: 1;
  padding: 3px 6px;
}

.stamp svg {
  font-size: 0.9em;
}

/* The stamp takes the left, where the eye starts, and the date gives way to
   it -- but only then: with no stamp beside it the date keeps the left, which
   is where it has always been. */
.stamp + time {
  margin-left: auto;
}

.body {
  overflow-wrap: anywhere;
}

.body :deep(p) {
  margin: 0;
  padding: 0;
}

.body :deep(p + p) {
  margin-top: 7px;
}

/* An author's emphasis should read as emphasis, not just as a heavier weight
   of the same ink. */
.body :deep(b),
.body :deep(strong) {
  color: #231a04;
  font-weight: 800;
}

.body :deep(a) {
  color: #5a3102;
  font-weight: 700;
  text-decoration: underline;
  text-underline-offset: 2px;
}

.body :deep(a:hover) {
  color: #231a04;
}

.body :deep(ul),
.body :deep(ol) {
  margin: 6px 0 0;
  padding-left: 18px;
}
</style>
