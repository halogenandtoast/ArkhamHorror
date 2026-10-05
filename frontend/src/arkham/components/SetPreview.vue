<script lang="ts" setup>
/* The bottom half of a set's panel: a row of its cards, and the way through to
 * the rest of them.
 *
 * The "View all 34" button used to float at the right-hand end of the strip,
 * inside the same box, where it read as a card that had failed to load. It is a
 * footer now -- the strip is the picture, the footer is the way out of it.
 */
import { computed, nextTick, onUnmounted, ref, watch, type ComponentPublicInstance } from 'vue'
import { useI18n } from 'vue-i18n'
import type { CustomCard } from '@/arkham/customCards'
import CardSetStrip from '@/arkham/components/CardSetStrip.vue'

const { t } = useI18n()
const K = 'customCardSets.'

const props = withDefaults(
  defineProps<{
    cards: CustomCard[]
    /** How many the set actually holds; the preview is only the first few. */
    total: number
    /** A card can be clicked to edit it. Only true in your own library. */
    interactive?: boolean
    /** Said instead of the strip when there is nothing to show. */
    emptyLabel?: string
    /** The caption on the box. Overridable: a set's own library row is the
     *  same strip but is not previewing anything. */
    label?: string
  }>(),
  { interactive: false },
)

const caption = computed(() => props.label ?? t(`${K}previewLabel`))

const emit = defineEmits<{ pick: [card: CustomCard]; viewAll: [] }>()

/* Whether the row is hiding anything, which is two different questions
 * depending on who is asking. A marketplace listing is handed the first twelve
 * cards of thirty-four, so it knows by counting. Your own library is handed the
 * whole set and lets CSS clip the row, so only the browser knows how many fit:
 * measured here rather than guessed at from widths and gaps.
 *
 * Measuring lives with the strip rather than with the page, because the strip
 * is the thing being clipped -- the pages that used to do it each kept their own
 * observer, and the one that forgot showed "view all" on a set with four cards
 * in it. */
const strip = ref<ComponentPublicInstance | null>(null)
const clipped = ref(false)

// The strip's own root is the box CSS clips, so that is what gets measured.
const stripEl = () => (strip.value?.$el as HTMLElement | undefined) ?? null

function measure() {
  const el = stripEl()
  clipped.value = !!el && el.scrollHeight > el.clientHeight + 1
}

let observer: ResizeObserver | null = null

watch(strip, () => {
  observer?.disconnect()
  const el = stripEl()
  if (!el) return
  observer ??= new ResizeObserver(measure)
  observer.observe(el)
})

// A card added or removed can change what fits without changing the height the
// observer watches, so the list is watched too.
watch(() => props.cards, () => nextTick(measure), { deep: false })

onUnmounted(() => observer?.disconnect())

const more = computed(() => props.total > props.cards.length || clipped.value)
</script>

<template>
  <div class="set-preview">
    <p v-if="!cards.length" class="empty">{{ emptyLabel }}</p>
    <!-- A section rather than a bare div: the caption names the cards it sits
         with, so the box should be labelled to a screen reader too. -->
    <section v-else class="preview-box" :aria-label="caption">
      <CardSetStrip
        ref="strip"
        class="strip"
        :cards="cards"
        :interactive="interactive"
        @pick="emit('pick', $event)"
      />
      <div class="more">
        <p class="caption">{{ caption }}</p>
        <button type="button" class="view-all" @click="emit('viewAll')">
          {{ more ? t(`${K}viewAll`, { count: total }) : t(`${K}openSet`) }}
          <font-awesome-icon icon="chevron-right" />
        </button>
      </div>
    </section>
  </div>
</template>

<style scoped lang="scss">
.set-preview {
  padding: 0;
}

/* A full-bleed band across the foot of the panel rather than an inset card:
   the panel clips to its own radius, so a rule along the top and a shade more
   black is all it takes to read as its own container. */
.preview-box {
  background: color-mix(in srgb, black 16%, transparent);
  border-top: 1px solid var(--box-border);
  padding: 0.45rem 0.9rem 0.1rem;
}

/* Small caps rather than a heading size: it names the box, it is not competing
   with the set's own title above it. It rides on the footer rather than taking
   a line above the cards -- a line of its own cost every set in the list the
   same height again, for one word. */
.caption {
  color: color-mix(in srgb, var(--title) 55%, transparent);
  font-size: 0.66rem;
  font-weight: 600;
  letter-spacing: 0.08em;
  margin: 0;
  text-transform: uppercase;
}

/* Carries the side padding itself, now that the band around it is full-bleed. */
.empty {
  color: color-mix(in srgb, var(--title) 55%, transparent);
  font-size: 0.85rem;
  font-style: italic;
  margin: 0;
  padding: 0.5rem 0.9rem 1rem;
}

/* Pulled out by the card's own padding, so the first card's art starts on the
   same line as the caption over it and the set's name above that -- otherwise
   the whole row reads as nudged to the right. */
.strip {
  margin-bottom: 0.1rem;
  margin-inline: calc(-1 * var(--strip-pad));
}

/* A div, not a <footer>: `base.css` styles bare `footer` into the site's own
   pinned credit line -- fixed to the bottom of the window, and hidden outright
   on a portrait phone, which is where this went missing. */
.more {
  align-items: center;
  display: flex;
  gap: 0.75rem;
  justify-content: space-between;
  padding: 0 0 0.2rem;
}

/* A link, not a button: it goes somewhere rather than doing something, and a
   third filled button on the panel would compete with Import and Publish. */
.view-all {
  align-items: center;
  background: none;
  border: none;
  border-radius: 4px;
  color: color-mix(in srgb, var(--title) 75%, transparent);
  cursor: pointer;
  display: inline-flex;
  font-size: 0.78rem;
  gap: 0.35rem;
  min-height: 32px;
  padding: 0 0.4rem;

  svg {
    font-size: 0.7em;
    transition: transform 150ms ease;
  }

  &:hover {
    color: var(--title);

    svg {
      transform: translateX(2px);
    }
  }

  &:focus-visible {
    outline: 2px solid var(--spooky-green);
    outline-offset: 1px;
  }
}

@media (prefers-reduced-motion: reduce) {
  .view-all svg {
    transition: none;
  }
}
</style>
