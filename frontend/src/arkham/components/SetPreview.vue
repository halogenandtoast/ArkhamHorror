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
  }>(),
  { interactive: false },
)

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
    <template v-else>
      <CardSetStrip
        ref="strip"
        class="strip"
        :cards="cards"
        :interactive="interactive"
        @pick="emit('pick', $event)"
      />
      <div class="more">
        <button type="button" class="view-all" @click="emit('viewAll')">
          {{ more ? t(`${K}viewAll`, { count: total }) : t(`${K}openSet`) }}
          <font-awesome-icon icon="chevron-right" />
        </button>
      </div>
    </template>
  </div>
</template>

<style scoped lang="scss">
.set-preview {
  padding: 0.75rem 0.9rem 0;
}

.empty {
  color: color-mix(in srgb, var(--title) 55%, transparent);
  font-size: 0.85rem;
  font-style: italic;
  margin: 0;
  padding: 0.5rem 0 1rem;
}

.strip {
  margin-bottom: 0.1rem;
}

/* A div, not a <footer>: `base.css` styles bare `footer` into the site's own
   pinned credit line -- fixed to the bottom of the window, and hidden outright
   on a portrait phone, which is where this went missing. */
.more {
  display: flex;
  justify-content: flex-end;
  padding: 0.15rem 0 0.5rem;
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
