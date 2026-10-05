<script lang="ts" setup>
/* The cards of a set, as pictures.
 *
 * One row clipped to what fits, which is what a set's own line on a list wants,
 * or every card wrapped, which is what looking at the whole set wants. Shared so
 * a set looks the same in your own library and in the marketplace.
 */
import { renderCardPlaceholder, type CustomCard } from '@/arkham/customCards'

const props = defineProps<{
  cards: CustomCard[]
  /** Wrap to as many rows as it takes, rather than clipping to one. */
  wrap?: boolean
  /** Whether a card can be clicked. A picture that does nothing is not a button. */
  interactive?: boolean
}>()

const emit = defineEmits<{ pick: [card: CustomCard] }>()

const cardArt = (card: CustomCard) => card.art ?? renderCardPlaceholder(card.def)
</script>

<template>
  <div class="card-strip" :class="{ wrap: props.wrap }">
    <component
      v-for="card in props.cards"
      :is="props.interactive ? 'button' : 'span'"
      :key="card.def.cardCode"
      :type="props.interactive ? 'button' : undefined"
      class="strip-card"
      :class="{ pickable: props.interactive }"
      v-tooltip="card.def.name.title"
      @click="props.interactive ? emit('pick', card) : undefined"
    >
      <!-- `data-image` is what CardOverlay hovers on. The art is given outright
           rather than as a card code, because a custom card's code resolves to
           nothing in the printed-card image host. -->
      <img :src="cardArt(card)" :data-image="cardArt(card)" alt="" />
      <span class="name">{{ card.def.name.title }}</span>
    </component>
  </div>
</template>

<style scoped lang="scss">
/* `auto-fill` works out how many whole cards fit the column, so a clipped row is
   cut on a card edge rather than through one.

   Clipped and wrapped want different columns. A clipped row is a sample, so its
   cards keep one size however wide the page is and the row simply shows more of
   them. A wrapped grid is the whole set, so its cards stretch to share the width
   evenly -- fixed columns there left a ragged gutter down the right-hand side,
   which on a phone was most of the screen. */
.card-strip {
  --strip-card: 112px;
  --strip-image: 158px;
  --strip-card-height: 190px;

  display: grid;
  gap: 0.6rem 0.5rem;
  grid-auto-rows: var(--strip-card-height);
  grid-template-columns: repeat(auto-fill, var(--strip-card));
  max-height: var(--strip-card-height);
  min-width: 0;
  overflow: hidden;

  &.wrap {
    grid-template-columns: repeat(auto-fill, minmax(var(--strip-card), 1fr));
    max-height: none;
    overflow: visible;
  }

  @media (max-width: 560px) {
    --strip-card: 96px;
    --strip-image: 136px;
    --strip-card-height: 166px;
  }
}

.strip-card {
  background: none;
  border: 1px solid transparent;
  border-radius: 6px;
  color: inherit;
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  height: var(--strip-card-height);
  padding: 0.3rem;
  text-align: center;
  transition: background 120ms ease, border-color 120ms ease, transform 120ms ease;
  width: 100%;

  img {
    /* A fixed box so the row has one height whatever shape the card is --
       locations and acts are landscape. `drop-shadow` rather than `box-shadow`
       because the shadow has to follow the letterboxed picture, not the box.

       Bottom-aligned: a landscape card centred in a portrait box floats in the
       middle of its cell while its neighbours fill theirs, and a row of those
       reads as misaligned rather than as two shapes of card. Sitting them all
       on one baseline is what makes the row look level. */
    border-radius: 3px;
    filter: drop-shadow(1px 1px 2px rgba(0, 0, 0, 0.8));
    height: var(--strip-image);
    object-fit: contain;
    object-position: center bottom;
    width: 100%;
  }

  .name {
    font-size: 0.72rem;
    line-height: 1.2;
    opacity: 0.8;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
}

.pickable {
  cursor: pointer;

  &:hover {
    background: rgba(255, 255, 255, 0.06);
    border-color: var(--spooky-green);
    transform: translateY(-1px);

    .name {
      opacity: 1;
    }
  }

  &:focus-visible {
    border-color: var(--spooky-green);
    outline: 2px solid var(--spooky-green);
    outline-offset: 1px;
  }
}

@media (prefers-reduced-motion: reduce) {
  .strip-card {
    transition: none;
  }

  .pickable:hover {
    transform: none;
  }
}
</style>
