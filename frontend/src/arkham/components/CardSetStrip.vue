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
   cut on a card edge rather than through one. */
.card-strip {
  --strip-card: 110px;
  --strip-card-height: 190px;

  display: grid;
  gap: 0.75rem;
  grid-auto-rows: var(--strip-card-height);
  grid-template-columns: repeat(auto-fill, var(--strip-card));
  max-height: var(--strip-card-height);
  min-width: 0;
  overflow: hidden;

  &.wrap {
    max-height: none;
    overflow: visible;
  }
}

.strip-card {
  background: none;
  border: 1px solid transparent;
  border-radius: 6px;
  color: inherit;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  height: var(--strip-card-height);
  padding: 0.3rem;
  text-align: left;
  width: var(--strip-card);

  img {
    /* A fixed box so the row has one height whatever shape the card is --
       locations and acts are landscape. `drop-shadow` rather than `box-shadow`
       because the shadow has to follow the letterboxed picture, not the box. */
    border-radius: 3px;
    filter: drop-shadow(1px 1px 2px rgba(0, 0, 0, 0.8));
    height: 156px;
    object-fit: contain;
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
    background: rgba(255, 255, 255, 0.05);
    border-color: var(--spooky-green);
  }
}
</style>
