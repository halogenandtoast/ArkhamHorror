<script lang="ts" setup>
/* The mark on a card an effect forbids using, and what is forbidding it.
 *
 * Hovering it shows the offending card, through the same `data-image-id` the
 * blanked badge on an investigator uses -- see CardOverlay.
 *
 * `suppressed` is for a caller that knows the game is offering this card right
 * now -- committing it to a skill test under Antarctic Wind's "cannot play
 * cards", say. The ban still holds for playing, but saying so over the choice in
 * front of the player reads as a ban on that choice (#5959). The caller decides,
 * because it is the one that knows what the card is being offered for; it owns
 * the greying too, so the two cannot disagree.
 */
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { useDbCardStore } from '@/stores/dbCards'
import {
  cardProhibition,
  isProhibited,
  prohibitionImageId,
  prohibitionTitle,
} from '@/arkham/cardProhibitions'
import type { Game } from '@/arkham/types/Game'

const props = defineProps<{
  game: Game
  ownerId: string
  cardId: string
  suppressed?: boolean
}>()

const dbCards = useDbCardStore()
const { t } = useI18n()

const prohibition = computed(() => cardProhibition(props.game, props.ownerId, props.cardId))
const show = computed(() => !props.suppressed && isProhibited(prohibition.value))
const imageId = computed(() => prohibitionImageId(prohibition.value))
const title = computed(() => prohibitionTitle(prohibition.value, dbCards, t))
</script>

<template>
  <span
    v-if="show"
    class="prohibited-badge"
    :data-image-id="imageId"
    :title="title"
    :aria-label="title"
  >
    <font-awesome-icon icon="ban" />
  </span>
</template>

<style scoped>
.prohibited-badge {
  position: absolute;
  top: 4px;
  right: 4px;
  width: 22px;
  height: 22px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: #e05252;
  font-size: 16px;
  filter: drop-shadow(0 1px 4px rgba(0, 0, 0, 0.8));
  cursor: default;
  z-index: var(--z-index-20);
  pointer-events: auto;
}
</style>
