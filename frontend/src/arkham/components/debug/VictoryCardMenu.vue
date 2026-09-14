<script lang="ts" setup>
import { Dropdown } from 'floating-vue'
import { ref } from 'vue'
import { type Card, type CardContents, toCardContents } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'
import * as DebugMove from '@/arkham/debugCardMove'
import { useI18n } from 'vue-i18n'

const { t } = useI18n()

const props = defineProps<{
  game: Game
  card: Card | CardContents
}>()

const shown = ref(false)

/* Nothing in the rules ever takes a card back out of the victory display, so a
 * card the engine puts there wrongly (#5662) is otherwise stuck for the rest of
 * the scenario. These are the three zones it could plausibly have belonged in. */
const destinations: { key: string; destination: DebugMove.DebugCardDestination }[] = [
  { key: 'removeFromGame', destination: DebugMove.removedFromGame },
  { key: 'setAside', destination: DebugMove.setAside },
  { key: 'discard', destination: DebugMove.discarded },
]

function move(destination: DebugMove.DebugCardDestination) {
  shown.value = false
  DebugMove.debugMoveCard(props.game.id, toCardContents(props.card).id, destination)
}
</script>

<template>
  <!-- Opened by hand rather than by floating-vue's own trigger: the click has to
       stop here so it never reaches the card underneath, and stopping it is what
       would keep the trigger from ever seeing it. -->
  <Dropdown
    v-model:shown="shown"
    :triggers="[]"
    :auto-hide="true"
    placement="bottom"
    :distance="6"
    theme="cards-under-popover"
  >
    <button
      type="button"
      class="debug-card-menu"
      :aria-label="t('debug.victoryDisplay.title')"
      v-tooltip="t('debug.victoryDisplay.title')"
      @click.stop.prevent="shown = !shown"
    >
      <font-awesome-icon icon="bug" />
    </button>

    <template #popper>
      <div class="debug-card-menu__items">
        <button
          v-for="{ key, destination } in destinations"
          :key="key"
          type="button"
          @click.stop.prevent="move(destination)"
        >{{ t(`debug.victoryDisplay.${key}`) }}</button>
      </div>
    </template>
  </Dropdown>
</template>

<style scoped>
/* Matches the `.debug-customize` button Card.vue already puts on a card, but on
   the opposite corner so the two never overlap. Both carry a bug rather than a
   wrench: a wrench reads as settings, and the wrench on an asset means it is
   jammed. */
.debug-card-menu {
  position: absolute;
  top: 4px;
  right: 4px;
  z-index: var(--z-index-20);
  display: flex;
  align-items: center;
  justify-content: center;
  width: 22px;
  height: 22px;
  padding: 0;
  border: 1px solid #111;
  border-radius: 50%;
  background: rgba(255, 255, 255, 0.9);
  color: #111;
  font-size: 11px;
  cursor: pointer;
}

.debug-card-menu:hover {
  background: #fff;
}

.debug-card-menu__items {
  display: flex;
  flex-direction: column;
  gap: 4px;
  padding: 6px;
  min-width: 160px;
}

.debug-card-menu__items button {
  width: 100%;
  text-align: left;
  white-space: nowrap;
}
</style>
