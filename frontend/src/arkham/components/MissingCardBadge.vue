<script lang="ts" setup>
/* Shown on a custom card whose definition is gone.
 *
 * The engine builds an inert card rather than failing, so the game keeps
 * playing; this says why that card does nothing, and links to the one place
 * that fixes it. The builder's ?card= deep link covers both cases on its own:
 * it opens the entry when the code is still in your library, and lands on an
 * empty builder when the card was deleted and has to be built again.
 *
 * The library is deliberately not loaded to tell those apart -- loading it
 * registers its defs into this game's registry, which would make the card look
 * whole here while the server still holds nothing for it. */
import { computed } from 'vue'
import { isMissingCustomCard, stripCardCodePrefix } from '@/arkham/customCards'

const props = defineProps<{ cardCode: string }>()

const missing = computed(() => isMissingCustomCard(props.cardCode))
const code = computed(() => stripCardCodePrefix(props.cardCode))
</script>

<template>
  <router-link
    v-if="missing"
    class="missing-card-badge"
    :to="{ name: 'CardBuilder', query: { card: code } }"
    v-tooltip="'This card has no definition in this game, so it does nothing. Open it in the card builder to restore it, or build it again if it was deleted.'"
    @click.stop
  >
    <font-awesome-icon :icon="['fas', 'triangle-exclamation']" />
    <span>Fix in builder</span>
  </router-link>
</template>

<style scoped lang="scss">
.missing-card-badge {
  align-items: center;
  background: rgba(127, 29, 29, 0.94);
  border: 1px solid #fca5a5;
  border-radius: 4px;
  bottom: 8%;
  color: #fee2e2;
  display: flex;
  font-size: 0.6em;
  gap: 0.4em;
  left: 50%;
  padding: 0.3em 0.5em;
  position: absolute;
  text-decoration: none;
  transform: translateX(-50%);
  white-space: nowrap;
  z-index: 5;

  &:hover {
    background: rgb(153, 27, 27);
  }
}
</style>
