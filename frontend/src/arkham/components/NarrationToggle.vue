<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { useNarrationControls } from '@/arkham/composables/useNarration'

/* Replay / stop for the spoken story narration, pinned to the corner of the
 * story panel it sits in (the parent must be position: relative). */
const { t } = useI18n()
const { playing, blocked, hasTracks, replay, stop } = useNarrationControls()
const label = computed(() =>
  playing.value ? t('gameBar.settings.narration.stop') : t('gameBar.settings.narration.play'),
)
</script>

<template>
  <button
    v-if="hasTracks"
    type="button"
    class="narration-toggle"
    :class="{ blocked }"
    :title="label"
    :aria-label="label"
    @click="playing ? stop() : replay()"
  >{{ playing ? '■' : '🔊' }}</button>
</template>

<style scoped>
.narration-toggle {
  position: absolute;
  top: 6px;
  right: 8px;
  z-index: 2;
  width: 32px;
  height: 32px;
  padding: 0;
  border: 1px solid rgba(0, 0, 0, 0.25);
  border-radius: 50%;
  background: rgba(255, 255, 255, 0.75);
  color: #222;
  font-size: 15px;
  line-height: 1;
  cursor: pointer;
}
.narration-toggle:hover { background: #fff; }
/* The browser refused autoplay: draw the eye to the button that starts it. */
.narration-toggle.blocked { animation: narration-pulse 1.2s ease-in-out infinite; }

@keyframes narration-pulse {
  50% { box-shadow: 0 0 0 6px rgba(255, 200, 80, 0.55); }
}
</style>
