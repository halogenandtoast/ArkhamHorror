<script lang="ts" setup>
/* Debug -/+ for an investigator's trauma and experience, under their row on the
 * campaign log's Investigators tab. Trauma follows the rules: suffering it can
 * kill or drive the investigator insane at the limit, mid-scenario as well.
 * XP is reported to the breakdown like any other gain, so the totals add up. */
import { useDebug } from '@/arkham/debug'
import type { Game } from '@/arkham/types/Game'
import type { Investigator } from '@/arkham/types/Investigator'
import {
  gainXpMessage,
  healTraumaMessage,
  spendXpMessage,
  sufferTraumaMessage,
  type TraumaKind,
} from '@/arkham/debugPlayer'

const props = defineProps<{
  game: Game
  investigator: Investigator
}>()

const debug = useDebug()

const send = (message: unknown) => debug.send(props.game.id, message)
const trauma = (kind: TraumaKind, by: 1 | -1) =>
  send(by > 0 ? sufferTraumaMessage(props.investigator.id, kind) : healTraumaMessage(props.investigator.id, kind))
const xp = (by: 1 | -1) =>
  send(by > 0 ? gainXpMessage(props.investigator.id) : spendXpMessage(props.investigator.id))
</script>

<template>
  <div class="campaign-investigator-debug">
    <div class="stepper" v-tooltip="$t('debug.campaign.xpHint')">
      <span class="stepper__label">XP</span>
      <button type="button" :disabled="investigator.xp <= 0" @click="xp(-1)">−</button>
      <span class="stepper__value">{{ investigator.xp }}</span>
      <button type="button" @click="xp(1)">+</button>
    </div>
    <div class="stepper" v-tooltip="$t('debug.campaign.traumaHint')">
      <span class="stepper__label">{{ $t('investigatorRow.physical') }}</span>
      <button type="button" :disabled="investigator.physicalTrauma <= 0" @click="trauma('physical', -1)">−</button>
      <span class="stepper__value">{{ investigator.physicalTrauma }}</span>
      <button type="button" @click="trauma('physical', 1)">+</button>
    </div>
    <div class="stepper" v-tooltip="$t('debug.campaign.traumaHint')">
      <span class="stepper__label">{{ $t('investigatorRow.mental') }}</span>
      <button type="button" :disabled="investigator.mentalTrauma <= 0" @click="trauma('mental', -1)">−</button>
      <span class="stepper__value">{{ investigator.mentalTrauma }}</span>
      <button type="button" @click="trauma('mental', 1)">+</button>
    </div>
  </div>
</template>

<style scoped>
.campaign-investigator-debug {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  padding: 6px 10px 10px;
  font-size: 0.8rem;
  color: var(--text);
}

.stepper {
  display: inline-flex;
  align-items: center;
  gap: 4px;
  padding: 2px 6px;
  border: 1px dashed rgba(255, 255, 255, 0.25);
  border-radius: 999px;
}

.stepper__label {
  color: rgba(255, 255, 255, 0.6);
}

.stepper__value {
  min-width: 1.4em;
  text-align: center;
  font-weight: 600;
}

.stepper button {
  width: 22px;
  height: 22px;
  padding: 0;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: var(--background-dark);
  color: var(--text);
  cursor: pointer;
}

.stepper button:hover:not(:disabled) {
  background: var(--button);
  border-color: var(--select);
}

.stepper button:disabled {
  opacity: 0.4;
  cursor: not-allowed;
}
</style>
