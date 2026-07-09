<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'

const props = defineProps<{ entries: string[]; enabled: boolean; rolled?: string | null }>()
const { t } = useI18n()

// Ultimatum of Ultimatums' per-game random roll: shown apart from the
// permanent lists, gold for a boon, crimson for an ultimatum.
const rolledKind = computed(() => (props.rolled?.startsWith('BoonOf') ? 'boons' : 'ultimatums'))

const boons = computed(() => props.entries.filter((tag) => tag.startsWith('BoonOf')))
const ultimatums = computed(() => props.entries.filter((tag) => tag.startsWith('UltimatumOf')))

const groups = computed(() =>
  [
    { key: 'boons', title: t('ultimatumsAndBoons.boons'), entries: boons.value },
    { key: 'ultimatums', title: t('ultimatumsAndBoons.ultimatums'), entries: ultimatums.value },
  ].filter((g) => g.entries.length > 0)
)
</script>

<template>
  <div v-if="rolled" class="log-section rolled-section" :class="[rolledKind, { disabled: !enabled }]">
    <h3 class="section-title">
      {{ t('ultimatumsAndBoons.rolledThisGame') }}
      <span v-if="!enabled" class="disabled-badge">{{ t('ultimatumsAndBoons.currentlyDisabled') }}</span>
    </h3>
    <ul class="entry-list">
      <li class="entry rolled-entry">
        <span class="entry-icon" aria-hidden="true">{{ rolledKind === 'boons' ? '✦' : '✖' }}</span>
        <div class="entry-body">
          <span class="entry-name">{{ t(`ultimatumsAndBoons.entries.${rolled}.name`) }}</span>
          <span class="entry-text">{{ t(`ultimatumsAndBoons.entries.${rolled}.text`) }}</span>
        </div>
      </li>
    </ul>
  </div>
  <div
    v-for="group in groups"
    :key="group.key"
    class="log-section"
    :class="[group.key, { disabled: !enabled }]"
  >
    <h3 class="section-title">
      {{ group.title }}
      <span v-if="!enabled" class="disabled-badge">{{ t('ultimatumsAndBoons.currentlyDisabled') }}</span>
    </h3>
    <ul class="entry-list">
      <li v-for="tag in group.entries" :key="tag" class="entry">
        <span class="entry-icon" aria-hidden="true">{{ group.key === 'boons' ? '✦' : '✖' }}</span>
        <div class="entry-body">
          <span class="entry-name">{{ t(`ultimatumsAndBoons.entries.${tag}.name`) }}</span>
          <span class="entry-text">{{ t(`ultimatumsAndBoons.entries.${tag}.text`) }}</span>
        </div>
      </li>
    </ul>
  </div>
</template>

<style scoped>
.log-section {
  background: var(--box-background);
  border: 1px solid rgba(255,255,255,0.07);
  border-radius: 8px;
  padding: 14px 16px;

  &.boons { --accent: #b3922f; }
  &.ultimatums { --accent: #a03c3c; }

  &.disabled {
    opacity: 0.55;
    filter: grayscale(60%);
  }

  &.rolled-section {
    border-color: var(--accent);
    background: color-mix(in srgb, var(--accent) 12%, var(--box-background));
  }
}

.rolled-entry {
  border: 1px dashed var(--accent);
  border-left: 3px solid var(--accent);
}

.section-title {
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  font-weight: normal;
  color: rgba(255,255,255,0.75);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0 0 10px;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255,255,255,0.07);
  display: flex;
  align-items: baseline;
  gap: 10px;
}

.disabled-badge {
  font-family: sans-serif;
  font-size: 0.7em;
  text-transform: none;
  letter-spacing: normal;
  color: rgba(255,255,255,0.45);
  border: 1px solid rgba(255,255,255,0.2);
  border-radius: 999px;
  padding: 1px 8px;
}

.entry-list {
  display: flex;
  flex-direction: column;
  gap: 6px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.entry {
  display: flex;
  gap: 10px;
  padding: 8px 12px;
  border-radius: 5px;
  background: rgba(255,255,255,0.04);
  border-left: 3px solid var(--accent);
}

.entry-icon {
  color: var(--accent);
  flex-shrink: 0;
  line-height: 1.4;
}

.entry-body {
  display: flex;
  flex-direction: column;
  gap: 2px;
}

.entry-name {
  color: var(--title);
  font-weight: 600;
  font-size: 0.95rem;
  line-height: 1.4;
}

.entry-text {
  color: rgba(255,255,255,0.6);
  font-size: 0.85rem;
  line-height: 1.45;
}
</style>
