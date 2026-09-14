<script lang="ts" setup>
import { useI18n } from 'vue-i18n'
import { imgsrc } from '@/arkham/helpers'

/* One selectable side story. Shared by the Add Side Scenario list and by the
 * continue screen, which promotes any side story a campaign overlay is
 * currently offering so it does not have to be hunted for in the list. */
defineProps<{
  sideStory: {
    id: string
    name: string
    xp: number
    baseXp: number
    overlay?: string
    requiredInvestigator?: string
    deckRequirements?: string[]
  }
  disabled?: boolean
}>()

defineEmits<{ (e: 'select', sideStoryId: string): void }>()

const { t } = useI18n()
</script>

<template>
  <div class="side-story-option" :class="{ 'side-story-option--overlay': sideStory.overlay }">
    <div class="scenario-icon">
      <img :src="imgsrc(`sets/${sideStory.id}.png`)" />
    </div>
    <div class="scenario-info">
      <h2>{{ sideStory.name }}</h2>
      <p v-if="sideStory.overlay" class="campaign-overlay-label">
        {{ t('sideStory.variant') }}
      </p>
      <h3 v-if="sideStory.requiredInvestigator">
        {{ t('sideStory.xpAsymmetric', { signatureXp: sideStory.xp, name: sideStory.requiredInvestigator, otherXp: 1 }) }}
      </h3>
      <template v-else>
        <h3>
          <del v-if="sideStory.overlay && sideStory.xp !== sideStory.baseXp" class="original-xp">{{ sideStory.baseXp }} XP</del>
          ({{ sideStory.xp }} XP)
        </h3>
        <h3 v-for="requirement in sideStory.deckRequirements" :key="requirement">{{ requirement }}</h3>
      </template>
    </div>

    <button class="add" @click="$emit('select', sideStory.id)" :disabled="disabled">+</button>
  </div>
</template>

<style scoped lang="scss">
.side-story-option {
  border: 1px solid var(--line);
  border-radius: 8px;
  padding: 10px;
  background: rgba(255, 255, 255, 0.1);
  display: flex;
  gap: 10px;
  text-align: left;
  h3 {
    margin: 0;
    color: white;
  }
  img {
    max-height: 60px;
    filter: invert(100%) brightness(60%);
  }
  .scenario-icon {
    margin-right: 10px;
    width: 60px;
    justify-content: center;
    display: flex;
  }
}

.side-story-option--overlay {
  background: #392e48;
  border-color: #b98bd0;

  h2 { color: #f0e2f7; }
  .original-xp { color: #c9bbd1; font-size: 0.8em; }
}

.scenario-info {
  h2 {
    color: white;
    font-family: "Teutonic", sans-serif;
    font-size: 1.8em;
    margin: 0;
  }
}

.campaign-overlay-label {
  margin: 0.25rem 0;
  color: #e1c3f1;
  font-size: 0.85rem;
}

.add {
  border: 0;
  background: rgba(0, 0, 0, 0.3);
  color: white;
  border-radius: 8px;
  font-size: 1.5em;
  width: 40px;
  height: 40px;
  align-self: center;
  margin-left: auto;
  display: flex;
  align-items: center;
  justify-content: center;
  &:hover {
    background: rgba(0, 0, 0, 0.5);
    cursor: pointer;
  }
  &:disabled {
    cursor: default;
    opacity: 0.6;
  }
}
</style>
