<script setup lang="ts">
import { computed } from 'vue';
import { useI18n } from 'vue-i18n';
import { Modifier } from '@/arkham/types/Modifier';
import { Game } from '@/arkham/types/Game';
import { imgsrc } from '@/arkham/helpers';
import { cardArt, sourceCardCode } from '@/arkham/cardImages';

const { t } = useI18n()
const props = defineProps<{ modifier: Modifier, game: Game }>()

const modifierSource = computed(() => {
  if(props.modifier.card) {
    return cardArt(props.modifier.card.contents.cardCode)
  }

  return sourceCardCode(props.modifier.source, props.game)
})

const normalizeSkill = (skill: string) => {
  switch (skill) {
    case 'SkillWillpower': return 'willpower'
    case 'SkillIntellect': return 'intellect'
    case 'SkillCombat': return 'combat'
    case 'SkillAgility': return 'agility'
    default: return skill
  }
}
</script>

<template>
  <div class="modifier" :data-image-id="modifierSource">
    <template v-if="modifier.type.tag === 'CannotCommitCards'">
      <span class="text">{{ $t('modifier.cannotCommitCards') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'Difficulty'">
      <span><template v-if="modifier.type.contents >= 0">+</template>{{modifier.type.contents}}</span>
      {{ $t('modifier.difficulty') }}
    </template>
    <template v-else-if="modifier.type.tag === 'CancelEffects'">
      <span class="text">{{ $t('modifier.cancelEffects') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'CannotPerformSkillTest'">
      <span class="text">{{ $t('modifier.cannotPerformSkillTest') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'DiscoveredClues'">
      <span>+{{modifier.type.contents}}</span>
      <img :src="imgsrc(`clue.png`)" />
    </template>
    <template v-else-if="modifier.type.tag === 'SkillTestResultValueModifier'">
      <span class="text">{{ $t('modifier.result') }}</span> <span>{{modifier.type.contents > 0 ? '+' : ''}}{{modifier.type.contents}}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'DamageDealt'">
      <span>+{{modifier.type.contents}}</span>
      <img :src="imgsrc(`damage.png`)" />
    </template>
    <template v-else-if="modifier.type.tag === 'AddSkillValue'">
      <span>+</span>
      <i
         :class="`${normalizeSkill(modifier.type.contents)}-icon`"
         :style="{ color: `var(--${normalizeSkill(modifier.type.contents)})` }"
      ></i>
    </template>
    <template v-else-if="modifier.type.tag === 'SkillModifier'">
      <span>+ {{modifier.type.value}}</span>
      <i
         :class="`${normalizeSkill(modifier.type.skillType)}-icon`"
         :style="{ color: `var(--${normalizeSkill(modifier.type.skillType)})` }"
      ></i>
    </template>
    <template v-else-if="modifier.type.tag === 'ActionSkillModifier'">
      <span>+ {{modifier.type.value}}</span>
      <i
         :class="`${normalizeSkill(modifier.type.skillType)}-icon`"
         :style="{ color: `var(--${normalizeSkill(modifier.type.skillType)})` }"
      ></i>
    </template>
    <template v-else-if="modifier.type.tag === 'AnySkillValue'">
      <span>+ {{modifier.type.contents}}</span>
      <span class="text">{{ $t('modifier.skillValue') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'DoubleSuccess'">
      <span class="text">{{ $t('modifier.doubleSuccess') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'DoubleDifficulty'">
      <span class="text">{{ $t('modifier.doubleDifficulty') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'MayIgnoreLocationEffectsAndKeywords'">
      <span class="text">{{ $t('modifier.mayIgnoreLocationEffects') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'SkillIconsSubtract'">
      <span class="text">{{ $t('modifier.skillIconsSubtract') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'SkillTestAutomaticallySucceeds'">
      <span class="text">{{ $t('modifier.skillTestAutomaticallySucceeds') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'RevealAnotherChaosToken'">
      <span class="text">{{ $t('modifier.revealAnotherChaosToken') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'CancelAnyChaosTokenAndDrawAnother'">
      <span class="text">{{ $t('modifier.cancelMatchingChaosTokens') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'HandSize'">
      <span class="text">{{modifier.type.contents > 0 ? "+" : "-"}}{{modifier.type.contents}} {{ t('handSize') }}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier'">
      <span class="text">{{modifier.type.contents}}</span>
    </template>
  </div>
</template>

<style scoped>
.modifier {
  flex-shrink: 0;
  align-items: center;
  background: #000;
  border-radius: 100px;
  border: none;
  color: var(--title);
  display: flex;
  gap: 4px;
  padding: 2px 10px;
  text-align: center;
  user-select: none;
  text-decoration: none;
  text-transform: uppercase;
  font-family: sans-serif;
  max-height: fit-content;
  > * {
    pointer-events: none;
  }

  img {
    height: 15px;
  }

  i {
    align-self: flex-end;
  }

  .text {
    font-size: 0.8em;
  }
}
</style>
