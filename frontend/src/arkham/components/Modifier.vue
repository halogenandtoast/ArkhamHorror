<script setup lang="ts">
import { computed } from 'vue';
import { Modifier } from '@/arkham/types/Modifier';
import { Source } from '@/arkham/types/Source';
import { Game } from '@/arkham/types/Game';

const props = defineProps<{ modifier: Modifier, game: Game }>()

function sourceCardCode(source: Source) {
  if (source.tag === 'LocationSource') {
    const location = props.game.locations[source.contents]
    if (!location) return null
    const { cardCode, revealed } = location
    const suffix = revealed ? '' : 'b'

    return `${cardCode.replace('c', '')}${suffix}`
  }

  if (source.tag === 'AssetSource') {
    const asset = props.game.assets[source.contents]
    if (!asset) return null

    const mutated = asset.mutated ? `_${asset.mutated}` : ''
    if (asset.flipped) {
      if (asset.cardCode === "c90052") return "90052b"
      return null
    }
    return `${asset.cardCode.replace('c', '')}${mutated}`
  }

  if (source.tag === 'TreacherySource') {
    const treachery = props.game.treacheries[source.contents]
    if (!treachery) return null
    return `${treachery.cardCode.replace('c', '')}`
  }

  if (source.tag === 'EnemySource') {
    const enemy = props.game.enemies[source.contents]
    if (!enemy) return null

    const { cardCode, flipped } = enemy
    const suffix = flipped ? 'b' : ''
    return `${cardCode.replace('c', '')}${suffix}`
  }

  if (source.tag === 'AbilitySource') {
    const [inner,] = source.contents
    return sourceCardCode(inner)
  }

  if (source.tag === 'EventSource') {
    const event = props.game.events[source.contents]
    if (!event) return null

    const mutated = event.mutated ? `_${event.mutated}` : ''
    return `${event.cardCode.replace('c', '')}${mutated}`
  }

  if (source.tag === 'InvestigatorSource') {
    return `${source.contents.replace('c', '')}`
  }

  return null
}

const modifierSource = computed(() => {
  if(props.modifier.card) {
    return props.modifier.card.contents.cardCode.replace(/^c/, '')
  }

  return sourceCardCode(props.modifier.source)
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
      <span>{{cannotCommitCardsToWords(modifier.type)}}</span>
    </template>
    <template v-else-if="modifier.type.tag === 'Difficulty'">
      <span><template v-if="modifier.type.contents >= 0">+</template>{{modifier.type.contents}}</span>
      Difficulty
    </template>
    <template v-else-if="modifier.type.tag === 'CancelEffects'">
      <span class="text">Cancel Effects</span>
    </template>
    <template v-else-if="modifier.type.tag === 'CannotPerformSkillTest'">
      <span class="text">Cannot Perform Skill Test</span>
    </template>
    <template v-else-if="modifier.type.tag === 'DiscoveredClues'">
      <span>+{{modifier.type.contents}}</span>
      <img :src="imgsrc(`clue.png`)" />
    </template>
    <template v-else-if="modifier.type.tag === 'SkillTestResultValueModifier'">
      <span class="text">Result</span> <span>{{modifier.type.contents > 0 ? '+' : ''}}{{modifier.type.contents}}</span>
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
      <span class="text">Skill Value</span>
    </template>
    <template v-else-if="modifier.type.tag === 'DoubleSuccess'">
      <span class="text">Double Success</span>
    </template>
    <template v-else-if="modifier.type.tag === 'DoubleDifficulty'">
      <span class="text">Double Difficulty</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'MayIgnoreLocationEffectsAndKeywords'">
      <span class="text">May Ignore Location Effects</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'SkillIconsSubtract'">
      <span class="text">Skill Icons Subtract</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'SkillTestAutomaticallySucceeds'">
      <span class="text">Skill test automatically succeeds</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'RevealAnotherChaosToken'">
      <span class="text">Reveal another chaos token</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier' && modifier.type.contents === 'CancelAnyChaosTokenAndDrawAnother'">
      <span class="text">Cancel matching chaos tokens and reveal another</span>
    </template>
    <template v-else-if="modifier.type.tag === 'HandSize'">
      <span class="text">{{modifier.type.contents > 0 ? "+" : "-"}}{{modifier.type.contents}} Hand Size</span>
    </template>
    <template v-else-if="modifier.type.tag === 'OtherModifier'">
      <span class="text">{{modifier.type.contents}}</span>
    </template>
  </div>
</template>

<style scoped lang="scss">
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
