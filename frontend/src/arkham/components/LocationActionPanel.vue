<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Location } from '@/arkham/types/Location'
import { MessageType } from '@/arkham/types/Message'
import { cardId, toCardContents } from '@/arkham/types/Card'
import { useGameChoices } from '@/arkham/composables/useGameChoices'
import { useCardNames } from '@/arkham/composables/useCardNames'
import { sourceEntityId, type EntityChoice } from '@/arkham/locationChoices'
import { ownInvestigator } from '@/arkham/ownInvestigator'
import AbilityButton from '@/arkham/components/AbilityButton.vue'

/* Every action the server offers right now, listed in one panel beside the scene the way the offline demo
 * does: basic actions, what the location and scenario offer, enemies, cards in play and cards in hand.
 * It only arranges the current choices; each row sends the index its choice already had. */
const props = defineProps<{ game: Game; playerId: string; location: Location | null }>()
const emit = defineEmits<{ (e: 'choose', index: number): void; (e: 'close'): void }>()

const { t } = useI18n()
const nameOf = useCardNames()
const choices = useGameChoices(() => props.game, () => props.playerId)
const me = computed(() => ownInvestigator(props.game, props.playerId))

type Known = { id: string; cardCode?: string }
type SectionKey = 'basic' | 'scenario' | 'enemies' | 'inPlay' | 'hand' | 'other'
const SECTIONS: SectionKey[] = ['basic', 'scenario', 'enemies', 'inPlay', 'hand', 'other']

// Which part of the board an entity id belongs to decides the section its abilities fall under.
const KIND_SECTION: [string, SectionKey][] = [
  ['investigators', 'basic'], ['locations', 'scenario'], ['acts', 'scenario'], ['agendas', 'scenario'], ['stories', 'scenario'],
  ['enemies', 'enemies'], ['assets', 'inPlay'], ['events', 'inPlay'], ['skills', 'inPlay'], ['treacheries', 'inPlay'],
]
function lookup(id: string): { section: SectionKey; name: string } | null {
  for (const [kind, section] of KIND_SECTION) {
    const found = (props.game[kind as keyof Game] as unknown as Record<string, Known> | undefined)?.[id]
    if (found) return { section, name: nameOf(found.cardCode ?? found.id) }
  }
  return null
}

const handNames = computed(() => new Map((me.value?.hand ?? []).map((c) => [cardId(c), nameOf(toCardContents(c).cardCode)])))

type Row = { index: number; group: string | null; label: string | null; contents: EntityChoice['contents'] | null }

const sections = computed(() => {
  const rows: Record<SectionKey, Row[]> = { basic: [], scenario: [], enemies: [], inPlay: [], hand: [], other: [] }
  const mine = me.value?.id
  const ability = (section: SectionKey, index: number, group: string | null, contents: EntityChoice['contents']) =>
    rows[section].push({ index, group, label: null, contents })
  const plain = (section: SectionKey, index: number, group: string | null, label: string) =>
    rows[section].push({ index, group, label, contents: null })

  choices.value.forEach((choice, index) => {
    switch (choice.tag) {
      case MessageType.FIGHT_LABEL:
      case MessageType.FIGHT_LABEL_WITH_SKILL:
      case MessageType.EVADE_LABEL:
      case MessageType.EVADE_LABEL_WITH_SKILL:
      case MessageType.ENGAGE_LABEL:
        ability('enemies', index, lookup(choice.enemyId)?.name ?? null, choice)
        break
      case MessageType.ABILITY_LABEL: {
        const sid = sourceEntityId(choice.ability.source)
        if (choice.ability.basic) ability('basic', index, null, choice)
        else if (sid && handNames.value.has(sid)) ability('hand', index, handNames.value.get(sid) ?? null, choice)
        else {
          const known = sid ? lookup(sid) : null
          ability(known?.section ?? 'other', index, known && known.section !== 'basic' ? known.name : null, choice)
        }
        break
      }
      case MessageType.TARGET_LABEL: {
        const target = choice.target.contents
        if (typeof target === 'string' && handNames.value.has(target)) plain('hand', index, null, t('locationMap.playCard', { name: handNames.value.get(target) }))
        break
      }
      case MessageType.COMPONENT_LABEL: {
        const c = choice.component
        if (c?.tag === 'InvestigatorComponent' && c.tokenType === 'ResourceToken' && c.investigatorId === mine) plain('basic', index, null, t('locationMap.gainResource'))
        else if (c?.tag === 'InvestigatorDeckComponent' && props.playerId === me.value?.playerId) plain('basic', index, null, t('locationMap.drawCard'))
        break
      }
      case MessageType.END_TURN_BUTTON:
        if (choice.investigatorId === mine) plain('basic', index, null, t('locationMap.endTurn'))
        break
      case MessageType.SKIP_TRIGGERS_BUTTON:
        if (choice.investigatorId === mine) plain('basic', index, null, t('locationMap.skipTriggers'))
        break
    }
  })
  return SECTIONS.filter((key) => rows[key].length > 0).map((key) => ({ key, title: t(`locationMap.section.${key}`), rows: rows[key] }))
})

</script>

<template>
  <aside class="panel" :aria-label="t('locationMap.actions')">
    <header>
      <h3>{{ location ? nameOf(location.cardCode) : t('locationMap.actions') }}</h3>
      <button class="close" :aria-label="t('locationMap.close')" @click="emit('close')">✕</button>
    </header>
    <p v-if="sections.length === 0" class="empty">{{ t('locationMap.noActions') }}</p>
    <section v-for="s in sections" :key="s.key" class="section">
      <h4>{{ s.title }}</h4>
      <template v-for="(r, i) in s.rows" :key="r.index">
        <h5 v-if="r.group && r.group !== s.rows[i - 1]?.group">{{ r.group }}</h5>
        <button v-if="r.label" class="row" @click="emit('choose', r.index)">{{ r.label }}</button>
        <AbilityButton v-else-if="r.contents" class="row" :game="game" :ability="r.contents" @choose="emit('choose', r.index)" />
      </template>
    </section>
  </aside>
</template>

<style scoped>
.panel { display: flex; flex-direction: column; gap: 8px; width: 270px; max-height: 100%; overflow-y: auto; padding: 12px; background: #0f1813f2; border: 1px solid #c6a873aa; border-radius: 8px; box-shadow: 0 8px 28px #000a; color: #e5e2d4; }
header { display: flex; justify-content: space-between; align-items: center; gap: 8px; }
h3 { margin: 0; font: 1.4rem Georgia, serif; }
.close { background: transparent; border: 1px solid #ffffff33; border-radius: 4px; color: #c9d0c3; padding: 2px 8px; cursor: pointer; }
.section { display: flex; flex-direction: column; gap: 4px; border-top: 1px solid #ffffff14; padding-top: 6px; }
h4 { margin: 0; font-size: 0.75rem; color: #c6d6b4; }
h5 { margin: 4px 0 0; font-size: 0.7rem; color: #e0a595; font-weight: 600; }
.row, .section :deep(button) { width: 100%; text-align: left; justify-content: flex-start; padding: 7px 10px; font-size: 0.78rem; background: #17261f !important; border: 1px solid #ffffff22 !important; border-radius: 4px; color: #e5e2d4 !important; cursor: pointer; }
.row:hover, .section :deep(button:hover:not(:disabled)) { background: #213529 !important; border-color: #e6c584 !important; }
.section :deep(button:disabled) { opacity: 0.5; }
.empty { margin: 0; font-size: 0.8rem; color: #98a38f; }
</style>
