<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import { MessageType } from '@/arkham/types/Message'
import { useGameChoices } from '@/arkham/composables/useGameChoices'
import { useCardNames } from '@/arkham/composables/useCardNames'
import { sourceEntityId, type EntityChoice } from '@/arkham/locationChoices'
import AbilityButton from '@/arkham/components/AbilityButton.vue'

/* Every action the server offers right now, in one list under the scene -- the way the offline demo keeps
 * its actions in a bar rather than behind each object. Buttons are grouped by what they belong to
 * (the investigator, the location, an enemy, a card in play ...). Each still sends the index it had. */
const props = defineProps<{ game: Game; playerId: string }>()
const emit = defineEmits<{ (e: 'choose', index: number): void }>()

const { t } = useI18n()
const nameOf = useCardNames()
const choices = useGameChoices(() => props.game, () => props.playerId)

type Known = { id: string; cardCode?: string }
const KINDS = ['investigators', 'locations', 'enemies', 'assets', 'events', 'skills', 'treacheries', 'acts', 'agendas', 'stories'] as const

// Where an entity id lives decides how its group is ranked and named.
function lookup(id: string): { rank: number; name: string } | null {
  for (const [rank, kind] of KINDS.entries()) {
    const found = (props.game[kind] as unknown as Record<string, Known> | undefined)?.[id]
    if (found) return { rank, name: nameOf(found.cardCode ?? found.id) }
  }
  return null
}

type Group = { key: string; rank: number; name: string; entries: EntityChoice[] }

const groups = computed<Group[]>(() => {
  const byKey = new Map<string, Group>()
  const add = (entityId: string | null, index: number, contents: EntityChoice['contents']) => {
    const known = entityId ? lookup(entityId) : null
    const key = entityId ?? 'other'
    const group = byKey.get(key) ?? { key, rank: known?.rank ?? 99, name: known?.name ?? t('locationMap.otherActions'), entries: [] }
    group.entries.push({ index, contents })
    byKey.set(key, group)
  }
  choices.value.forEach((choice, index) => {
    switch (choice.tag) {
      case MessageType.FIGHT_LABEL:
      case MessageType.FIGHT_LABEL_WITH_SKILL:
      case MessageType.EVADE_LABEL:
      case MessageType.EVADE_LABEL_WITH_SKILL:
      case MessageType.ENGAGE_LABEL:
        add(choice.enemyId, index, choice)
        break
      case MessageType.ABILITY_LABEL:
        add(sourceEntityId(choice.ability.source), index, choice)
        break
    }
  })
  return [...byKey.values()].sort((a, b) => a.rank - b.rank)
})
</script>

<template>
  <section class="action-list" :aria-label="t('locationMap.actions')">
    <h3>{{ t('locationMap.actions') }}</h3>
    <p v-if="groups.length === 0" class="empty">{{ t('locationMap.noActions') }}</p>
    <div v-else class="groups">
      <div v-for="g in groups" :key="g.key" class="group" :class="{ enemy: g.rank === 2 }">
        <h4>{{ g.name }}</h4>
        <AbilityButton v-for="c in g.entries" :key="c.index" :game="game" :ability="c.contents" @choose="emit('choose', c.index)" />
      </div>
    </div>
  </section>
</template>

<style scoped>
.action-list { padding: 8px 12px 10px; background: #101a14; border-top: 1px solid #a68b5540; color: #e5e2d4; }
h3 { margin: 0 0 6px; font-size: 0.65rem; letter-spacing: 0.18em; text-transform: uppercase; color: #c6a873; }
.groups { display: flex; flex-wrap: wrap; gap: 10px; }
.group { display: flex; flex-direction: column; gap: 4px; min-width: 190px; max-width: 320px; flex: 1 1 190px; padding: 6px 8px; background: #16221b; border: 1px solid #ffffff14; border-radius: 6px; }
.group h4 { margin: 0; font: 0.9rem Georgia, serif; }
.group.enemy h4 { color: #e0a595; }
.empty { margin: 0; font-size: 0.8rem; color: #98a38f; }
</style>
