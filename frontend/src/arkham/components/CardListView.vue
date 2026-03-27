<script lang="ts" setup>
import { useDbCardStore } from '@/stores/dbCards'
import type { ArkhamDBCard } from '@/stores/dbCards'
import * as Arkham from '@/arkham/types/CardDef'
import { localizeArkhamDBBaseUrl } from '@/arkham/helpers'
import sets from '@/arkham/data/sets.json'

defineProps<{ cards: Arkham.CardDef[] }>()

const store = useDbCardStore()

const cardName = (card: Arkham.CardDef) => {
  const subtitle = card.name.subtitle === null ? "" : `: ${card.name.subtitle}`
  return `${card.name.title}${subtitle}`
}

const levelText = (card: Arkham.CardDef) => {
  if (!card.level || card.level === 0) return ''
  return ` (${card.level})`
}

const cardCost = (card: Arkham.CardDef) => {
  if (card.cost?.tag === "StaticCost") return card.cost.contents
  if (card.cost?.tag === "DynamicCost") return -2
  if (card.cost?.tag === "DeferredCost") return -2
  if (card.cost?.tag === "DiscardAmountCost") return -2
  return null
}

const cardType = (card: Arkham.CardDef) => {
  switch (card.cardType) {
    case "PlayerTreacheryType": return "Treachery"
    case "PlayerEnemyType": return "Enemy"
    default: return card.cardType.replace(/Type$/, '')
  }
}

const cardTraits = (card: Arkham.CardDef) => {
  if (card.cardTraits.length === 0) return ''
  return `${card.cardTraits.join('. ')}.`
}

const cardIcons = (card: Arkham.CardDef) => {
  return card.skills.map((s) => {
    if (s.tag === "SkillIcon") {
      switch (s.contents) {
        case "SkillWillpower": return "willpower"
        case "SkillIntellect": return "intellect"
        case "SkillCombat": return "combat"
        case "SkillAgility": return "agility"
        default: return "unknown"
      }
    }
    if (s.tag == "WildIcon" || s.tag == "WildMinusIcon") return "wild"
    return "unknown"
  })
}

const cardSet = (card: Arkham.CardDef) => {
  const cardCode = parseInt(card.art)
  return sets.find((s) => cardCode >= s.min && cardCode <= s.max)
}

const cardSetText = (card: Arkham.CardDef) => {
  const setNumber = parseInt(card.art.slice(2))
  const language = localStorage.getItem('language') || 'en'
  let setName = ''

  if (language !== 'en') {
    const match: ArkhamDBCard | null = store.getDbCard(card.art)
    if (match) setName = match.pack_name
  }

  if (!setName) {
    const set = cardSet(card)
    if (set) setName = set.name
  }

  if (setName) return `${setName} ${setNumber % 500}`
  return "Unknown"
}
</script>

<template>
  <div class="card-table-wrapper">
    <table class="card-table">
      <thead>
        <tr>
          <th>Name</th>
          <th>Class</th>
          <th>Cost</th>
          <th>Type</th>
          <th>Icons</th>
          <th>Traits</th>
          <th>Set</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="(card, idx) in cards" :key="idx">
          <td><a target="_blank" :href="`${localizeArkhamDBBaseUrl()}/card/${card.art}`">{{ cardName(card) }}{{ levelText(card) }}</a></td>
          <td>{{ card.classSymbols.join(', ') }}</td>
          <td>{{ cardCost(card) }}</td>
          <td>{{ cardType(card) }}</td>
          <td>
            <i v-for="(icon, index) in cardIcons(card)" :key="index" :class="[icon, `${icon}-icon`]"></i>
          </td>
          <td>{{ cardTraits(card) }}</td>
          <td>{{ cardSetText(card) }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<style scoped>
.card-table-wrapper {
  flex: 1;
  overflow-y: auto;
}

.card-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.86rem;
}

.card-table thead {
  position: sticky;
  top: 0;
  z-index: 1;
}

.card-table th {
  text-align: left;
  padding: 8px 10px;
  color: #888;
  background: color-mix(in srgb, var(--background) 96%, transparent);
  border-bottom: 1px solid rgba(255,255,255,0.08);
  font-weight: 600;
  font-size: 0.75rem;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  white-space: nowrap;

  &:first-child { padding-left: 20px; }
}

.card-table td {
  padding: 6px 10px;
  color: #cecece;
  border-bottom: 1px solid rgba(255,255,255,0.04);

  &:first-child { padding-left: 20px; }
}

.card-table tbody tr {
  transition: background 0.1s;
  &:hover { background: rgba(255,255,255,0.04); }
}

i { font-style: normal; }

.willpower { font-size: 1.3em; margin: 0 1px; color: var(--willpower); }
.intellect { font-size: 1.3em; margin: 0 1px; color: var(--intellect); }
.combat    { font-size: 1.3em; margin: 0 1px; color: var(--combat); }
.agility   { font-size: 1.3em; margin: 0 1px; color: var(--agility); }
.wild      { font-size: 1.3em; margin: 0 1px; color: var(--wild); }

a {
  color: var(--spooky-green);
  text-decoration: none;
  font-weight: 500;
  &:hover { opacity: 0.8; }
}
</style>
