<script lang="ts" setup>
import { useDbCardStore } from '@/stores/dbCards'
import type { ArkhamDBCard } from '@/stores/dbCards'
import * as Arkham from '@/arkham/types/CardDef'
import { localizeArkhamDBBaseUrl } from '@/arkham/helpers'
import sets from '@/arkham/data/sets.json'

const props = withDefaults(defineProps<{ cards: Arkham.CardDef[], attachments?: Record<string, Arkham.CardDef[]> }>(), {
  attachments: () => ({}),
})

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

const attachedCards = (card: Arkham.CardDef) => props.attachments[card.art] ?? []
</script>

<template>
  <div class="card-table-wrapper">
    <table class="card-table">
      <thead>
        <tr>
          <th>{{ $t('cardsList.name') }}</th>
          <th>{{ $t('cardsList.class') }}</th>
          <th>{{ $t('cardsList.cost') }}</th>
          <th>{{ $t('cardsList.type') }}</th>
          <th>{{ $t('cardsList.icons') }}</th>
          <th class="traits-col">{{ $t('cardsList.traits') }}</th>
          <th class="set-col">{{ $t('cardsList.set') }}</th>
        </tr>
      </thead>
      <tbody>
        <template v-for="(card, idx) in cards" :key="idx">
          <tr>
            <td><a target="_blank" :href="`${localizeArkhamDBBaseUrl()}/card/${card.art}`">{{ cardName(card) }}{{ levelText(card) }}</a></td>
            <td>
              <span class="class-text">
                <span v-for="(sym, i) in card.classSymbols" :key="sym" :class="`class-sym ${sym.toLowerCase()}-sym`">{{ sym }}{{ i < card.classSymbols.length - 1 ? ', ' : '' }}</span>
              </span>
              <span class="class-icons">
                <span v-for="sym in card.classSymbols" :key="sym" :class="[`${sym.toLowerCase()}-icon`, `${sym.toLowerCase()}-sym`]"></span>
              </span>
            </td>
            <td>{{ cardCost(card) }}</td>
            <td>{{ cardType(card) }}</td>
            <td>
              <i v-for="(icon, index) in cardIcons(card)" :key="index" :class="[icon, `${icon}-icon`]"></i>
            </td>
            <td class="traits-col">{{ cardTraits(card) }}</td>
            <td class="set-col">{{ cardSetText(card) }}</td>
          </tr>
          <tr v-if="attachedCards(card).length > 0" class="attachments-row">
            <td colspan="7">
              <div class="attachments-list">
                <div class="attachments-heading">
                  <font-awesome-icon icon="paperclip" /> Attached cards for {{ cardName(card) }}
                </div>
                <div class="attachment-pills">
                  <a
                    v-for="(attached, attachedIdx) in attachedCards(card)"
                    :key="`${attached.art}-${attachedIdx}`"
                    class="attachment-pill"
                    target="_blank"
                    :href="`${localizeArkhamDBBaseUrl()}/card/${attached.art}`"
                  >
                    <span class="attachment-count">{{ attachedIdx + 1 }}</span>
                    {{ cardName(attached) }}{{ levelText(attached) }}
                  </a>
                </div>
              </div>
            </td>
          </tr>
        </template>
      </tbody>
    </table>
  </div>
</template>

<style scoped>
.card-table-wrapper {
  flex: 1;
  overflow-x: auto;
  overflow-y: auto;
}

.card-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.86rem;
  @media (max-width: 768px) {
    width: auto;
    min-width: 580px;
  }
}

.card-table th {
  position: sticky;
  top: 0;
  z-index: 1;
  text-align: left;
  padding: 11px 12px;
  color: #a8a8a8;
  background: var(--box-background);
  border-bottom: 2px solid rgba(255,255,255,0.1);
  box-shadow: 0 2px 8px rgba(0,0,0,0.35);
  font-weight: 700;
  font-size: 0.68rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  white-space: nowrap;

  &:first-child { padding-left: 20px; }

  @media (max-width: 768px) {
    padding: 8px 8px;
    font-size: 0.62rem;
    letter-spacing: 0.06em;
    &:first-child { padding-left: 12px; }
  }
}

.card-table td {
  padding: 7px 12px;
  color: #d0d0d0;
  border-bottom: 1px solid rgba(255,255,255,0.04);

  &:first-child { padding-left: 20px; }

  @media (max-width: 768px) {
    padding: 5px 8px;
    &:first-child { padding-left: 12px; }
  }
}

.card-table tbody tr {
  transition: background 0.1s;
  &:hover { background: rgba(255,255,255,0.05); }
  &:nth-child(even) { background: rgba(255,255,255,0.02); }
  &:nth-child(even):hover { background: rgba(255,255,255,0.05); }
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

.attachments-row td {
  padding-top: 0;
  padding-bottom: 10px;
  background: rgba(200, 169, 110, 0.035);
}

.attachments-list {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  gap: 7px;
  width: 100%;
  padding: 9px 10px;
  background: linear-gradient(135deg, rgba(200, 169, 110, 0.14), rgba(255, 255, 255, 0.035));
  border: 1px solid rgba(200, 169, 110, 0.24);
  border-radius: 9px;
  box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.04);
}

.attachments-heading {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  margin-right: 3px;
  color: #c8a96e;
  font-size: 0.68rem;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.attachment-pills {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
}

.attachment-pill {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  padding: 3px 8px 3px 4px;
  color: #f0e2c0;
  background: rgba(0, 0, 0, 0.28);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 999px;
  font-size: 0.74rem;
  font-weight: 600;
  &:hover { background: rgba(200, 169, 110, 0.16); opacity: 1; }
}

.attachment-count {
  display: inline-grid;
  place-items: center;
  width: 16px;
  height: 16px;
  color: #1d170f;
  background: #c8a96e;
  border-radius: 50%;
  font-size: 0.62rem;
  font-weight: 900;
}

.class-icons {
  display: none;
  gap: 2px;
  span[class$="-icon"] { font-size: 1.1em; }
}

.guardian-sym  { color: var(--guardian); }
.seeker-sym    { color: var(--seeker); }
.rogue-sym     { color: var(--rogue); }
.mystic-sym    { color: var(--mystic); }
.survivor-sym  { color: var(--survivor); }
.neutral-sym   { color: var(--neutral); }

@media (max-width: 768px) {
  .class-text { display: none; }
  .class-icons { display: inline-flex; }
}
</style>
