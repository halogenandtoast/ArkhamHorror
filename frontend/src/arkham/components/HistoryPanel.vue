<script lang="ts" setup>
import { computed, ref } from 'vue';
import { useStorage } from '@vueuse/core';
import { useI18n } from 'vue-i18n';
import type { Game } from '@/arkham/types/Game';
import type { History, SkillTestResult, DefeatedEnemy } from '@/arkham/types/History';
import Card from '@/arkham/components/Card.vue';
import { useDbCardStore } from '@/stores/dbCards';
import { imgsrc } from '@/arkham/helpers';

const props = defineProps<{
  game: Game
  playerId: string
}>()

const emit = defineEmits<{ close: [] }>()

const { t } = useI18n()
const dbCards = useDbCardStore()

type Scope = 'round' | 'phase' | 'turn'

const scope = useStorage<Scope>('history-panel:scope', 'round')

const scopes: { id: Scope, labelKey: string }[] = [
  { id: 'round', labelKey: 'historyPanel.round' },
  { id: 'phase', labelKey: 'historyPanel.phase' },
  { id: 'turn',  labelKey: 'historyPanel.turn' },
]

function emptyHistory(): History {
  return {
    historyTreacheriesDrawn: [],
    historyEnemiesDrawn: [],
    historyDealtDamageTo: [],
    historyEnemiesDefeated: [],
    historyEnemiesAttackedBy: [],
    historyMoved: 0,
    historyLocationsSuccessfullyInvestigated: [],
    historySuccessfulExplore: false,
    historyActionsCompleted: 0,
    historyActionsSpent: 0,
    historySkillTestsPerformed: [],
    historyPlayedCards: [],
    historyCluesDiscovered: {},
    historyAttacksOfOpportunity: 0,
    historySuccessfulAttacks: 0,
    historySuccessfulEvasions: 0,
    historySuccessfulInvestigations: 0,
    historyResourcesGained: 0,
    historyCardsDrawn: 0,
  }
}

function mergeHistory(a: History, b: History): History {
  const cluesDiscovered: Record<string, number> = { ...a.historyCluesDiscovered }
  for (const [k, v] of Object.entries(b.historyCluesDiscovered)) {
    cluesDiscovered[k] = (cluesDiscovered[k] ?? 0) + v
  }
  return {
    historyTreacheriesDrawn: [...a.historyTreacheriesDrawn, ...b.historyTreacheriesDrawn],
    historyEnemiesDrawn: [...a.historyEnemiesDrawn, ...b.historyEnemiesDrawn],
    historyDealtDamageTo: [...a.historyDealtDamageTo, ...b.historyDealtDamageTo],
    historyEnemiesDefeated: [...a.historyEnemiesDefeated, ...b.historyEnemiesDefeated],
    historyEnemiesAttackedBy: [...a.historyEnemiesAttackedBy, ...b.historyEnemiesAttackedBy],
    historyMoved: a.historyMoved + b.historyMoved,
    historyLocationsSuccessfullyInvestigated: [
      ...a.historyLocationsSuccessfullyInvestigated,
      ...b.historyLocationsSuccessfullyInvestigated.filter(x => !a.historyLocationsSuccessfullyInvestigated.includes(x)),
    ],
    historySuccessfulExplore: a.historySuccessfulExplore || b.historySuccessfulExplore,
    historyActionsCompleted: a.historyActionsCompleted + b.historyActionsCompleted,
    historyActionsSpent: a.historyActionsSpent + b.historyActionsSpent,
    historySkillTestsPerformed: [...a.historySkillTestsPerformed, ...b.historySkillTestsPerformed],
    historyPlayedCards: [...a.historyPlayedCards, ...b.historyPlayedCards],
    historyCluesDiscovered: cluesDiscovered,
    historyAttacksOfOpportunity: a.historyAttacksOfOpportunity + b.historyAttacksOfOpportunity,
    historySuccessfulAttacks: a.historySuccessfulAttacks + b.historySuccessfulAttacks,
    historySuccessfulEvasions: a.historySuccessfulEvasions + b.historySuccessfulEvasions,
    historySuccessfulInvestigations: a.historySuccessfulInvestigations + b.historySuccessfulInvestigations,
    historyResourcesGained: a.historyResourcesGained + b.historyResourcesGained,
    historyCardsDrawn: a.historyCardsDrawn + b.historyCardsDrawn,
  }
}

const histories = computed<Record<string, History>>(() => {
  switch (scope.value) {
    case 'phase': return props.game.phaseHistory ?? {}
    case 'turn':  return props.game.turnHistory ?? {}
    case 'round': {
      const round = props.game.roundHistory ?? {}
      const phase = props.game.phaseHistory ?? {}
      const ids = new Set([...Object.keys(round), ...Object.keys(phase)])
      const merged: Record<string, History> = {}
      for (const iid of ids) {
        merged[iid] = mergeHistory(round[iid] ?? emptyHistory(), phase[iid] ?? emptyHistory())
      }
      return merged
    }
  }
})

const investigatorIds = computed(() => Object.keys(histories.value))

const selectedInvestigatorId = ref<string>(
  histories.value[props.playerId] ? props.playerId : (investigatorIds.value[0] ?? props.playerId)
)

function selectScope(s: Scope) {
  scope.value = s
  if (!histories.value[selectedInvestigatorId.value]) {
    selectedInvestigatorId.value = investigatorIds.value[0] ?? props.playerId
  }
}

function investigatorFor(iid: string) {
  return props.game.investigators[iid] ?? props.game.otherInvestigators[iid] ?? props.game.killedInvestigators[iid]
}

function investigatorName(iid: string): string {
  const inv = investigatorFor(iid)
  return inv ? inv.name.title : iid
}

function investigatorClassKey(iid: string): string {
  const inv = investigatorFor(iid)
  return (inv?.class ?? 'Neutral').toLowerCase()
}

function locationName(lid: string): string {
  return props.game.locations[lid]?.label ?? lid
}

function cardCodeName(code: string): string {
  const stripped = code.startsWith('c') ? code.slice(1) : code
  const dbCard = dbCards.getDbCard(stripped)
  return dbCard?.name ?? code
}

function targetLabel(target: unknown): string {
  if (!target || typeof target !== 'object') return JSON.stringify(target)
  const t = target as { tag?: string; contents?: unknown }
  if (t.tag) {
    if (t.contents !== undefined) {
      const c = typeof t.contents === 'string' ? t.contents : JSON.stringify(t.contents)
      return `${t.tag}: ${c}`
    }
    return t.tag
  }
  return JSON.stringify(target)
}

function skillTestResultLabel(r: SkillTestResult): string {
  switch (r.tag) {
    case 'Unrun': return t('historyPanel.skillResult.unrun')
    case 'SucceededBy': return t('historyPanel.skillResult.succeededBy', { n: r.contents[1] })
    case 'FailedBy':    return t('historyPanel.skillResult.failedBy', { n: r.contents[1] })
  }
}

function skillTypeLabel(s: string): string {
  return s.replace(/^Skill/, '')
}

function skillIconClass(s: string): string {
  switch (s) {
    case 'SkillWillpower': return 'willpower-icon'
    case 'SkillIntellect': return 'intellect-icon'
    case 'SkillCombat':    return 'combat-icon'
    case 'SkillAgility':   return 'agility-icon'
    case 'SkillWild':      return 'wild-icon'
    default: return ''
  }
}

const selected = computed<History | null>(() => histories.value[selectedInvestigatorId.value] ?? null)

const hasAnything = computed(() => {
  const h = selected.value
  if (!h) return false
  return (
    h.historyPlayedCards.length > 0
    || h.historyTreacheriesDrawn.length > 0
    || h.historyEnemiesDrawn.length > 0
    || h.historyEnemiesDefeated.length > 0
    || h.historyEnemiesAttackedBy.length > 0
    || h.historyDealtDamageTo.length > 0
    || h.historyLocationsSuccessfullyInvestigated.length > 0
    || h.historySkillTestsPerformed.length > 0
    || Object.keys(h.historyCluesDiscovered).length > 0
    || h.historyMoved > 0
    || h.historyActionsCompleted > 0
    || h.historyActionsSpent > 0
    || h.historyAttacksOfOpportunity > 0
    || h.historySuccessfulAttacks > 0
    || h.historySuccessfulEvasions > 0
    || h.historySuccessfulInvestigations > 0
    || h.historyResourcesGained > 0
    || h.historyCardsDrawn > 0
    || h.historySuccessfulExplore
  )
})

const counts = computed(() => {
  const h = selected.value
  if (!h) return [] as { key: string, label: string, value: number, icon: string | null }[]
  return ([
    ['historyActionsCompleted', 'historyPanel.field.actionsCompleted', null],
    ['historyActionsSpent',     'historyPanel.field.actionsSpent',     null],
    ['historyMoved',            'historyPanel.field.moved',            null],
    ['historyCardsDrawn',       'historyPanel.field.cardsDrawn',       null],
    ['historyResourcesGained',  'historyPanel.field.resourcesGained',  'resource.png'],
    ['historyAttacksOfOpportunity',    'historyPanel.field.attacksOfOpportunity',    null],
    ['historySuccessfulAttacks',       'historyPanel.field.successfulAttacks',       'health-icon.png'],
    ['historySuccessfulEvasions',      'historyPanel.field.successfulEvasions',      null],
    ['historySuccessfulInvestigations','historyPanel.field.successfulInvestigations','clue-icon.png'],
  ] as const)
    .map(([key, label, icon]) => ({
      key,
      label: t(label),
      value: (h as unknown as Record<string, number>)[key] ?? 0,
      icon: icon ? imgsrc(icon) : null,
    }))
    .filter(({ value }) => value > 0)
})

const cluesDiscovered = computed(() => {
  const h = selected.value
  if (!h) return []
  return Object.entries(h.historyCluesDiscovered).map(([lid, n]) => ({ lid, location: resolveLocationId(lid), n }))
})

const locationsInvestigated = computed(() => {
  const h = selected.value
  if (!h) return []
  return h.historyLocationsSuccessfullyInvestigated.map((lid) => ({ lid, location: resolveLocationId(lid) }))
})

function strippedCode(code: string): string {
  return code.startsWith('c') ? code.slice(1) : code
}

function cardImageFor(code: string): string {
  return imgsrc(`cards/${strippedCode(code)}.avif`)
}

const clueIcon = imgsrc('clue-icon.png')
const healthIcon = imgsrc('health-icon.png')

type EnemyResolution =
  | { tag: 'live', cardCode: string, name: string }
  | { tag: 'defeated', cardCode: string, name: string }
  | { tag: 'unknown', enemyId: string }

type LocationResolution =
  | { tag: 'live', cardCode: string, name: string | null }
  | { tag: 'unknown', locationId: string }

function resolveLocationId(locationId: string): LocationResolution {
  const live = props.game.locations[locationId]
  if (live) {
    const stripped = live.cardCode.startsWith('c') ? live.cardCode.slice(1) : live.cardCode
    const dbCard = dbCards.getDbCard(stripped)
    return { tag: 'live', cardCode: live.cardCode, name: dbCard?.name ?? null }
  }
  return { tag: 'unknown', locationId }
}

function resolveEnemyId(enemyId: string): EnemyResolution {
  const live = props.game.enemies[enemyId]
  if (live) return { tag: 'live', cardCode: live.cardCode, name: cardCodeName(live.cardCode) }
  const h = selected.value
  const def = h?.historyEnemiesDefeated.find(e => e.defeatedEnemyAttrs.id === enemyId)
  if (def) {
    const code = def.defeatedEnemyAttrs.cardCode
    return { tag: 'defeated', cardCode: code, name: cardCodeName(code) }
  }
  return { tag: 'unknown', enemyId }
}

type DamagedTargetView =
  | { kind: 'enemy', enemy: EnemyResolution }
  | { kind: 'investigator', id: string, name: string }
  | { kind: 'asset', cardCode: string | null, name: string }
  | { kind: 'location', id: string, name: string }
  | { kind: 'other', label: string }

const damagedTargets = computed<DamagedTargetView[]>(() => {
  const h = selected.value
  if (!h) return []
  return h.historyDealtDamageTo.map<DamagedTargetView>((target) => {
    const t = target as { tag?: string; contents?: unknown }
    switch (t.tag) {
      case 'EnemyTarget':
      case 'EnemyId':
      case 'ProxyTarget': {
        const id = typeof t.contents === 'string' ? t.contents : null
        if (id) return { kind: 'enemy', enemy: resolveEnemyId(id) }
        break
      }
      case 'InvestigatorTarget': {
        const id = typeof t.contents === 'string' ? t.contents : null
        if (id) return { kind: 'investigator', id, name: investigatorName(id) }
        break
      }
      case 'AssetTarget': {
        const id = typeof t.contents === 'string' ? t.contents : null
        const asset = id ? props.game.assets[id] : null
        if (asset) return { kind: 'asset', cardCode: asset.cardCode, name: cardCodeName(asset.cardCode) }
        break
      }
      case 'LocationTarget': {
        const id = typeof t.contents === 'string' ? t.contents : null
        if (id) return { kind: 'location', id, name: locationName(id) }
        break
      }
    }
    return { kind: 'other', label: targetLabel(target) }
  })
})

const defeatedEnemies = computed(() => {
  const h = selected.value
  if (!h) return []
  return h.historyEnemiesDefeated.map((e) => ({
    id: e.defeatedEnemyAttrs.id,
    cardCode: e.defeatedEnemyAttrs.cardCode,
    name: cardCodeName(e.defeatedEnemyAttrs.cardCode),
    health: e.defeatedEnemyHealth,
  }))
})

const attackedByEnemies = computed(() => {
  const h = selected.value
  if (!h) return []
  return h.historyEnemiesAttackedBy.map((id) => ({ id, enemy: resolveEnemyId(id) }))
})

const skillTestSummary = computed(() => {
  const h = selected.value
  if (!h) return { succeeded: 0, total: 0 }
  const total = h.historySkillTestsPerformed.length
  const succeeded = h.historySkillTestsPerformed.filter(([, r]) => r.tag === 'SucceededBy').length
  return { succeeded, total }
})

function classIconClass(iid: string): string {
  const k = investigatorClassKey(iid)
  switch (k) {
    case 'guardian': return 'guardian-icon'
    case 'seeker':   return 'seeker-icon'
    case 'rogue':    return 'rogue-icon'
    case 'mystic':   return 'mystic-icon'
    case 'survivor': return 'survivor-icon'
    default: return ''
  }
}

function damagedFallbackText(view: DamagedTargetView): string {
  switch (view.kind) {
    case 'enemy':         return view.enemy.tag === 'unknown' ? view.enemy.enemyId : view.enemy.name
    case 'investigator':  return view.name
    case 'asset':         return view.name
    case 'location':      return view.name
    case 'other':         return view.label
  }
}
</script>

<template>
  <div class="history-panel-overlay" @click.self="emit('close')">
    <div class="history-panel">
      <header class="history-header">
        <h3>{{ $t('historyPanel.title') }}</h3>
        <button class="close-btn" @click="emit('close')">×</button>
      </header>

      <nav class="scope-tabs">
        <button
          v-for="s in scopes"
          :key="s.id"
          class="scope-tab"
          :class="{ 'scope-tab--active': scope === s.id }"
          @click="selectScope(s.id)"
        >{{ $t(s.labelKey) }}</button>
      </nav>

      <nav v-if="investigatorIds.length > 1" class="investigator-tabs">
        <button
          v-for="iid in investigatorIds"
          :key="iid"
          class="investigator-tab"
          :class="[
            `investigator-tab--${investigatorClassKey(iid)}`,
            { 'investigator-tab--active': selectedInvestigatorId === iid },
          ]"
          @click="selectedInvestigatorId = iid"
        >
          <span v-if="classIconClass(iid)" :class="['class-glyph', classIconClass(iid)]"></span>
          {{ investigatorName(iid) }}
        </button>
      </nav>

      <section class="history-body">
        <p v-if="!selected || !hasAnything" class="empty">{{ $t('historyPanel.empty') }}</p>

        <template v-else-if="selected">
          <div v-if="counts.length > 0" class="section">
            <h4>{{ $t('historyPanel.section.counts') }}</h4>
            <ul class="pills">
              <li v-for="c in counts" :key="c.key">
                <span
                  v-if="c.key === 'historySuccessfulInvestigations'"
                  class="clue-badge pill-clue"
                >
                  <span class="token-mask token-mask--clue" :style="{ maskImage: `url(${clueIcon})`, WebkitMaskImage: `url(${clueIcon})` }"></span>
                </span>
                <span
                  v-else-if="c.key === 'historySuccessfulAttacks'"
                  class="token-mask pill-icon token-mask--health"
                  :style="{ maskImage: `url(${healthIcon})`, WebkitMaskImage: `url(${healthIcon})` }"
                ></span>
                <img v-else-if="c.icon" :src="c.icon" :alt="c.label" class="pill-icon" />
                <span class="pill-label">{{ c.label }}</span>
                <span class="pill-value">{{ c.value }}</span>
              </li>
              <li v-if="selected.historySuccessfulExplore">
                <span class="pill-label">{{ $t('historyPanel.field.successfulExplore') }}</span>
                <span class="pill-value">✓</span>
              </li>
            </ul>
          </div>

          <div v-if="selected.historyPlayedCards.length > 0" class="section">
            <h4>{{ $t('historyPanel.section.playedCards') }}<span class="section-count">{{ selected.historyPlayedCards.length }}</span></h4>
            <div class="card-grid">
              <Card
                v-for="(card, idx) in selected.historyPlayedCards"
                :key="idx"
                :game="game"
                :card="card"
                :playerId="playerId"
              />
            </div>
          </div>

          <div v-if="selected.historyTreacheriesDrawn.length > 0" class="section">
            <h4>{{ $t('historyPanel.section.treacheriesDrawn') }}<span class="section-count">{{ selected.historyTreacheriesDrawn.length }}</span></h4>
            <div class="enemy-grid">
              <figure v-for="(code, idx) in selected.historyTreacheriesDrawn" :key="idx" class="enemy-tile">
                <img :src="cardImageFor(code)" :alt="cardCodeName(code)" :data-image-id="strippedCode(code)" class="enemy-img" />
              </figure>
            </div>
          </div>

          <div v-if="selected.historyEnemiesDrawn.length > 0" class="section">
            <h4>{{ $t('historyPanel.section.enemiesDrawn') }}<span class="section-count">{{ selected.historyEnemiesDrawn.length }}</span></h4>
            <div class="enemy-grid">
              <figure v-for="(code, idx) in selected.historyEnemiesDrawn" :key="idx" class="enemy-tile">
                <img :src="cardImageFor(code)" :alt="cardCodeName(code)" :data-image-id="strippedCode(code)" class="enemy-img" />
              </figure>
            </div>
          </div>

          <div v-if="defeatedEnemies.length > 0" class="section">
            <h4>{{ $t('historyPanel.section.enemiesDefeated') }}<span class="section-count">{{ defeatedEnemies.length }}</span></h4>
            <div class="enemy-grid">
              <figure v-for="(e, idx) in defeatedEnemies" :key="idx" class="enemy-tile">
                <img :src="cardImageFor(e.cardCode)" :alt="e.name" :data-image-id="strippedCode(e.cardCode)" class="enemy-img" />
                <figcaption>
                  <span class="enemy-meta health-meta">
                    <span class="token-mask token-mask--health" :style="{ maskImage: `url(${healthIcon})`, WebkitMaskImage: `url(${healthIcon})` }"></span>{{ e.health }}
                  </span>
                </figcaption>
              </figure>
            </div>
          </div>

          <div v-if="attackedByEnemies.length > 0" class="section">
            <h4>{{ $t('historyPanel.section.enemiesAttackedBy') }}<span class="section-count">{{ attackedByEnemies.length }}</span></h4>
            <div class="enemy-grid">
              <figure v-for="(item, idx) in attackedByEnemies" :key="idx" class="enemy-tile">
                <img v-if="item.enemy.tag !== 'unknown'" :src="cardImageFor(item.enemy.cardCode)" :alt="item.enemy.name" :data-image-id="strippedCode(item.enemy.cardCode)" class="enemy-img" />
                <figcaption v-else>
                  <span class="enemy-name">{{ item.enemy.enemyId }}</span>
                </figcaption>
              </figure>
            </div>
          </div>

          <div v-if="damagedTargets.length > 0" class="section">
            <h4>{{ $t('historyPanel.section.dealtDamageTo') }}<span class="section-count">{{ damagedTargets.length }}</span></h4>
            <div class="enemy-grid">
              <template v-for="(view, idx) in damagedTargets" :key="idx">
                <figure v-if="view.kind === 'enemy' && view.enemy.tag !== 'unknown'" class="enemy-tile">
                  <img :src="cardImageFor(view.enemy.cardCode)" :alt="view.enemy.name" :data-image-id="strippedCode(view.enemy.cardCode)" class="enemy-img" />
                  <figcaption v-if="view.enemy.tag === 'defeated'">
                    <span class="enemy-meta">{{ $t('historyPanel.enemy.defeated') }}</span>
                  </figcaption>
                </figure>
                <figure v-else-if="view.kind === 'asset' && view.cardCode" class="enemy-tile">
                  <img :src="cardImageFor(view.cardCode)" :alt="view.name" :data-image-id="strippedCode(view.cardCode)" class="enemy-img" />
                </figure>
                <span v-else class="chip">{{ damagedFallbackText(view) }}</span>
              </template>
            </div>
          </div>

          <div v-if="locationsInvestigated.length > 0" class="section">
            <h4>{{ $t('historyPanel.section.locationsInvestigated') }}<span class="section-count">{{ locationsInvestigated.length }}</span></h4>
            <div class="enemy-grid">
              <template v-for="(item, idx) in locationsInvestigated" :key="idx">
                <figure v-if="item.location.tag === 'live'" class="enemy-tile">
                  <img :src="cardImageFor(item.location.cardCode)" :alt="item.location.name ?? ''" :data-image-id="strippedCode(item.location.cardCode)" class="enemy-img" />
                </figure>
                <span v-else class="chip">{{ item.lid }}</span>
              </template>
            </div>
          </div>

          <div v-if="cluesDiscovered.length > 0" class="section">
            <h4>{{ $t('historyPanel.section.cluesDiscovered') }}<span class="section-count">{{ cluesDiscovered.length }}</span></h4>
            <div class="enemy-grid">
              <template v-for="c in cluesDiscovered" :key="c.lid">
                <figure v-if="c.location.tag === 'live'" class="enemy-tile">
                  <img :src="cardImageFor(c.location.cardCode)" :alt="c.location.name ?? ''" :data-image-id="strippedCode(c.location.cardCode)" class="enemy-img" />
                  <figcaption>
                    <span class="enemy-meta clue-meta">
                      <span class="clue-badge">
                        <span class="token-mask token-mask--clue" :style="{ maskImage: `url(${clueIcon})`, WebkitMaskImage: `url(${clueIcon})` }"></span>
                      </span>
                      ×{{ c.n }}
                    </span>
                  </figcaption>
                </figure>
                <span v-else class="chip">{{ c.lid }} <span class="pill-value">×{{ c.n }}</span></span>
              </template>
            </div>
          </div>

          <div v-if="selected.historySkillTestsPerformed.length > 0" class="section">
            <h4>
              {{ $t('historyPanel.section.skillTests') }}
              <span class="section-count">{{ skillTestSummary.succeeded }}/{{ skillTestSummary.total }}</span>
            </h4>
            <ul class="rows">
              <li v-for="(stp, idx) in selected.historySkillTestsPerformed" :key="idx" class="skill-row">
                <span class="check-name skill-icons">
                  <template v-if="stp[0].length > 0">
                    <span
                      v-for="(s, i) in stp[0]"
                      :key="i"
                      :class="['skill-icon', skillIconClass(s)]"
                      :title="skillTypeLabel(s)"
                    ></span>
                  </template>
                  <span v-else>—</span>
                </span>
                <span
                  class="check-detail skill-result"
                  :class="{ 'skill-result--succeeded': stp[1].tag === 'SucceededBy', 'skill-result--failed': stp[1].tag === 'FailedBy' }"
                >{{ skillTestResultLabel(stp[1]) }}</span>
              </li>
            </ul>
          </div>
        </template>
      </section>
    </div>
  </div>
</template>

<style scoped>
.history-panel-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.7);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 900;
}

.history-panel {
  background: #1a1a2e;
  border: 1px solid #444;
  border-radius: 8px;
  padding: 1rem 1.25rem 1.25rem;
  width: min(90vw, 900px);
  height: 65vh;
  display: flex;
  flex-direction: column;
  color: #eee;
}

.history-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 0.5rem;

  h3 {
    margin: 0;
    font-size: 1.1rem;
    color: #adf;
  }
}

.close-btn {
  background: transparent;
  border: none;
  color: #eee;
  font-size: 1.5rem;
  cursor: pointer;
  line-height: 1;
  padding: 0 0.4rem;
}

.scope-tabs, .investigator-tabs {
  display: flex;
  gap: 0.25rem;
  margin-bottom: 0.5rem;
  flex-wrap: wrap;
}

.scope-tab, .investigator-tab {
  background: #232346;
  border: 1px solid #333;
  color: #ccc;
  padding: 0.35rem 0.75rem;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.9rem;
}

.scope-tab--active {
  background: #345;
  color: #fff;
  border-color: #67a;
}

.investigator-tabs {
  margin-bottom: 0.75rem;
  font-size: 0.85rem;
}

.investigator-tab {
  border-left-width: 4px;
  padding-left: 0.6rem;
}

.investigator-tab--guardian  { border-left-color: var(--guardian); }
.investigator-tab--mystic    { border-left-color: var(--mystic); }
.investigator-tab--seeker    { border-left-color: var(--seeker); }
.investigator-tab--rogue     { border-left-color: var(--rogue); }
.investigator-tab--survivor  { border-left-color: var(--survivor); }
.investigator-tab--neutral   { border-left-color: var(--neutral); }

.investigator-tab--active.investigator-tab--guardian { background: var(--guardian-dark); border-color: var(--guardian); color: #fff; }
.investigator-tab--active.investigator-tab--mystic   { background: var(--mystic-dark);   border-color: var(--mystic);   color: #fff; }
.investigator-tab--active.investigator-tab--seeker   { background: var(--seeker-dark);   border-color: var(--seeker);   color: #fff; }
.investigator-tab--active.investigator-tab--rogue    { background: var(--rogue-dark);    border-color: var(--rogue);    color: #fff; }
.investigator-tab--active.investigator-tab--survivor { background: var(--survivor-dark); border-color: var(--survivor); color: #fff; }
.investigator-tab--active.investigator-tab--neutral  { background: var(--neutral-dark);  border-color: var(--neutral);  color: #fff; }

.class-glyph {
  display: inline-block;
  margin-right: 0.35rem;
  font-size: 0.95em;
  vertical-align: middle;
}

.investigator-tab--guardian .class-glyph { color: var(--guardian); }
.investigator-tab--mystic .class-glyph   { color: var(--mystic); }
.investigator-tab--seeker .class-glyph   { color: var(--seeker); }
.investigator-tab--rogue .class-glyph    { color: var(--rogue); }
.investigator-tab--survivor .class-glyph { color: var(--survivor); }
.investigator-tab--active .class-glyph   { color: #fff; }

.history-body {
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  padding-right: 0.25rem;
}

.section {
  margin-bottom: 1rem;

  h4 {
    margin: 0 0 0.4rem;
    font-size: 0.85rem;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: #adf;
    opacity: 0.85;
    border-left: 3px solid #345;
    padding-left: 0.5rem;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }
}

.section-count {
  display: inline-flex;
  align-items: center;
  min-width: 1.4rem;
  height: 1.05rem;
  padding: 0 0.4rem;
  border-radius: 999px;
  background: #2c2c52;
  color: #d8e8ff;
  font-size: 0.7rem;
  font-weight: 600;
  letter-spacing: 0;
  text-transform: none;
  justify-content: center;
}

.empty {
  opacity: 0.6;
  font-style: italic;
  text-align: center;
  padding: 2rem 0;
}

.pills, .chips, .rows {
  list-style: none;
  padding: 0;
  margin: 0;
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem;
}

.rows {
  flex-direction: column;
  gap: 0.2rem;
}

.pills li, .chips li {
  background: #232346;
  border: 1px solid #333;
  border-radius: 999px;
  padding: 0.2rem 0.65rem;
  font-size: 0.85rem;
  display: inline-flex;
  align-items: center;
  gap: 0.4rem;
}

.rows li {
  background: #232346;
  border: 1px solid #333;
  border-radius: 4px;
  padding: 0.2rem 0.5rem;
  font-size: 0.85rem;
  display: flex;
  gap: 0.5rem;
  align-items: baseline;
}

.pill-label {
  opacity: 0.8;
}

.pill-value {
  font-weight: 600;
  color: #fff;
}

.pill-icon {
  width: 14px;
  height: 14px;
  flex-shrink: 0;
}

.pill-clue {
  width: 16px;
  height: 16px;
}

.inline-icon {
  width: 12px;
  height: 12px;
  vertical-align: middle;
  margin-right: 2px;
}

.clue-meta {
  display: inline-flex;
  align-items: center;
  gap: 4px;
  color: #ffe18a;
}

.clue-badge {
  position: relative;
  display: inline-flex;
  width: 22px;
  height: 22px;
  background: radial-gradient(circle at 35% 30%, #dabe9e 0%, #8f855b 60%, #403b19 100%);
  border-radius: 50%;
  box-shadow: 0 0 1px rgba(0,0,0,0.6) inset;
  overflow: hidden;
  flex-shrink: 0;
}

.token-mask {
  display: inline-block;
  mask-repeat: no-repeat;
  mask-position: center;
  mask-size: contain;
  -webkit-mask-repeat: no-repeat;
  -webkit-mask-position: center;
  -webkit-mask-size: contain;
}

.token-mask--health {
  background-color: #d44;
  width: 12px;
  height: 12px;
}

.token-mask--horror {
  background-color: #4aa3ff;
  width: 12px;
  height: 12px;
}

.token-mask--clue {
  background-color: #ffffff;
  position: absolute;
  bottom: 5%;
  left: 50%;
  transform: translateX(-50%);
  width: 89%;
  height: 89%;
  -webkit-mask-position: center bottom;
  mask-position: center bottom;
  filter: drop-shadow(0 0 1px rgba(0,0,0,0.6));
}

.health-meta {
  display: inline-flex;
  align-items: center;
  gap: 3px;
  color: #ff8888;
  font-weight: 600;
}

.check-name { font-weight: 500; }
.check-detail { font-size: 0.8rem; opacity: 0.8; font-style: italic; }

.skill-icons {
  display: inline-flex;
  align-items: center;
  gap: 4px;
  font-size: 1rem;
  line-height: 1;
}

.skill-icon {
  display: inline-block;
  font-size: 1.05rem;
  line-height: 1;
}

.skill-icon.willpower-icon { color: var(--willpower); }
.skill-icon.intellect-icon { color: var(--intellect); }
.skill-icon.combat-icon    { color: var(--combat); }
.skill-icon.agility-icon   { color: var(--agility); }
.skill-icon.wild-icon      { color: var(--wild); }

.skill-result--succeeded { color: #6fd56f; font-style: normal; opacity: 1; }
.skill-result--failed    { color: #ff8888; font-style: normal; opacity: 1; }

.card-grid {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
}

.enemy-grid {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  align-items: flex-start;
}

.enemy-tile {
  margin: 0;
  width: var(--card-width);
  min-width: var(--card-width);
  display: flex;
  flex-direction: column;
  gap: 0.2rem;
  font-size: 0.7rem;
  line-height: 1.15;
  text-align: center;
}

.enemy-img {
  width: 100%;
  border-radius: 4px;
  display: block;
  box-shadow: 0 2px 4px rgba(0,0,0,0.4);
}

.enemy-name {
  display: block;
  font-weight: 500;
  line-height: 1.2;
}

.enemy-meta {
  display: block;
  font-size: 0.75rem;
  color: rgba(238, 238, 238, 0.7);
}

.chip {
  background: #232346;
  border: 1px solid #333;
  border-radius: 999px;
  padding: 0.2rem 0.65rem;
  font-size: 0.85rem;
  display: inline-flex;
  align-items: baseline;
}
</style>
