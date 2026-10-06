<script lang="ts" setup>
/* Every in-play entity, grouped by kind, with the id its placement points at
 * resolved against the map that should hold it.
 *
 * The reason this exists: an entity can survive in the game state while the
 * thing it is placed on does not. Nothing renders it then -- the UI draws a
 * treachery inside its host location -- so an ability it still publishes
 * becomes an unclickable prompt and the game looks deadlocked with no card to
 * point at. Searching for Izzie outlived R'lyeh Streets that way (#5812).
 * "Dangling" is that check, and it is the first thing to look at when a
 * question names a source you cannot find on the board.
 */
import { computed, ref, watch } from 'vue'
import type { Game } from '@/arkham/types/Game'
import type { Placement } from '@/arkham/types/Placement'
import DebugLocation from '@/arkham/components/debug/Location.vue'
import DebugEnemy from '@/arkham/components/debug/Enemy.vue'
import DebugAsset from '@/arkham/components/debug/Asset.vue'
import DebugConcealedCard from '@/arkham/components/debug/ConcealedCard.vue'
import { useDebug } from '@/arkham/debug'
import { useDbCardStore } from '@/stores/dbCards'
import { useEscape } from '@/composable/escape'

const props = defineProps<{ game: Game, playerId: string }>()
const emit = defineEmits<{ close: [] }>()

/* The four entity kinds with a debug window of their own. A row opens its own
 * window straight from the list, so finding a thing and acting on it are not
 * two separate hunts. */
type PanelKind = 'location' | 'enemy' | 'asset' | 'concealed'

const openPanel = ref<{ kind: PanelKind, id: string } | null>(null)

/* The window is a Draggable, which teleports to #modal at z-index 10 -- under
 * this overlay. Rather than fight the stacking, step aside while it is open:
 * v-show keeps the list mounted, so the filter and scroll position survive. */
const panelEntity = computed(() => {
  const open = openPanel.value
  if (!open) return null
  const from = {
    location: props.game.locations,
    enemy: props.game.enemies,
    asset: props.game.assets,
    concealed: props.game.concealed,
  }[open.kind]
  return from?.[open.id] ?? null
})

// A window can remove its own entity (discard, remove from game). Come back to
// the list rather than leaving both hidden behind a window with nothing in it.
watch(panelEntity, (entity) => {
  if (openPanel.value && !entity) openPanel.value = null
})

const panelLocation = computed(() =>
  openPanel.value?.kind === 'location' ? props.game.locations[openPanel.value.id] ?? null : null)
const panelEnemy = computed(() =>
  openPanel.value?.kind === 'enemy' ? props.game.enemies[openPanel.value.id] ?? null : null)
const panelAsset = computed(() =>
  openPanel.value?.kind === 'asset' ? props.game.assets[openPanel.value.id] ?? null : null)
const panelConcealed = computed(() =>
  openPanel.value?.kind === 'concealed' ? props.game.concealed[openPanel.value.id] ?? null : null)

// Escape belongs to the window while one is open; the stack in `useEscape`
// already prefers it, and this keeps the list out of the running entirely.
useEscape(() => emit('close'), () => openPanel.value === null)

const debug = useDebug()
const dbCards = useDbCardStore()
const filter = ref('')
const danglingOnly = ref(false)

function cardName(code: string | undefined): string {
  if (!code) return ''
  const stripped = code.startsWith('c') ? code.slice(1) : code
  const dbCard = dbCards.getDbCard(stripped) ?? dbCards.getDbCard(stripped.replace(/[a-h]$/, ''))
  return dbCard?.name ?? ''
}

/* Which map each placement expects to find its referenced ids in. A tag absent
 * from here references nothing (Limbo, NextToAct, InPosition, ...) and can
 * never dangle. */
type Realm = 'locations' | 'investigators' | 'assets' | 'enemies' | 'agendas'

function references(placement: Placement | null | undefined): { realm: Realm, id: string }[] {
  if (!placement) return []
  const loc = (id: string) => ({ realm: 'locations' as Realm, id })
  const inv = (id: string) => ({ realm: 'investigators' as Realm, id })
  switch (placement.tag) {
    case 'AtLocation':
    case 'AttachedToLocation':
      return [loc(placement.contents)]
    case 'AtLocations':
      return placement.contents.map(loc)
    case 'BetweenLocations':
      return [loc(placement.contents[0]), loc(placement.contents[1])]
    case 'InThreatArea':
    case 'FacedownInThreatArea':
    case 'InPlayArea':
    case 'StillInHand':
    case 'StillInDiscard':
    case 'HiddenInHand':
    case 'OnTopOfDeck':
      return [inv(placement.contents)]
    case 'AttachedToAsset':
      return [{ realm: 'assets', id: placement.contents[0] }]
    case 'InVehicle':
      return [{ realm: 'assets', id: placement.contents }]
    case 'AsSwarm':
      return [{ realm: 'enemies', id: placement.swarmHost }]
    case 'AttachedToAgenda':
      return [{ realm: 'agendas', id: placement.contents }]
    default:
      return []
  }
}

const realmKeys = computed<Record<Realm, Set<string>>>(() => ({
  locations: new Set(Object.keys(props.game.locations ?? {})),
  investigators: new Set(Object.keys(props.game.investigators ?? {})),
  assets: new Set(Object.keys(props.game.assets ?? {})),
  enemies: new Set(Object.keys(props.game.enemies ?? {})),
  agendas: new Set(Object.keys(props.game.agendas ?? {})),
}))

function shortId(id: string): string {
  return id.includes('-') ? id.slice(0, 8) : id
}

function placementText(placement: Placement | null | undefined): string {
  if (!placement) return '—'
  switch (placement.tag) {
    case 'AtLocations':
      return `AtLocations ${placement.contents.map(shortId).join(', ')}`
    case 'BetweenLocations':
      return `BetweenLocations ${placement.contents.map(shortId).join(', ')}`
    case 'AttachedToAsset':
      return `AttachedToAsset ${shortId(placement.contents[0])}`
    case 'AsSwarm':
      return `AsSwarm ${shortId(placement.swarmHost)}`
    case 'InPosition':
      return `InPosition ${placement.contents.x},${placement.contents.y}`
    default:
      return 'contents' in placement && typeof placement.contents === 'string'
        ? `${placement.tag} ${shortId(placement.contents)}`
        : placement.tag
  }
}

interface Row {
  id: string
  cardCode: string
  name: string
  extra: string
  placement: string
  dangling: string[]
  panel: PanelKind | null
  /* The `Target` constructor to discard this row through, for kinds with no
   * debug window of their own -- a treachery stranded on a location that left
   * play is only reachable from here (#5812). */
  discardTag: string | null
}

function rowsFor(
  entities: Record<string, unknown> | undefined,
  panel: PanelKind | null = null,
  extraOf: (e: Record<string, unknown>) => string = () => '',
  discardTag: string | null = null,
): Row[] {
  return Object.entries(entities ?? {}).map(([id, raw]) => {
    const e = raw as Record<string, unknown>
    const placement = e.placement as Placement | null | undefined
    const keys = realmKeys.value
    const dangling = references(placement)
      .filter(({ realm, id: ref }) => !keys[realm].has(ref))
      .map(({ realm, id: ref }) => `${shortId(ref)} not in ${realm}`)
    const cardCode = (e.cardCode as string) ?? ''
    return {
      id,
      cardCode,
      name: cardName(cardCode),
      extra: extraOf(e),
      placement: placementText(placement),
      dangling,
      panel,
      discardTag,
    }
  })
}

const groups = computed(() => {
  const g = props.game
  const seq = (e: Record<string, unknown>) => {
    const s = e.sequence as { step?: number, side?: string } | undefined
    return s ? `${s.step ?? '?'}${s.side ?? ''}` : ''
  }
  const owner = (e: Record<string, unknown>) => (e.owner as string) ?? ''
  return [
    { label: 'Locations', rows: rowsFor(g.locations, 'location', (e) => (e.label as string) ?? '') },
    { label: 'Enemies', rows: rowsFor(g.enemies, 'enemy') },
    { label: 'Assets', rows: rowsFor(g.assets, 'asset', (e) => (e.controller as string) ?? (e.owner as string) ?? '') },
    { label: 'Treacheries', rows: rowsFor(g.treacheries, null, owner, 'TreacheryTarget') },
    { label: 'Events', rows: rowsFor(g.events, null, owner) },
    { label: 'Skills', rows: rowsFor(g.skills, null, owner) },
    { label: 'Stories', rows: rowsFor(g.stories) },
    { label: 'Acts', rows: rowsFor(g.acts, null, seq) },
    { label: 'Agendas', rows: rowsFor(g.agendas, null, seq) },
    { label: 'Concealed', rows: rowsFor(g.concealed, 'concealed') },
  ]
})

function discardRow(row: Row) {
  if (!row.discardTag) return
  debug.send(props.game.id, {
    tag: 'Discard',
    contents: [null, { tag: 'GameSource' }, { tag: row.discardTag, contents: row.id }],
  })
}

function matches(row: Row): boolean {
  if (danglingOnly.value && row.dangling.length === 0) return false
  const q = filter.value.trim().toLowerCase()
  if (!q) return true
  return [row.id, row.cardCode, row.name, row.extra, row.placement, ...row.dangling]
    .some((field) => field.toLowerCase().includes(q))
}

const visibleGroups = computed(() =>
  groups.value
    .map((group) => ({ ...group, visible: group.rows.filter(matches) }))
    .filter((group) => group.visible.length > 0),
)

const danglingCount = computed(() =>
  groups.value.reduce((n, g) => n + g.rows.filter((r) => r.dangling.length > 0).length, 0),
)

const totalCount = computed(() => groups.value.reduce((n, g) => n + g.rows.length, 0))

async function copyJson() {
  const g = props.game
  const payload = {
    locations: g.locations,
    enemies: g.enemies,
    assets: g.assets,
    treacheries: g.treacheries,
    events: g.events,
    skills: g.skills,
    stories: g.stories,
    acts: g.acts,
    agendas: g.agendas,
    concealed: g.concealed,
  }
  try {
    await navigator.clipboard.writeText(JSON.stringify(payload, null, 2))
  } catch {
    // Clipboard is unavailable outside a secure context; nothing to recover.
  }
}
</script>

<template>
  <div v-show="!openPanel" class="entity-browser-overlay" @click.self="emit('close')">
    <div class="entity-browser" role="dialog" aria-modal="true" aria-labelledby="entity-browser-title">
      <header class="entity-browser-header">
        <h2 id="entity-browser-title">Entities</h2>
        <span class="entity-browser-count">
          {{ totalCount }} in play<template v-if="danglingCount > 0">, <span class="dangling-count">{{ danglingCount }} dangling</span></template>
        </span>
        <button type="button" aria-label="Close entity browser" @click="emit('close')">×</button>
      </header>

      <div class="entity-browser-controls">
        <input v-model="filter" type="search" placeholder="Filter by id, code, name, placement…" />
        <label class="entity-browser-toggle">
          <input v-model="danglingOnly" type="checkbox" />
          <span>Dangling only</span>
        </label>
        <button type="button" class="entity-browser-copy" @click="copyJson">Copy JSON</button>
      </div>

      <div class="entity-browser-body">
        <p v-if="visibleGroups.length === 0" class="entity-browser-empty">Nothing matches.</p>
        <section v-for="group in visibleGroups" :key="group.label">
          <h3>{{ group.label }} <span class="group-count">{{ group.visible.length }}/{{ group.rows.length }}</span></h3>
          <table>
            <tbody>
              <tr v-for="row in group.visible" :key="row.id" :class="{ 'row--dangling': row.dangling.length > 0 }">
                <td class="col-id" :title="row.id">{{ shortId(row.id) }}</td>
                <td class="col-code">{{ row.cardCode }}</td>
                <td class="col-name">{{ row.name }}<span v-if="row.extra" class="extra"> · {{ row.extra }}</span></td>
                <td class="col-placement">
                  {{ row.placement }}
                  <span v-for="d in row.dangling" :key="d" class="dangling-flag">{{ d }}</span>
                </td>
                <td class="col-actions">
                  <button
                    v-if="row.panel"
                    type="button"
                    class="row-debug"
                    @click="openPanel = { kind: row.panel, id: row.id }"
                  >Debug</button>
                  <button
                    v-else-if="row.discardTag"
                    type="button"
                    class="row-debug row-discard"
                    @click="discardRow(row)"
                  >Discard</button>
                </td>
              </tr>
            </tbody>
          </table>
        </section>
      </div>
    </div>
  </div>

  <DebugLocation
    v-if="panelLocation"
    :game="game"
    :location="panelLocation"
    :playerId="playerId"
    @close="openPanel = null"
  />
  <DebugEnemy
    v-else-if="panelEnemy"
    :game="game"
    :enemy="panelEnemy"
    :playerId="playerId"
    @close="openPanel = null"
  />
  <DebugAsset
    v-else-if="panelAsset"
    :game="game"
    :asset="panelAsset"
    :playerId="playerId"
    @close="openPanel = null"
  />
  <DebugConcealedCard
    v-else-if="panelConcealed"
    :game="game"
    :card="panelConcealed"
    :playerId="playerId"
    @close="openPanel = null"
  />
</template>

<style scoped>
.entity-browser-overlay {
  position: fixed;
  inset: 0;
  z-index: var(--z-modal-overlay, 10000);
  background: rgba(0, 0, 0, 0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 16px;
}

.entity-browser {
  width: min(900px, 100%);
  max-height: 90dvh;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  border: 1px solid rgba(255, 255, 255, 0.18);
  border-radius: 8px;
  background: var(--background);
  box-shadow: 0 12px 40px rgba(0, 0, 0, 0.45);
}

.entity-browser-header {
  display: flex;
  align-items: center;
  gap: 12px;
  color: white;
  padding: 16px 20px;
  border-bottom: 1px solid var(--box-border);
}

.entity-browser-header h2 {
  margin: 0;
  font-family: teutonic, sans-serif;
  font-size: 1.3rem;
  font-weight: normal;
  letter-spacing: .04em;
}

.entity-browser-count {
  flex: 1;
  font-size: 0.8rem;
  opacity: 0.7;
}

.dangling-count {
  color: var(--survivor);
  opacity: 1;
}

.entity-browser-header button {
  border: 0;
  background: transparent;
  color: white;
  cursor: pointer;
  font-size: 1.5rem;
  line-height: 1;
  padding: 0;
}

.entity-browser-controls {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 12px 20px;
  border-bottom: 1px solid var(--box-border);
  color: var(--text);
}

.entity-browser-controls input[type="search"] {
  flex: 1;
  min-width: 0;
  padding: 6px 10px;
  border-radius: 4px;
  border: 1px solid var(--box-border);
  background: var(--background-dark);
  color: var(--text);
}

.entity-browser-toggle {
  display: flex;
  align-items: center;
  gap: 6px;
  font-size: 0.8rem;
  white-space: nowrap;
}

.entity-browser-copy {
  padding: 6px 10px;
  white-space: nowrap;
}

.entity-browser-body {
  padding: 12px 20px 20px;
  overflow-y: auto;
  min-height: 0;
  color: var(--text);
}

.entity-browser-empty {
  opacity: 0.7;
  font-size: 0.85rem;
}

.entity-browser-body h3 {
  margin: 16px 0 4px;
  font-size: 0.8rem;
  text-transform: uppercase;
  letter-spacing: .08em;
  opacity: 0.75;
}

.entity-browser-body section:first-child h3 {
  margin-top: 0;
}

.group-count {
  opacity: 0.5;
  letter-spacing: 0;
  text-transform: none;
}

table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.78rem;
}

td {
  padding: 3px 8px 3px 0;
  vertical-align: top;
  border-bottom: 1px solid rgba(255, 255, 255, 0.06);
}

.row--dangling {
  background: color-mix(in srgb, var(--survivor) 18%, transparent);
}

.col-id, .col-code, .col-placement {
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  white-space: nowrap;
}

.col-id {
  opacity: 0.6;
}

.col-name {
  width: 40%;
}

.extra {
  opacity: 0.55;
}

.col-actions {
  text-align: right;
  white-space: nowrap;
}

.row-debug {
  padding: 1px 8px;
  font-size: 0.72rem;
  border: 1px solid var(--box-border);
  border-radius: 3px;
  background: var(--background-dark);
  color: var(--text);
  cursor: pointer;
}

.row-debug:hover {
  background: var(--button);
  border-color: var(--select);
}

.row-discard:hover {
  background: var(--survivor);
  border-color: var(--survivor);
}

.dangling-flag {
  display: inline-block;
  margin-left: 8px;
  padding: 0 6px;
  border-radius: 3px;
  background: var(--survivor);
  color: white;
  font-family: var(--font-family);
  white-space: nowrap;
}

@media (max-width: 800px) {
  table, tbody, tr, td {
    display: block;
    width: auto;
  }

  tr {
    padding: 6px 0;
    border-bottom: 1px solid rgba(255, 255, 255, 0.06);
  }

  td {
    border: 0;
    padding: 0;
  }

  .col-placement {
    white-space: normal;
  }

  /* The row is a stack at this width, so the button belongs under it rather
     than flush right against nothing. */
  .col-actions {
    text-align: left;
    padding-top: 4px;
  }
}
</style>
