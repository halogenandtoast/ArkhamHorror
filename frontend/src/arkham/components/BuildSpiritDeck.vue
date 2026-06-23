<script lang="ts" setup>
import { computed, ref, inject, onMounted } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Question } from '@/arkham/types/Question'
import { QuestionType } from '@/arkham/types/Question'
import { capitalize, imgsrc, type InvestigatorClass } from '@/arkham/helpers'
import { cardImage } from '@/arkham/cardImages'
import { useDbCardStore } from '@/stores/dbCards'

const DISCRIMINATOR = 'laidToRest.buildSpiritDeck'

interface BuildSpiritDeckPayload {
  cardCodes: string[]
  count: number
  // Cards always in the spirit deck (e.g. Vengeful Shade); shown but not chosen.
  fixed: string[]
}

const props = defineProps<{
  game: Game
  playerId: string
  question: Question
}>()

const { t } = useI18n()
const dbCardStore = useDbCardStore()
const scenarioSpecificAnswer = inject<(key: string, value: unknown) => Promise<void>>('scenarioSpecificAnswer')

// Ensure the card metadata is loaded so names/classes resolve.
onMounted(() => { void dbCardStore.initDbCards() })

// Narrow the (intentionally loosely-typed) PickScenarioSpecific payload.
const payload = computed<BuildSpiritDeckPayload>(() => {
  const q = props.question
  if (
    q.tag === QuestionType.PICK_SCENARIO_SPECIFIC &&
    Array.isArray(q.contents) &&
    q.contents[0] === DISCRIMINATOR
  ) {
    const inner = q.contents[1] as Partial<BuildSpiritDeckPayload> | undefined
    return {
      cardCodes: inner?.cardCodes ?? [],
      count: inner?.count ?? 0,
      fixed: inner?.fixed ?? [],
    }
  }
  return { cardCodes: [], count: 0, fixed: [] }
})

const count = computed(() => payload.value.count)

const ALL_CLASSES: InvestigatorClass[] = ['guardian', 'seeker', 'rogue', 'mystic', 'survivor', 'neutral']

interface Candidate {
  code: string
  name: string
  klass: InvestigatorClass | null
  image: string
}

// Resolve a backend card code to display data. The backend sends player-card
// codes with a leading "c" (e.g. "c01018"), but the db card store and the
// card-image helper key on the bare code, so we strip it for lookups. The
// original `code` is kept for selection/answer so it round-trips unchanged.
const resolveCandidate = (code: string): Candidate => {
  const bare = code.replace(/^c/, '')
  const dbCard = dbCardStore.getDbCard(bare)
  const faction = dbCard?.faction_code as InvestigatorClass | undefined
  const klass = faction && (ALL_CLASSES as string[]).includes(faction) ? faction : null
  const name = dbCard
    ? (dbCard.subname ? `${dbCard.name}: ${dbCard.subname}` : dbCard.name)
    : bare
  return { code, name, klass, image: cardImage(bare) }
}

const candidates = computed<Candidate[]>(() => payload.value.cardCodes.map(resolveCandidate))

// Cards always in the spirit deck (Vengeful Shade) — shown but not selectable.
const fixedCandidates = computed<Candidate[]>(() => payload.value.fixed.map(resolveCandidate))

const searchText = ref('')
const filterClasses = ref<InvestigatorClass[]>([])

function toggleClass(c: InvestigatorClass) {
  const idx = filterClasses.value.indexOf(c)
  filterClasses.value = idx === -1
    ? [...filterClasses.value, c]
    : filterClasses.value.filter((x) => x !== c)
}

const filteredCandidates = computed<Candidate[]>(() => {
  const term = searchText.value.trim().toLowerCase()
  const classSet = filterClasses.value.length > 0 ? new Set(filterClasses.value) : null
  return candidates.value.filter((c) => {
    // Cards whose class can't be resolved (missing data) are never hidden behind a
    // class chip, so they stay selectable.
    if (classSet && c.klass && !classSet.has(c.klass)) return false
    if (term) {
      const nameMatches = c.name.toLowerCase().includes(term)
      const codeMatches = c.code.toLowerCase().includes(term)
      if (!nameMatches && !codeMatches) return false
    }
    return true
  })
})

const selected = ref<string[]>([])

const isSelected = (code: string) => selected.value.includes(code)
const atMax = computed(() => selected.value.length >= count.value)

function toggleCard(code: string) {
  if (isSelected(code)) {
    selected.value = selected.value.filter((c) => c !== code)
  } else if (!atMax.value) {
    selected.value = [...selected.value, code]
  }
}

const selectedCandidates = computed<Candidate[]>(() =>
  selected.value.flatMap((code) => {
    const candidate = candidates.value.find((c) => c.code === code)
    return candidate ? [candidate] : []
  }),
)

const canConfirm = computed(() => selected.value.length === count.value && count.value > 0)

function confirm() {
  if (!canConfirm.value || !scenarioSpecificAnswer) return
  void scenarioSpecificAnswer(DISCRIMINATOR, { cardCodes: selected.value })
}
</script>

<template>
  <div class="build-spirit-deck">
    <header class="bsd-header">
      <div class="bsd-title-row">
        <font-awesome-icon :icon="['fas', 'ghost']" class="bsd-ghost-icon" aria-hidden="true" />
        <h2 class="bsd-title">{{ t('standalone.laidToRest.buildSpiritDeck.title') }}</h2>
      </div>
      <p class="bsd-instructions" v-html="t('standalone.laidToRest.setup.spiritDeck')"></p>
      <div
        class="bsd-counter"
        :class="{ complete: canConfirm }"
      >{{ t('standalone.laidToRest.buildSpiritDeck.counter', { selected: selected.length, count }) }}</div>
    </header>

    <div class="bsd-spirit-deck">
      <span class="bsd-selected-heading">{{ t('standalone.laidToRest.buildSpiritDeck.selectedHeading') }}</span>
      <div class="bsd-slots">
        <template v-for="i in count" :key="`slot-${i}`">
          <button
            v-if="selectedCandidates[i - 1]"
            type="button"
            class="bsd-slot filled"
            :title="selectedCandidates[i - 1].name"
            @click="toggleCard(selectedCandidates[i - 1].code)"
          >
            <img
              class="card"
              data-is-spirit="true"
              :src="selectedCandidates[i - 1].image"
              :alt="selectedCandidates[i - 1].name"
            />
            <span class="bsd-selected-remove" aria-hidden="true">×</span>
          </button>
          <div v-else class="bsd-slot empty" aria-hidden="true">
            <img class="bsd-slot-icon" :src="imgsrc('slots/ally.png')" alt="" />
          </div>
        </template>
        <div
          v-for="fixed in fixedCandidates"
          :key="`fixed-${fixed.code}`"
          class="bsd-slot fixed"
          :title="fixed.name"
        >
          <img class="card" data-is-spirit="true" :src="fixed.image" :alt="fixed.name" />
        </div>
      </div>
    </div>

    <div class="bsd-toolbar">
      <div class="class-filters">
        <button
          v-for="iclass in ALL_CLASSES"
          :key="iclass"
          type="button"
          class="class-pill"
          :class="{ [iclass]: filterClasses.includes(iclass), active: filterClasses.includes(iclass) }"
          :title="capitalize(iclass)"
          @click.prevent="toggleClass(iclass)"
        >
          <span :class="`${iclass}-icon`"></span>
          <span class="pill-label">{{ capitalize(iclass) }}</span>
        </button>
      </div>
      <input
        v-model="searchText"
        class="bsd-search"
        type="search"
        :placeholder="t('standalone.laidToRest.buildSpiritDeck.search')"
      />
    </div>

    <div v-if="filteredCandidates.length > 0" class="bsd-grid">
      <button
        v-for="candidate in filteredCandidates"
        :key="candidate.code"
        type="button"
        class="bsd-card"
        :class="{ selected: isSelected(candidate.code), disabled: atMax && !isSelected(candidate.code) }"
        :aria-pressed="isSelected(candidate.code)"
        @click="toggleCard(candidate.code)"
      >
        <div class="bsd-card-art">
          <img
            class="card bsd-card-image"
            :data-is-spirit="isSelected(candidate.code) ? 'true' : undefined"
            :src="candidate.image"
            :alt="candidate.name"
            loading="lazy"
          />
          <font-awesome-icon
            v-if="isSelected(candidate.code)"
            :icon="['fas', 'ghost']"
            class="spirit-icon"
            aria-hidden="true"
          />
          <span v-if="isSelected(candidate.code)" class="bsd-check" aria-hidden="true">✓</span>
        </div>
        <span class="bsd-card-name">{{ candidate.name }}</span>
      </button>
    </div>
    <div v-else class="bsd-empty">{{ t('standalone.laidToRest.buildSpiritDeck.noResults') }}</div>

    <footer class="bsd-footer">
      <button
        type="button"
        class="bsd-confirm"
        :disabled="!canConfirm"
        @click="confirm"
      >{{ t('standalone.laidToRest.buildSpiritDeck.confirm') }}</button>
    </footer>
  </div>
</template>

<style scoped>
.build-spirit-deck {
  display: flex;
  flex-direction: column;
  gap: 14px;
  width: min(100%, 1100px);
  margin: 0 auto;
  padding: 20px 16px 96px;
  box-sizing: border-box;
  color: #e7ecf3;
}

.bsd-header {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.bsd-title-row {
  display: flex;
  align-items: center;
  gap: 10px;
}

.bsd-ghost-icon {
  flex-shrink: 0;
  font-size: 1.5em;
  color: #8fb7e3;
  filter: drop-shadow(0 0 8px rgba(120, 160, 230, 0.6));
}

.bsd-title {
  margin: 0;
  font-family: Teutonic, sans-serif;
  font-size: 1.7em;
  letter-spacing: 0.04em;
  color: #cdd9ec;
}

.bsd-instructions {
  margin: 0;
  font-size: 0.95em;
  line-height: 1.5;
  font-style: italic;
  color: #b9c4d6;
  border-left: 3px solid rgba(120, 160, 230, 0.5);
  padding-left: 12px;
}

.bsd-instructions :deep(em) {
  font-style: normal;
  color: #9cc0ff;
}

.bsd-counter {
  align-self: flex-start;
  padding: 4px 12px;
  border-radius: 999px;
  font-weight: 700;
  font-size: 0.95em;
  letter-spacing: 0.06em;
  color: #cdd9ec;
  background: rgba(49, 54, 155, 0.25);
  border: 1px solid rgba(120, 160, 230, 0.35);
  transition: background 0.15s, border-color 0.15s, color 0.15s;
}

.bsd-counter.complete {
  color: #fff;
  background: rgba(110, 134, 64, 0.85);
  border-color: rgba(154, 196, 78, 0.55);
}

/* ── Toolbar (mirrors DeckToolbar styling) ── */
.bsd-toolbar {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 8px;
}

.class-filters {
  display: flex;
  flex-wrap: wrap;
  gap: 4px;
  flex: 1;
}

.class-pill {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 5px;
  padding: 6px 10px;
  font-size: 0.78rem;
  font-weight: 600;
  color: rgba(255, 255, 255, 0.7);
  background: rgba(0, 0, 0, 0.3);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 4px;
  cursor: pointer;
  transition: background 0.12s, color 0.12s, border-color 0.12s;
  user-select: none;

  &:hover { border-color: rgba(255, 255, 255, 0.2); color: #fff; }

  &.active.guardian { background: var(--guardian-extra-dark); border-color: var(--guardian-dark); color: #fff; }
  &.active.seeker   { background: var(--seeker-extra-dark);   border-color: var(--seeker-dark);   color: #fff; }
  &.active.rogue    { background: var(--rogue-extra-dark);    border-color: var(--rogue-dark);    color: #fff; }
  &.active.mystic   { background: var(--mystic-extra-dark);   border-color: var(--mystic-dark);   color: #fff; }
  &.active.survivor { background: var(--survivor-extra-dark); border-color: var(--survivor-dark); color: #fff; }
  &.active.neutral  { background: var(--neutral-extra-dark);  border-color: var(--neutral-dark);  color: #fff; }

  span[class$="-icon"] { font-size: 1em; }
}

.bsd-search {
  padding: 7px 10px;
  font-size: 0.85rem;
  color: #ccc;
  background: rgba(0, 0, 0, 0.3);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 4px;
  outline: none;
  width: 200px;
  transition: border-color 0.12s;

  &::placeholder { color: var(--button); }
  &:focus { border-color: rgba(120, 160, 230, 0.5); }
}

/* ── Spirit deck (always shown; 9 slots, unaffected by the filter) ── */
.bsd-spirit-deck {
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 10px 12px;
  background: rgba(49, 54, 155, 0.12);
  border: 1px solid rgba(120, 160, 230, 0.25);
  border-radius: 8px;
}

.bsd-selected-heading {
  font-size: 0.72em;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: #9cb0d4;
}

.bsd-slots {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
}

.bsd-slot {
  position: relative;
  width: 52px;
  border-radius: 4px;
  padding: 0;
  line-height: 0;
}

.bsd-slot.filled {
  border: 1px solid rgba(120, 160, 230, 0.6);
  background: transparent;
  cursor: pointer;
  transition: transform 0.12s, border-color 0.12s;

  img {
    width: 100%;
    border-radius: 3px;
    display: block;
  }

  &:hover {
    transform: translateY(-2px);
    border-color: #c9534a;
  }
}

.bsd-slot.empty {
  aspect-ratio: 5 / 7;
  display: flex;
  align-items: center;
  justify-content: center;
  border: 1px dashed rgba(120, 160, 230, 0.4);
  background: rgba(255, 255, 255, 0.03);
}

.bsd-slot-icon {
  width: 58%;
  height: auto;
  opacity: 0.35;
}

/* Vengeful Shade and any other required spirit-deck card: shown, not removable. */
.bsd-slot.fixed {
  border: 1px solid rgba(201, 83, 74, 0.7);
  background: transparent;
  cursor: default;

  img {
    width: 100%;
    border-radius: 3px;
    display: block;
  }
}

.bsd-selected-remove {
  position: absolute;
  top: -7px;
  right: -7px;
  width: 18px;
  height: 18px;
  border-radius: 50%;
  background: #c9534a;
  color: #fff;
  font-size: 13px;
  line-height: 18px;
  text-align: center;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.5);
}

/* ── Grid ── */
.bsd-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
  gap: 12px;
}

.bsd-card {
  position: relative;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 6px;
  padding: 6px;
  background: rgba(255, 255, 255, 0.04);
  border: 2px solid transparent;
  border-radius: 8px;
  cursor: pointer;
  transition: transform 0.12s, border-color 0.12s, background 0.12s, box-shadow 0.12s;
  -webkit-tap-highlight-color: transparent;

  &:hover {
    transform: translateY(-3px);
    background: rgba(255, 255, 255, 0.07);
    box-shadow: 0 8px 18px rgba(0, 0, 0, 0.35);
  }

  &.selected {
    border-color: #6f8fd6;
    background: rgba(49, 54, 155, 0.22);
    box-shadow: 0 0 12px rgba(120, 160, 230, 0.45);
  }

  &.disabled {
    opacity: 0.4;
    cursor: not-allowed;
    &:hover { transform: none; box-shadow: none; background: rgba(255, 255, 255, 0.04); }
  }
}

.bsd-card-art {
  position: relative;
  width: 100%;
  line-height: 0;
}

.bsd-card-image {
  width: 100%;
  border-radius: 6px;
  display: block;
}

/* Mirrors the IsSpirit asset rendering: a little ghost over the ally icon. */
.spirit-icon {
  position: absolute;
  bottom: 4%;
  right: 2%;
  font-size: 1.2em;
  color: rgba(180, 230, 255, 0.95);
  filter:
    drop-shadow(0 0 1px rgba(0, 0, 0, 0.9))
    drop-shadow(0 1px 2px rgba(0, 0, 0, 0.8))
    drop-shadow(0 0 5px rgba(130, 200, 255, 0.7));
  pointer-events: none;
}

.bsd-check {
  position: absolute;
  top: 10px;
  right: 10px;
  width: 26px;
  height: 26px;
  border-radius: 50%;
  background: #6f8fd6;
  color: #fff;
  font-size: 15px;
  font-weight: 700;
  line-height: 26px;
  text-align: center;
  box-shadow: 0 2px 6px rgba(0, 0, 0, 0.5);
}

.bsd-card-name {
  font-size: 0.74em;
  line-height: 1.2;
  text-align: center;
  color: #c4cedd;
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
}

.bsd-empty {
  padding: 40px 16px;
  text-align: center;
  color: var(--button);
  font-size: 0.9em;
}

/* ── Footer / confirm ── */
.bsd-footer {
  position: sticky;
  bottom: 0;
  display: flex;
  justify-content: center;
  padding: 12px 0;
  margin-top: 4px;
  background: linear-gradient(to top, var(--background, #26283b) 60%, transparent);
  z-index: var(--z-index-10);
}

.bsd-confirm {
  min-width: 240px;
  height: 48px;
  border-radius: 6px;
  border: 1px solid rgba(255, 255, 255, 0.1);
  background: rgba(110, 134, 64, 0.95);
  color: #fff;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  font-size: 0.9em;
  font-weight: 700;
  cursor: pointer;
  box-shadow: 0 5px 18px rgba(0, 0, 0, 0.3);
  transition: transform 0.12s, background 0.16s, box-shadow 0.16s;

  &:hover:not(:disabled) {
    transform: translateY(-1px);
    background: rgba(110, 134, 64, 1);
    box-shadow: 0 10px 28px rgba(0, 0, 0, 0.4);
  }

  &:active:not(:disabled) { transform: translateY(0); }

  &:disabled {
    opacity: 0.5;
    cursor: not-allowed;
    box-shadow: none;
    transform: none;
  }
}

@media (max-width: 768px) {
  .build-spirit-deck { padding: 14px 10px 96px; }

  .class-filters { flex: none; width: 100%; }

  .class-pill {
    flex: 1;
    height: 38px;
    padding: 0;
    .pill-label { display: none; }
    span[class$="-icon"] { font-size: 1.15em; }
  }

  .bsd-search { flex: 1; width: 100%; }

  .bsd-grid { grid-template-columns: repeat(auto-fill, minmax(96px, 1fr)); gap: 8px; }

  .bsd-confirm { min-width: 0; width: 100%; }
}
</style>
