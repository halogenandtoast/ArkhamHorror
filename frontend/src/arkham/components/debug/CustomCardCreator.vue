<script lang="ts" setup>
/* Debug tool: invent a card mid-game.
 *
 * The def built here is a real CardDef -- the backend stores it on the game and
 * registers it so every def and builder lookup resolves it, and the card is then
 * run by a generic runner for its type. There is no card text: whatever the def
 * says (stats, keywords, traits, icons, slots, uses) is exactly what the card
 * does. */
import { computed, onMounted, reactive, ref } from 'vue'
import * as Api from '@/arkham/api'
import { useDebug } from '@/arkham/debug'
import { useCardStore } from '@/stores/cards'
import {
  customCards,
  mintCustomCardCode,
  registerCustomCards,
  renderCardPlaceholder,
  unregisterCustomCard,
  type CustomCard,
} from '@/arkham/customCards'
import { libraryCards, removeFromLibrary, saveToLibrary } from '@/arkham/customCardLibrary'
import type { Game } from '@/arkham/types/Game'

const props = defineProps<{ game: Game; investigatorId: string }>()
const emit = defineEmits<{ close: [] }>()

const debug = useDebug()
const cardStore = useCardStore()

type Placement = 'play' | 'hand' | 'campaignDeck' | 'encounterDeck'

const CARD_TYPES = [
  { value: 'AssetType', label: 'Asset (player back)' },
  { value: 'EventType', label: 'Event' },
  { value: 'SkillType', label: 'Skill' },
  { value: 'PlayerTreacheryType', label: 'Treachery (weakness)' },
  { value: 'PlayerEnemyType', label: 'Enemy (weakness)' },
  { value: 'EnemyType', label: 'Enemy (encounter)' },
  { value: 'TreacheryType', label: 'Treachery (encounter)' },
  { value: 'EncounterAssetType', label: 'Asset (encounter back)' },
  { value: 'LocationType', label: 'Location' },
  { value: 'StoryType', label: 'Story' },
] as const

const CLASSES = ['Guardian', 'Seeker', 'Rogue', 'Mystic', 'Survivor', 'Neutral', 'Mythos']
const SLOTS = ['HandSlot', 'BodySlot', 'AllySlot', 'AccessorySlot', 'ArcaneSlot', 'TarotSlot', 'HeadSlot']
const USE_TYPES = ['Ammo', 'Charge', 'Secret', 'Supply', 'Offering', 'Resource', 'Key', 'Evidence']
const ICONS = [
  { value: 'SkillWillpower', label: 'Willpower', icon: 'willpower-icon' },
  { value: 'SkillIntellect', label: 'Intellect', icon: 'intellect-icon' },
  { value: 'SkillCombat', label: 'Combat', icon: 'combat-icon' },
  { value: 'SkillAgility', label: 'Agility', icon: 'agility-icon' },
  { value: 'Wild', label: 'Wild', icon: 'wild-icon' },
]
const KEYWORDS = [
  'Alert', 'Aloof', 'Elusive', 'Fast', 'Hunter', 'Massive', 'Myriad',
  'Peril', 'Relentless', 'Retaliate', 'Surge', 'Doomed', 'Permanent', 'Predator',
]

const PLAYER_TYPES = ['AssetType', 'EventType', 'SkillType', 'PlayerTreacheryType', 'PlayerEnemyType']

const form = reactive({
  title: '',
  subtitle: '',
  cardType: 'EnemyType' as string,
  classSymbol: 'Neutral',
  cost: '' as string,
  level: '' as string,
  unique: false,
  victory: '' as string,
  traits: '',
  icons: [] as string[],
  keywords: [] as string[],
  // enemy — blank means the card prints a dash for that stat
  fight: '3',
  health: '3',
  healthPerPlayer: false,
  evade: '3',
  damage: '1',
  horror: '0',
  // location
  shroud: '2',
  clues: '1',
  cluesPerPlayer: true,
  // asset
  slots: [] as string[],
  assetHealth: '',
  assetSanity: '',
  useType: '',
  useCount: '',
  // escape hatch
  rawJson: '',
})

const tab = ref<'new' | 'library'>('new')
const selected = ref<string | null>(null)
const artUrl = ref('')
const artData = ref<string | null>(null)
const dragging = ref(false)
const error = ref<string | null>(null)
const busy = ref(false)

const isEnemy = computed(() => form.cardType === 'EnemyType' || form.cardType === 'PlayerEnemyType')
const isLocation = computed(() => form.cardType === 'LocationType')
const isAsset = computed(() => form.cardType === 'AssetType' || form.cardType === 'EncounterAssetType')
const isPlayerCard = computed(() => PLAYER_TYPES.includes(form.cardType))
const hasCost = computed(() => ['AssetType', 'EventType'].includes(form.cardType))

const art = computed(() => artData.value || artUrl.value.trim() || null)

// ---------------------------------------------------------------- traits ---

/* Traits are typed the way they are printed -- "Monster. Elite. Ancient One." --
 * and matched against what the engine actually knows, since a trait only does
 * anything if it is the same value the matchers use. Anything unrecognised
 * becomes a custom trait rather than being dropped. */
const traitIndex = ref(new Map<string, string>())

const normalizeTrait = (trait: string) => trait.toLowerCase().replace(/[^a-z0-9]/g, '')

const pascalCase = (trait: string) =>
  trait
    .trim()
    .split(/\s+/)
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join('')

const parsedTraits = computed(() =>
  form.traits
    .split('.')
    .map((trait) => trait.trim())
    .filter(Boolean)
    .map((raw) => {
      const known = traitIndex.value.get(normalizeTrait(raw))
      return { raw, name: known ?? pascalCase(raw), custom: !known }
    }),
)

onMounted(async () => {
  try {
    const index = new Map<string, string>()
    for (const [name, display] of await Api.fetchTraits()) {
      index.set(normalizeTrait(name), name)
      index.set(normalizeTrait(display), name)
    }
    traitIndex.value = index
  } catch (e) {
    console.error(e)
  }
})

// ------------------------------------------------------------ skill icons ---

const iconCount = (icon: string) => form.icons.filter((i) => i === icon).length

function addIcon(icon: string) {
  form.icons.push(icon)
}

function removeIcon(icon: string) {
  const index = form.icons.lastIndexOf(icon)
  if (index !== -1) form.icons.splice(index, 1)
}

// -------------------------------------------------------------------- def ---

const num = (v: string): number | null => {
  const n = parseInt(v, 10)
  return Number.isFinite(n) ? n : null
}

const gameValue = (v: string, perPlayer: boolean) => {
  const n = num(v)
  if (n === null) return null
  return perPlayer ? { tag: 'PerPlayer', contents: n } : { tag: 'Static', contents: n }
}

const setIf = (def: Record<string, any>, key: string, value: unknown) => {
  if (value !== null && value !== undefined) def[key] = value
}

const iconJson = (icon: string) =>
  icon === 'Wild' ? { tag: 'WildIcon', contents: [] } : { tag: 'SkillIcon', contents: icon }

/* The def the engine will run. Only meaningful keys are emitted -- CardDef's
 * parser defaults everything else, so an absent key is the printed blank, which
 * is how an enemy ends up with a dash for fight, health or evade. */
const buildDef = (cardCode: string): Record<string, any> => {
  const def: Record<string, any> = {
    cardCode,
    art: cardCode,
    cardType: form.cardType,
    name: { title: form.title.trim() || 'Custom Card', subtitle: form.subtitle.trim() || null },
    classSymbols: [form.classSymbol],
    cardTraits: parsedTraits.value.map((t) => t.name),
    skills: form.icons.map(iconJson),
    keywords: form.keywords.map((k) => ({ tag: k, contents: [] })),
    unique: form.unique,
    doubleSided: false,
    meta: {} as Record<string, any>,
  }

  if (form.cardType === 'PlayerTreacheryType' || form.cardType === 'PlayerEnemyType') {
    def.cardSubType = { tag: 'Weakness', contents: [] }
  }

  if (hasCost.value) setIf(def, 'cost', num(form.cost) === null ? null : { tag: 'StaticCost', contents: num(form.cost) })
  setIf(def, 'level', num(form.level))
  setIf(def, 'victoryPoints', num(form.victory))

  if (isEnemy.value) {
    setIf(def, 'fight', gameValue(form.fight, false))
    setIf(def, 'health', gameValue(form.health, form.healthPerPlayer))
    setIf(def, 'evade', gameValue(form.evade, false))
    setIf(def, 'healthDamage', gameValue(form.damage, false))
    setIf(def, 'sanityDamage', gameValue(form.horror, false))
  }

  if (isLocation.value) {
    def.meta.shroud = num(form.shroud) ?? 0
    def.meta.revealClues = gameValue(form.clues, form.cluesPerPlayer) ?? { tag: 'Static', contents: 0 }
  }

  if (isAsset.value) {
    if (form.slots.length) def.slots = form.slots
    setIf(def.meta, 'health', num(form.assetHealth))
    setIf(def.meta, 'sanity', num(form.assetSanity))
    if (form.useType && num(form.useCount) !== null) {
      def.uses = { type: form.useType, amount: num(form.useCount) }
    }
  }

  return def
}

/* The raw block wins over the form, so a field the form does not offer can
 * still be set (and one it does can be overridden). */
const mergeRaw = (def: Record<string, any>): Record<string, any> => {
  const raw = form.rawJson.trim()
  if (!raw) return def
  const parsed = JSON.parse(raw)
  return { ...def, ...parsed, meta: { ...def.meta, ...(parsed.meta ?? {}) } }
}

const previewDef = computed(() => {
  try {
    return mergeRaw(buildDef('*preview'))
  } catch {
    return buildDef('*preview')
  }
})

const previewArt = computed(() => art.value ?? renderCardPlaceholder(previewDef.value as any))

// ------------------------------------------------------------------- art ---

/* Keep the stored image small: it lives in the game state and is served to
 * every player, so a full-resolution scan has no business going in. */
async function readImage(file: File) {
  const dataUrl: string = await new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = () => resolve(reader.result as string)
    reader.onerror = () => reject(reader.error)
    reader.readAsDataURL(file)
  })

  const image = new Image()
  await new Promise((resolve, reject) => {
    image.onload = resolve
    image.onerror = reject
    image.src = dataUrl
  })

  const maxWidth = 500
  const scale = Math.min(1, maxWidth / image.width)
  const canvas = document.createElement('canvas')
  canvas.width = Math.round(image.width * scale)
  canvas.height = Math.round(image.height * scale)
  canvas.getContext('2d')?.drawImage(image, 0, 0, canvas.width, canvas.height)
  artData.value = canvas.toDataURL('image/webp', 0.85)
  artUrl.value = ''
}

async function takeImage(file: File | undefined) {
  if (!file || !file.type.startsWith('image/')) return
  error.value = null
  try {
    await readImage(file)
  } catch (e) {
    console.error(e)
    error.value = 'Could not read that image.'
  }
}

async function onDrop(event: DragEvent) {
  dragging.value = false
  await takeImage(event.dataTransfer?.files?.[0])
}

async function onFile(event: Event) {
  await takeImage((event.target as HTMLInputElement).files?.[0])
}

function clearArt() {
  artData.value = null
  artUrl.value = ''
}

// --------------------------------------------------------------- library ---

/* One list from two sources: the cards this campaign already knows about (the
 * game holds them, so they outlive each scenario and every player sees them)
 * and the ones made in this browser, which carry over into new campaigns. */
const library = computed(() => {
  const inCampaign = customCards()
  const campaignCodes = new Set(inCampaign.map((c) => c.def.cardCode))
  const local = libraryCards().filter((c) => !campaignCodes.has(c.def.cardCode))
  return [...inCampaign, ...local].map((card) => ({
    card,
    inCampaign: campaignCodes.has(card.def.cardCode),
  }))
})

const selectedCard = computed(() => library.value.find((e) => e.card.def.cardCode === selected.value)?.card)

const cardArt = (card: CustomCard) => card.art ?? renderCardPlaceholder(card.def)

/* Forgetting a card the campaign owns takes it off the game too, so it stops
 * coming back. Copies already in play keep working for this server run but lose
 * their def on the next reload, which is the trade for being able to clear out
 * a mistake. */
async function forget(cardCode: string, inCampaign: boolean) {
  if (inCampaign) {
    if (!confirm('Remove this card from the campaign? Copies already in play will break on reload.')) return
    await debug.send(props.game.id, { tag: 'DebugRemoveCustomCard', contents: cardCode })
    unregisterCustomCard(cardCode)
    cardStore.cards = cardStore.cards.filter((c) => c.cardCode !== cardCode)
  }

  removeFromLibrary(cardCode)
  if (selected.value === cardCode) selected.value = null
}

// ---------------------------------------------------------------- submit ---

/* Whichever card the action bar would act on: the one being built, or the one
 * picked out of the library. */
const activeCardType = computed(() =>
  tab.value === 'library' ? selectedCard.value?.def.cardType : form.cardType,
)

// Only player cards survive deck loading, so only they can be earned.
const canAddToCampaignDeck = computed(
  () => !!activeCardType.value && PLAYER_TYPES.includes(activeCardType.value),
)

const canSubmit = computed(() => tab.value === 'new' || !!selectedCard.value)

/* Registration is by card code and idempotent, so re-adding a card the campaign
 * already has just mints another copy of it rather than a lookalike. */
async function addCard(customCard: CustomCard, placement: Placement) {
  const cardCode = customCard.def.cardCode

  await debug.send(props.game.id, { tag: 'DebugRegisterCustomCard', contents: customCard })
  registerCustomCards([customCard])
  if (!cardStore.cards.some((c) => c.cardCode === cardCode)) {
    cardStore.cards = [...cardStore.cards, customCard.def]
  }

  const cardId = crypto.randomUUID()
  await debug.send(props.game.id, { tag: 'CreateCard', contents: [cardId, cardCode] })

  const message = {
    play: { tag: 'DebugPlaceCard', contents: [props.investigatorId, cardId] },
    hand: { tag: 'DebugAddToHand', contents: [props.investigatorId, cardId] },
    campaignDeck: { tag: 'DebugAddToCampaignDeck', contents: [props.investigatorId, cardId] },
    encounterDeck: { tag: 'DebugAddToEncounterDeck', contents: [{ tag: 'EncounterDeck' }, cardId] },
  }[placement]

  await debug.send(props.game.id, message)
}

async function submit(placement: Placement) {
  error.value = null
  busy.value = true

  try {
    if (tab.value === 'library') {
      const card = selectedCard.value
      if (!card) return
      await addCard(card, placement)
    } else {
      const customCard: CustomCard = {
        def: mergeRaw(buildDef(mintCustomCardCode())) as any,
        art: art.value,
      }
      await addCard(customCard, placement)
      const { saved, reason } = saveToLibrary(customCard)
      if (!saved && reason) {
        error.value = `${reason} The card was still added to the game.`
        return
      }
    }

    emit('close')
  } catch (e) {
    console.error(e)
    error.value = 'Could not create the card. Check the raw JSON, if you used any.'
  } finally {
    busy.value = false
  }
}

function toggle(list: string[], value: string) {
  const index = list.indexOf(value)
  if (index === -1) list.push(value)
  else list.splice(index, 1)
}
</script>

<template>
  <div class="custom-card-overlay" @click.self="emit('close')">
    <div class="custom-card-modal">
      <div class="custom-card-tabs">
        <button type="button" :class="{ on: tab === 'new' }" @click="tab = 'new'">New card</button>
        <button type="button" :class="{ on: tab === 'library' }" @click="tab = 'library'">
          Library <span v-if="library.length" class="count">{{ library.length }}</span>
        </button>
      </div>

      <div v-if="tab === 'library'" class="custom-card-library">
        <p v-if="!library.length" class="custom-card-status">
          No custom cards yet. Make one on the New card tab and it will be waiting here.
        </p>
        <div v-else class="library-grid">
          <div
            v-for="entry in library"
            :key="entry.card.def.cardCode"
            class="library-card"
            :class="{ on: selected === entry.card.def.cardCode }"
            @click="selected = entry.card.def.cardCode"
          >
            <img :src="cardArt(entry.card)" :data-image-id="entry.card.def.cardCode" alt="" />
            <span class="library-name">{{ entry.card.def.name.title }}</span>
            <small>{{ entry.card.def.cardType.replace(/Type$/, '') }}</small>
            <button
              type="button"
              class="library-forget"
              :title="entry.inCampaign ? 'Remove from this campaign' : 'Remove from your card library'"
              @click.stop="forget(entry.card.def.cardCode, entry.inCampaign)"
            >
              ×
            </button>
          </div>
        </div>
      </div>

      <div v-show="tab === 'new'" class="custom-card-body">
        <div class="custom-card-preview">
          <div
            class="art-dropzone"
            :class="{ dragging }"
            @dragover.prevent="dragging = true"
            @dragleave="dragging = false"
            @drop.prevent="onDrop"
          >
            <img :src="previewArt" alt="" />
            <span class="art-hint">Drop an image</span>
          </div>
          <label class="file-pick">
            Choose an image
            <input type="file" accept="image/*" @change="onFile" />
          </label>
          <label>
            …or an image URL
            <input v-model="artUrl" type="url" placeholder="https://…" @keydown.stop />
          </label>
          <button v-if="art" type="button" class="link" @click="clearArt">Clear art</button>
        </div>

        <div class="custom-card-form">
          <div class="row">
            <label>
              Title
              <input v-model="form.title" type="text" autofocus @keydown.stop />
            </label>
            <label>
              Subtitle
              <input v-model="form.subtitle" type="text" @keydown.stop />
            </label>
          </div>

          <div class="row">
            <label>
              Type
              <select v-model="form.cardType">
                <option v-for="t in CARD_TYPES" :key="t.value" :value="t.value">{{ t.label }}</option>
              </select>
            </label>
            <label>
              Class
              <select v-model="form.classSymbol">
                <option v-for="c in CLASSES" :key="c" :value="c">{{ c }}</option>
              </select>
            </label>
          </div>

          <div class="row">
            <label v-if="hasCost">
              Cost
              <input v-model="form.cost" type="number" @keydown.stop />
            </label>
            <label v-if="isPlayerCard">
              Level
              <input v-model="form.level" type="number" @keydown.stop />
            </label>
            <label>
              Victory
              <input v-model="form.victory" type="number" @keydown.stop />
            </label>
            <label class="checkbox">
              <input v-model="form.unique" type="checkbox" />
              Unique
            </label>
          </div>

          <label>
            Traits
            <input v-model="form.traits" type="text" placeholder="Monster. Elite. Ancient One." @keydown.stop />
          </label>
          <p v-if="parsedTraits.length" class="trait-preview">
            <span
              v-for="trait in parsedTraits"
              :key="trait.name"
              class="trait"
              :class="{ custom: trait.custom }"
              :title="trait.custom ? 'Not a known trait — added as a custom trait' : trait.name"
            >{{ trait.raw }}.</span>
          </p>

          <fieldset>
            <legend>Skill icons</legend>
            <div class="icon-steppers">
              <div v-for="icon in ICONS" :key="icon.value" class="icon-stepper" :title="icon.label">
                <button type="button" :disabled="!iconCount(icon.value)" @click="removeIcon(icon.value)">−</button>
                <span :class="icon.icon" />
                <span class="icon-count">{{ iconCount(icon.value) }}</span>
                <button type="button" @click="addIcon(icon.value)">+</button>
              </div>
            </div>
          </fieldset>

          <fieldset>
            <legend>Keywords</legend>
            <div class="chips">
              <button
                v-for="keyword in KEYWORDS"
                :key="keyword"
                type="button"
                class="chip"
                :class="{ on: form.keywords.includes(keyword) }"
                @click="toggle(form.keywords, keyword)"
              >
                {{ keyword }}
              </button>
            </div>
          </fieldset>

          <fieldset v-if="isEnemy">
            <legend>Enemy — leave a stat blank for a dash</legend>
            <div class="row">
              <label>Fight<input v-model="form.fight" type="number" placeholder="—" @keydown.stop /></label>
              <label>Health<input v-model="form.health" type="number" placeholder="—" @keydown.stop /></label>
              <label>Evade<input v-model="form.evade" type="number" placeholder="—" @keydown.stop /></label>
            </div>
            <div class="row">
              <label>Damage<input v-model="form.damage" type="number" placeholder="—" @keydown.stop /></label>
              <label>Horror<input v-model="form.horror" type="number" placeholder="—" @keydown.stop /></label>
              <label class="checkbox">
                <input v-model="form.healthPerPlayer" type="checkbox" />
                Health per investigator
              </label>
            </div>
          </fieldset>

          <fieldset v-if="isLocation">
            <legend>Location</legend>
            <div class="row">
              <label>Shroud<input v-model="form.shroud" type="number" @keydown.stop /></label>
              <label>Clues<input v-model="form.clues" type="number" @keydown.stop /></label>
              <label class="checkbox">
                <input v-model="form.cluesPerPlayer" type="checkbox" />
                Clues per investigator
              </label>
            </div>
          </fieldset>

          <fieldset v-if="isAsset">
            <legend>Asset</legend>
            <div class="chips">
              <button
                v-for="slot in SLOTS"
                :key="slot"
                type="button"
                class="chip"
                :class="{ on: form.slots.includes(slot) }"
                @click="toggle(form.slots, slot)"
              >
                {{ slot.replace('Slot', '') }}
              </button>
            </div>
            <div class="row">
              <label>Health<input v-model="form.assetHealth" type="number" placeholder="—" @keydown.stop /></label>
              <label>Sanity<input v-model="form.assetSanity" type="number" placeholder="—" @keydown.stop /></label>
            </div>
            <div class="row">
              <label>
                Uses
                <select v-model="form.useType">
                  <option value="">None</option>
                  <option v-for="u in USE_TYPES" :key="u" :value="u">{{ u }}</option>
                </select>
              </label>
              <label v-if="form.useType">
                Amount
                <input v-model="form.useCount" type="number" @keydown.stop />
              </label>
            </div>
          </fieldset>

          <details>
            <summary>Raw CardDef JSON (merged over the form)</summary>
            <textarea
              v-model="form.rawJson"
              rows="6"
              spellcheck="false"
              placeholder='{ "keywords": [{ "tag": "Swarming", "contents": { "tag": "Static", "contents": 2 } }] }'
              @keydown.stop
            />
          </details>
        </div>
      </div>

      <p v-if="error" class="custom-card-error">{{ error }}</p>

      <div class="custom-card-actions">
        <button type="button" :disabled="busy || !canSubmit" @click="submit('play')">Put into play</button>
        <button type="button" :disabled="busy || !canSubmit" @click="submit('hand')">Add to hand</button>
        <button
          v-if="canAddToCampaignDeck"
          type="button"
          :disabled="busy || !canSubmit"
          title="Shuffles into the deck now and records it in the campaign's story cards, so it comes back in later scenarios"
          @click="submit('campaignDeck')"
        >
          Add to deck for campaign
        </button>
        <button type="button" :disabled="busy || !canSubmit" @click="submit('encounterDeck')">
          Shuffle into encounter deck
        </button>
        <button type="button" class="secondary" @click="emit('close')">{{ $t('close') }}</button>
      </div>
    </div>
  </div>
</template>

<style scoped lang="scss">
.custom-card-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.7);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: var(--z-index-1000);
}

.custom-card-modal {
  background: #1a1a2e;
  border: 1px solid var(--button-highlight);
  border-radius: 8px;
  color: #eee;
  padding: 1.25rem 1.5rem 1.5rem;
  width: min(900px, 94vw);
  max-height: 92vh;
  overflow: auto;
}

.custom-card-tabs {
  display: flex;
  gap: 0.25rem;
  border-bottom: 1px solid #374151;
  margin-bottom: 1rem;

  button {
    background: none;
    border: none;
    border-bottom: 2px solid transparent;
    color: #9ca3af;
    cursor: pointer;
    font-size: 0.95rem;
    padding: 0.5rem 0.9rem;

    &.on {
      border-bottom-color: var(--button-highlight);
      color: #adf;
    }
  }

  .count {
    background: rgba(255, 255, 255, 0.12);
    border-radius: 999px;
    font-size: 0.75rem;
    margin-left: 0.25rem;
    padding: 0.05rem 0.4rem;
  }
}

.custom-card-body {
  display: flex;
  gap: 1.25rem;
  align-items: flex-start;

  @media (max-width: 700px) {
    flex-direction: column;
  }
}

.custom-card-preview {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  flex: 0 0 200px;
}

.art-dropzone {
  position: relative;
  border-radius: 8px;
  cursor: copy;

  img {
    width: 200px;
    border-radius: 8px;
    background: #111827;
    display: block;
  }

  .art-hint {
    position: absolute;
    inset: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    background: rgba(17, 24, 39, 0.85);
    border: 2px dashed var(--button-highlight);
    border-radius: 8px;
    opacity: 0;
    pointer-events: none;
    transition: opacity 0.12s ease;
  }

  &:hover .art-hint {
    opacity: 0.6;
  }

  &.dragging .art-hint {
    opacity: 1;
  }
}

.file-pick {
  font-size: 0.8rem;
  opacity: 0.85;
}

.custom-card-form {
  flex: 1 1 auto;
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
  min-width: 0;
}

.row {
  display: flex;
  gap: 0.6rem;
  flex-wrap: wrap;

  > label {
    flex: 1 1 90px;
  }
}

label {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
  font-size: 0.85rem;

  &.checkbox {
    flex-direction: row;
    align-items: center;
    gap: 0.35rem;
    align-self: flex-end;
    padding-bottom: 0.5rem;
  }
}

input,
select,
textarea {
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  padding: 0.4rem;
  font-family: inherit;
  width: 100%;
}

input[type='checkbox'] {
  width: auto;
}

.trait-preview {
  margin: 0;
  font-style: italic;
  font-size: 0.85rem;

  .trait {
    margin-right: 0.3rem;
  }

  .custom {
    color: #fc9;
    border-bottom: 1px dotted #fc9;
  }
}

fieldset {
  border: 1px solid #374151;
  border-radius: 6px;
  padding: 0.6rem;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;

  legend {
    font-size: 0.8rem;
    opacity: 0.8;
    padding: 0 0.35rem;
  }
}

.icon-steppers {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
}

.icon-stepper {
  align-items: center;
  background: rgba(255, 255, 255, 0.06);
  border-radius: 6px;
  display: flex;
  gap: 0.3rem;
  padding: 0.2rem 0.35rem;

  button {
    background: none;
    border: none;
    color: #eee;
    cursor: pointer;
    font-size: 1rem;
    line-height: 1;
    padding: 0.1rem 0.3rem;

    &:disabled {
      opacity: 0.3;
      cursor: default;
    }
  }

  .icon-count {
    font-variant-numeric: tabular-nums;
    min-width: 0.9em;
    text-align: center;
  }
}

.chips {
  display: flex;
  flex-wrap: wrap;
  gap: 0.3rem;
}

.chip {
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid transparent;
  border-radius: 999px;
  color: #eee;
  cursor: pointer;
  font-size: 0.78rem;
  padding: 0.25rem 0.6rem;

  &:hover {
    background: rgba(255, 255, 255, 0.12);
  }

  &.on {
    background: var(--button-highlight);
    border-color: var(--button-highlight);
    color: #10131f;
  }
}

.custom-card-library {
  min-height: 200px;
}

.library-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
  gap: 0.75rem;
  max-height: 60vh;
  overflow: auto;
  padding: 0.25rem;
}

.library-card {
  position: relative;
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
  cursor: pointer;
  border: 2px solid transparent;
  border-radius: 8px;
  padding: 0.25rem;

  img {
    width: 100%;
    border-radius: 6px;
    background: #111827;
  }

  small {
    opacity: 0.65;
  }

  .library-name {
    font-size: 0.85rem;
  }

  &:hover {
    background: rgba(255, 255, 255, 0.06);
  }

  &.on {
    border-color: var(--button-highlight);
  }
}

.library-forget {
  position: absolute;
  top: 0.35rem;
  right: 0.35rem;
  background: rgba(0, 0, 0, 0.65);
  border: none;
  border-radius: 50%;
  color: #eee;
  cursor: pointer;
  font-size: 0.9rem;
  height: 1.4rem;
  line-height: 1;
  opacity: 0;
  width: 1.4rem;

  .library-card:hover & {
    opacity: 1;
  }
}

details summary {
  cursor: pointer;
  font-size: 0.85rem;
  opacity: 0.85;
}

.custom-card-actions {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  margin-top: 1rem;

  button {
    background: rgba(255, 255, 255, 0.08);
    border: 1px solid var(--button-highlight);
    border-radius: 4px;
    color: #eee;
    cursor: pointer;
    padding: 0.5rem 0.8rem;

    &:disabled {
      opacity: 0.5;
      cursor: default;
    }

    &.secondary {
      border-color: #4b5563;
      margin-left: auto;
    }
  }
}

.link {
  background: none;
  border: none;
  color: #adf;
  cursor: pointer;
  font-size: 0.8rem;
  padding: 0;
  text-align: left;
}

.custom-card-status {
  opacity: 0.8;
}

.custom-card-error {
  color: #f88;
}
</style>
