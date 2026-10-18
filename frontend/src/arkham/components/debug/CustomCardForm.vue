<script lang="ts" setup>
/* The card builder's form: everything that makes up a custom card's def.
 *
 * It owns its own state and exposes `loadCard`, `reset` and `buildCustomCard`,
 * so the page can drive it for both new cards and edits without threading the
 * whole form through props. */
import { computed, onMounted, onUnmounted, reactive, ref } from 'vue'
import * as Api from '@/arkham/api'
import {
  ARKHAM_BUILD_ID_KEY,
  CARD_TYPES,
  PLAYER_CARD_TYPES,
  cardArtReference,
  isArkhamBuildCardId,
  renderCardPlaceholder,
  stripCardCodePrefix,
  type CustomCard,
} from '@/arkham/customCards'
import { cardImg, imgsrc } from '@/arkham/helpers'
import { readImage } from '@/arkham/customCardArt'
import { libraryCards } from '@/arkham/customCardLibrary'
import AbilityEditor from '@/arkham/components/debug/AbilityEditor.vue'
import StepsEditor from '@/arkham/components/debug/StepsEditor.vue'
import { cardBindings, defBindings, rememberedBindings } from '@/arkham/customCardBindings'
import BoolField from '@/arkham/components/debug/BoolField.vue'
import CardCodeField from '@/arkham/components/debug/CardCodeField.vue'
import ValueEditor from '@/arkham/components/debug/ValueEditor.vue'
import { loadSchema } from '@/arkham/schema'
import { provide } from 'vue'
import {
  CARD_TEXT,
  LOCALE_META_KEY,
  PRIMARY_LANGUAGE,
  TEXT_LANGUAGES,
  textKeysUsed,
  type CardTextAccess,
} from '@/arkham/customText'
import KeywordEffectEditor from '@/arkham/components/debug/KeywordEffectEditor.vue'
import type { CustomSetKeyword, CustomSetToken } from '@/arkham/api'

/* What the set this card belongs to declares. A card prints one of its set's
 * keywords, so the list of them has to come from outside the card: the set is
 * what holds them, and this form only ever sees one card. */
const props = withDefaults(
  defineProps<{
    setKeywords?: CustomSetKeyword[]
    setTokens?: CustomSetToken[]
  }>(),
  { setKeywords: () => [], setTokens: () => [] },
)

/* The matcher kinds a step can query, shared with the ability editor. */
const QUERY_KINDS: Record<string, string> = {
  enemy: 'EnemyMatcher',
  location: 'LocationMatcher',
  investigator: 'InvestigatorMatcher',
  asset: 'AssetMatcher',
  treachery: 'TreacheryMatcher',
  event: 'EventMatcher',
  skill: 'SkillMatcher',
  story: 'StoryMatcher',
  act: 'ActMatcher',
  agenda: 'AgendaMatcher',
  card: 'ExtendedCardMatcher',
}

// The types that are a weakness by their nature, rather than by choice.
const ALWAYS_WEAKNESS = ['PlayerTreacheryType', 'PlayerEnemyType']

/* Mythos belongs to encounter cards, which do not choose a class at all: they
 * take the default, overridable through the raw block. */
// The actions a card can be played as; an event is the usual case.
const ACTIONS = ['Fight', 'Evade', 'Investigate', 'Move', 'Parley', 'Resign', 'Draw', 'Play', 'Activate']

/* The form writes a bare array, but the server stores the tagged Actions it
 * decoded, so a card read back is never the shape it was written as. */
const actionList = (v: any): string[] =>
  Array.isArray(v)
    ? v.flatMap(actionList)
    : typeof v === 'string'
      ? [v]
      : v && typeof v === 'object'
        ? actionList(v.contents)
        : []

const CLASSES = ['Guardian', 'Seeker', 'Rogue', 'Mystic', 'Survivor', 'Neutral']
const SLOTS = ['HandSlot', 'BodySlot', 'AllySlot', 'AccessorySlot', 'ArcaneSlot', 'TarotSlot', 'HeadSlot']
const USE_TYPES = ['Ammo', 'Charge', 'Secret', 'Supply', 'Offering', 'Resource', 'Key', 'Evidence']
/* An investigator records stats rather than icons: the same control, without
 * wild, since there is no such stat. */
const STATS = [
  { key: 'willpower', label: 'Willpower', icon: 'willpower-icon' },
  { key: 'intellect', label: 'Intellect', icon: 'intellect-icon' },
  { key: 'combat', label: 'Combat', icon: 'combat-icon' },
  { key: 'agility', label: 'Agility', icon: 'agility-icon' },
] as const

const ICONS = [
  { value: 'SkillWillpower', label: 'Willpower', icon: 'willpower-icon' },
  { value: 'SkillIntellect', label: 'Intellect', icon: 'intellect-icon' },
  { value: 'SkillCombat', label: 'Combat', icon: 'combat-icon' },
  { value: 'SkillAgility', label: 'Agility', icon: 'agility-icon' },
  { value: 'Wild', label: 'Wild', icon: 'wild-icon' },
]
const KEYWORDS = [
  'Alert', 'Aloof', 'Doomed', 'Elusive', 'Fast', 'Hidden', 'Hunter', 'Massive',
  'Myriad', 'Peril', 'Permanent', 'Predator', 'Relentless', 'Retaliate', 'Surge',
]
const ENEMY_TYPES = ['EnemyType', 'PlayerEnemyType']
const TREACHERY_TYPES = ['TreacheryType', 'PlayerTreacheryType']
/* Which card types can print a keyword, for the keywords only some can. The rest
 * are open to any card. Fighting, evading, engaging and moving are things an
 * enemy does, so the keywords about them are an enemy's alone; hidden is a
 * revelation that puts the card in your hand, which only an enemy or a treachery
 * has a placement for. */
const KEYWORD_TYPES: Record<string, string[]> = {
  Alert: ENEMY_TYPES,
  Aloof: ENEMY_TYPES,
  Doomed: ENEMY_TYPES,
  Elusive: ENEMY_TYPES,
  Hunter: ENEMY_TYPES,
  Massive: ENEMY_TYPES,
  Predator: ENEMY_TYPES,
  Relentless: ENEMY_TYPES,
  Retaliate: ENEMY_TYPES,
  Hidden: [...ENEMY_TYPES, ...TREACHERY_TYPES],
}
const canPrintKeyword = (keyword: string, cardType: string) =>
  !KEYWORD_TYPES[keyword] || KEYWORD_TYPES[keyword].includes(cardType)


/* The text this card says, reachable from any field in the editor. Writing one
 * here is the same edit as writing it in the Text box below -- both are the
 * card's own `_locale`, so a key filled in beside the label it belongs to
 * appears there too. */
/* Which language the Text box is written in. The previews beside each label
 * follow it, so switching here is how the card is read back in French. */
const textLanguage = ref(PRIMARY_LANGUAGE)

provide(CARD_TEXT, {
  read: (key: string) => form.text[textLanguage.value]?.[key] ?? '',
  write: (key: string, value: string) => {
    const language = (form.text[textLanguage.value] ??= {})
    language[key] = value
  },
} satisfies CardTextAccess)

/* Every key the card names, and any left over from an earlier wording. A key
 * nothing names any more is still shown -- with its words, which are the part
 * worth keeping -- rather than quietly dropped while a label still reads it. */
/* Which of its own keys the card names, read off the form rather than off a
 * built def: the def is where the text ends up, so building one to decide what
 * text to put in it would be a circle. */
const textUsed = computed(() => {
  const written = (() => {
    try {
      return form.rawJson.trim() ? JSON.parse(form.rawJson) : null
    } catch {
      return null
    }
  })()
  return textKeysUsed({
    meta: {
      _onPlay: form.onPlaySteps,
      _abilities: form.abilities,
      _handlers: form.handlers,
      _modifiers: form.modifiers,
      _keywords: form.keywordEffects,
      _onRevelation: form.revelationSteps,
      _elderSignSteps: form.elderSignSteps,
      _elderSignRevealSteps: form.elderSignRevealSteps,
      _elderSignSuccessSteps: form.elderSignSuccessSteps,
      _elderSignFailureSteps: form.elderSignFailureSteps,
    },
    raw: written,
  })
})

/* The keys the card names, and only those. A label rewritten or taken away
 * leaves its words behind, which would otherwise sit in the Text box forever
 * as something nothing can show. They stay in the form while it is open, so
 * putting the label back puts the words back, and go on the next save. */
const textKeys = computed(() => textUsed.value)

const textFor = (key: string) => form.text[textLanguage.value]?.[key] ?? ''

const writeText = (key: string, value: string) => {
  const language = (form.text[textLanguage.value] ??= {})
  language[key] = value
}

/* Which languages this card already says something in, marked in the picker so
 * a translation that exists is not written twice. Counted the way saving counts
 * it: a key left blank is one nobody has written yet. */
const written = (language: string) =>
  Object.values(form.text[language] ?? {}).some((value) => typeof value === 'string' && value.trim())

const blankForm = () => ({
  title: '',
  subtitle: '',
  cardType: '' as string,
  classSymbol: 'Neutral',
  cost: '' as string,
  level: '' as string,
  unique: false,
  permanent: false,
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
  // abilities
  abilities: [] as any[],
  handlers: [] as any[],
  modifiers: [] as any[],
  // Keywords this card prints, out of the ones its set declares.
  keywordEffects: [] as any[],
  weaknessKind: '',
  // revelation
  revelation: false,
  revelationPlacement: '',
  revelationSteps: [] as any[],
  actions: [] as string[],
  /* What this card's own labels say, by language. The form edits the primary
   * one; the rest ride along untouched so a translation is never dropped by
   * someone editing in English. */
  text: {} as Record<string, Record<string, string>>,
  // Actions are all required unless the card says to pick one of them.
  actionsOr: false,
  // grouping, so a set of cards made together can be found together
  cardNumber: '',
  arkhamBuildId: '',
  // investigator
  elderSign: '1',
  elderSignRevealSteps: [] as any[],
  elderSignSteps: [] as any[],
  elderSignSuccessSteps: [] as any[],
  elderSignFailureSteps: [] as any[],
  prey: null as any,
  spawnAt: null as any,
  onPlaySteps: [] as any[],
  willpower: '3',
  intellect: '3',
  combat: '3',
  agility: '3',
  investigatorHealth: '7',
  investigatorSanity: '7',
  signatures: [] as string[],
  // art, one entry per slot
  artUrls: {} as Record<string, string>,
  artUploaded: {} as Record<string, string | null>,
  // escape hatch
  rawJson: '',
  /* `cdAdditionalCost`: what a card makes you do beyond paying its resource
   * cost, checked and taken as part of playing it. */
  additionalCost: null as any,
  /* `cdBondedWith`: how many of which card come with this one, kept out of the
   * deck until something searches the bonded cards for them. */
  bonded: [] as { count: string; cardCode: string }[],
  /* `cdDeckRestrictions`: what a deck must be for this card to go in it --
   * "limit 1 per deck", a signature's owner, and so on. */
  deckRestrictions: [] as any[],
})

const form = reactive(blankForm())
const dragging = ref<string | null>(null)
const uploading = ref<string | null>(null)
const error = ref<string | null>(null)
/* Display the processed local blob immediately after a drop. In development,
 * Vite can briefly return/cache a 404 for a newly written public image; using
 * the uploaded URL directly then leaves a broken image until a refresh. */
const artPreviews = reactive<Record<string, string>>({})

function clearArtPreviews() {
  for (const url of Object.values(artPreviews)) URL.revokeObjectURL(url)
  for (const slot of Object.keys(artPreviews)) delete artPreviews[slot]
}

onUnmounted(clearArtPreviews)

/* An investigator carries four images; everything else just its face. The extra
 * ones ride in meta so the card model stays one def plus one piece of art. */

type ArtSlot = { key: string; label: string; shape: 'card' | 'sideways' | 'portrait' }

const ART_SLOTS = computed<ArtSlot[]>(() =>
  form.cardType === 'InvestigatorType'
    ? [
        { key: 'art', label: 'Card front', shape: 'sideways' },
        { key: 'backArt', label: 'Card back', shape: 'sideways' },
        { key: 'portrait', label: 'Portrait', shape: 'portrait' },
        { key: 'portraitBack', label: 'Portrait back', shape: 'portrait' },
      ]
    : [{ key: 'art', label: 'Card art', shape: 'card' }],
)

/* An investigator is unique, so start it that way rather than making everyone
 * remember to tick it. A player treachery or enemy is only ever a weakness, so
 * it starts as one; an asset or event has to be told. */
function chooseType(cardType: string) {
  form.cardType = cardType
  form.unique = cardType === 'InvestigatorType'
  form.weaknessKind = ALWAYS_WEAKNESS.includes(cardType) ? 'Weakness' : ''
  // Drop what the new type cannot print rather than keeping it where nothing shows it.
  form.keywords = form.keywords.filter((k) => canPrintKeyword(k, cardType))
  if (cardType === 'InvestigatorType') {
    form.keywords = []
    form.keywordEffects = []
  }
}

const artFor = (slot: string) => form.artUploaded[slot] || form.artUrls[slot]?.trim() || null

const isEnemy = computed(() => ENEMY_TYPES.includes(form.cardType))
const isTreachery = computed(() => TREACHERY_TYPES.includes(form.cardType))
/* Only cards you can commit to a test print skill icons; an investigator has
 * stats instead, and enemies and treacheries have none at all. */
const hasSkillIcons = computed(
  () => !isInvestigator.value && !isEnemy.value && !isTreachery.value,
)
const isLocation = computed(() => form.cardType === 'LocationType')
const isAsset = computed(() => form.cardType === 'AssetType' || form.cardType === 'EncounterAssetType')
const isEvent = computed(() => form.cardType === 'EventType')
const isPlayerCard = computed(() => PLAYER_CARD_TYPES.includes(form.cardType))
// A weakness is never bought and never paid for.
const hasCost = computed(() => ['AssetType', 'EventType'].includes(form.cardType) && !isWeakness.value)

const art = computed(() => artFor('art'))
const isInvestigator = computed(() => form.cardType === 'InvestigatorType')

/* Every keyword this card could print: the engine's own, and whatever its set
 * declares. They read the same way on the card -- a word in bold above the text
 * -- so they are chosen the same way, from one row of chips.
 *
 * Where they differ is underneath. An engine keyword is a flag on the def; a
 * set's is an entry in `_keywords` that may have slots to fill, and a block for
 * those appears below once it is on. */
const keywordChoices = computed(() =>
  // An investigator prints no keywords at all -- not the engine's, and not its
  // own set's: a keyword is printed above the text of a card that has text.
  isInvestigator.value
    ? []
    : [
        ...KEYWORDS.filter((k) => canPrintKeyword(k, form.cardType)).map((name) => ({
          name,
          fromSet: false,
        })),
        ...props.setKeywords.map((keyword) => ({ name: keyword.name, fromSet: true })),
      ],
)

const setKeywordNames = computed(() => props.setKeywords.map((k) => k.name))

const hasKeyword = (keyword: { name: string; fromSet: boolean }) =>
  keyword.fromSet
    ? form.keywordEffects.some((e: any) => e.keyword === keyword.name)
    : form.keywords.includes(keyword.name)

/* The keywords on this card that ask it for something. A keyword with no slots
 * and no result of its own is said by the chip alone, and a block holding only
 * its own name would be noise. */
const keywordsNeedingInput = computed(() =>
  (isInvestigator.value ? [] : form.keywordEffects).filter((entry: any) => {
    const declared = props.setKeywords.find((k) => k.name === entry.keyword)
    // One this set no longer declares still gets a block: it is on the card, and
    // hiding it would leave no way to take it off.
    if (!declared) return true
    return (declared.slots ?? []).length > 0 || !!declared.behavior?.activeWhen
  }),
)
/* Read the same way the engine reads it: on a card type that cannot be hidden the
 * keyword is printed text and changes nothing. */
const isHidden = computed(
  () => canPrintKeyword('Hidden', form.cardType) && form.keywords.includes('Hidden'),
)

const isWeakness = computed(() => !!form.weaknessKind)
/* An asset or an event is a weakness only if it says so; a player treachery or
 * enemy has no other reason to exist. */
const canBeWeakness = computed(() =>
  ['AssetType', 'EventType', ...ALWAYS_WEAKNESS].includes(form.cardType),
)
const weaknessOptional = computed(() => !ALWAYS_WEAKNESS.includes(form.cardType))

// ------------------------------------------------------------ revelation ---

/* The cards that can resolve as they are drawn. A skill or a location never
 * does, and an investigator is not drawn at all. */
const canHaveRevelation = computed(
  () => isAsset.value || isEvent.value || isTreachery.value || isEnemy.value,
)

/* A treachery always resolves when it is drawn, and so does a weakness asset or
 * event — without a revelation it would simply sit in your hand. A weakness
 * enemy is spawned by being drawn and needs no revelation to do it, so that one
 * is asked for — unless it is hidden, which is a revelation by definition. */
const revelationImplied = computed(
  () => isTreachery.value || isHidden.value || (isWeakness.value && (isAsset.value || isEvent.value)),
)

const hasRevelation = computed(() => revelationImplied.value || form.revelation)

/* Only what stays on the table has anywhere to be put -- and a hidden card is
 * already answered: it goes secretly into the drawing investigator's hand. */
const hasRevelationPlacement = computed(
  () => !isHidden.value && (isAsset.value || isTreachery.value),
)

const REVELATION_PLACEMENTS = computed(() =>
  isTreachery.value
    ? [
        { value: 'none', label: 'Discarded once it resolves' },
        { value: 'threatArea', label: 'Your threat area' },
      ]
    : [
        { value: 'none', label: 'Nowhere — the steps place it' },
        { value: 'threatArea', label: 'Your threat area' },
        { value: 'playArea', label: 'Your play area' },
      ],
)

/* Where a revelation card ends up if you never say. A weakness asset has to go
 * somewhere — one that placed itself nowhere is stranded in play — and the
 * printed wording is almost always the threat area. A treachery left alone is
 * discarded once it resolves, which is what the engine does anyway. */
const defaultPlacement = computed(() =>
  isAsset.value && isWeakness.value ? 'threatArea' : 'none',
)

const revelationPlacement = computed({
  get: () => form.revelationPlacement || defaultPlacement.value,
  set: (value: string) => {
    form.revelationPlacement = value
  },
})

/* Level is what you pay xp for, so it belongs to cards you buy: never an
 * enemy, a treachery, or a weakness. */
const hasLevel = computed(
  () => isPlayerCard.value && !isEnemy.value && !isTreachery.value && !isWeakness.value,
)

// A class is a player-card idea, and a weakness has no class of its own.
const hasClass = computed(
  () => (isPlayerCard.value || isInvestigator.value) && !isWeakness.value,
)

/* Only a card a deck can name needs to claim its arkham.build twin, and an
 * investigator is one -- a deck names its investigator too, which is the case
 * this was asked for. */
const canClaimArkhamBuildId = computed(() => isPlayerCard.value || isInvestigator.value)

const badArkhamBuildId = computed(
  () => !!form.arkhamBuildId.trim() && !isArkhamBuildCardId(form.arkhamBuildId.trim()),
)

/* Signature cards are other cards in your library. Held on the investigator by
 * card code, which is what the deck overlay needs to bring them along. */
const signatureChoices = computed(() =>
  libraryCards().filter((c) => c.def.cardType !== 'InvestigatorType'),
)

/* The other direction, read only: an investigator names its signatures, so a
 * card learns whose it is by being listed there. Shown here because it is a
 * real deck restriction — only they can take it — and because it binds their id
 * as $investigator for this card's own abilities. */
/* Names this card has written into its own metaMap with a Set Global, which
 * the engine binds back for everything else the card does. Read off the card
 * rather than declared, so the binding exists exactly where the write does. */
const remembered = computed(() =>
  rememberedBindings([form.onPlaySteps, form.abilities, form.handlers, form.modifiers]),
)

const loadedCode = ref<string | null>(null)

const signatureOwner = computed(() => {
  const mine = loadedCode.value
  if (!mine) return undefined
  return libraryCards().find(
    (c) =>
      c.def.cardType === 'InvestigatorType' &&
      ((c.def.meta?._signatures ?? []) as string[]).some(
        (code) => stripCardCodePrefix(code) === stripCardCodePrefix(mine),
      ),
  )
})

const addingSignature = ref(false)


/* A library card carries the code the server sent, which `ToJSON CardCode`
 * prefixes with a `c`; `_signatures` holds the bare code. Comparing the two
 * as-is never matches, which showed the chips as raw ids instead of names, so
 * every comparison here goes through the bare form -- as `signatureOwner`
 * above already did. */
const sameCard = (a: string, b: string) =>
  stripCardCodePrefix(a) === stripCardCodePrefix(b)

const signatureCard = (cardCode: string) =>
  signatureChoices.value.find((c) => sameCard(c.def.cardCode, cardCode))

const isSignature = (cardCode: string) =>
  form.signatures.some((code) => sameCard(code, cardCode))

function addSignature(cardCode: string) {
  // Stored bare, which is the form the engine reads out of meta.
  if (cardCode && !isSignature(cardCode)) form.signatures.push(stripCardCodePrefix(cardCode))
  addingSignature.value = false
}

const removeSignature = (cardCode: string) =>
  form.signatures.splice(
    form.signatures.findIndex((code) => sameCard(code, cardCode)),
    1,
  )

// ---------------------------------------------------------------- traits ---

/* Traits are typed the way they are printed -- "Monster. Elite. Ancient One." --
 * and matched against what the engine actually knows, since a trait only does
 * anything if it is the same value the matchers use. Anything unrecognised
 * becomes a custom trait rather than being dropped. */
const traitIndex = ref(new Map<string, string>())
const traitDisplay = ref(new Map<string, string>())

const normalizeTrait = (trait: string) => trait.toLowerCase().replace(/[^a-z0-9]/g, '')

const pascalCase = (trait: string) =>
  trait.trim().split(/\s+/).map((word) => word.charAt(0).toUpperCase() + word.slice(1)).join('')

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

async function loadTraits() {
  if (traitIndex.value.size) return
  try {
    const index = new Map<string, string>()
    const display = new Map<string, string>()
    for (const [name, printed] of await Api.fetchTraits()) {
      index.set(normalizeTrait(name), name)
      index.set(normalizeTrait(printed), name)
      display.set(name, printed)
    }
    traitIndex.value = index
    traitDisplay.value = display
  } catch (e) {
    console.error(e)
  }
}

onMounted(() => {
  loadTraits()
  // Prey and spawn are built against the schema, same as an ability's fields.
  loadSchema()
})

// ----------------------------------------------------------- skill icons ---

const statValue = (key: string) => parseInt((form as any)[key], 10) || 0

function stepStat(key: string, delta: number) {
  ;(form as any)[key] = String(Math.max(0, statValue(key) + delta))
}

const iconCount = (icon: string) => form.icons.filter((i) => i === icon).length
const addIcon = (icon: string) => form.icons.push(icon)

function removeIcon(icon: string) {
  const index = form.icons.lastIndexOf(icon)
  if (index !== -1) form.icons.splice(index, 1)
}

// ------------------------------------------------------------------- def ---

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

/* Only meaningful keys are emitted -- CardDef's parser defaults everything else,
 * so an absent key is the printed blank, which is how an enemy ends up with a
 * dash for fight, health or evade. */
function buildDef(cardCode: string): Record<string, any> {
  const def: Record<string, any> = {
    cardCode,
    art: cardCode,
    cardType: form.cardType,
    name: { title: form.title.trim() || 'Custom Card', subtitle: form.subtitle.trim() || null },
    cardTraits: parsedTraits.value.map((t) => t.name),
    skills: hasSkillIcons.value ? form.icons.map(iconJson) : [],
    keywords: isInvestigator.value ? [] : form.keywords.map((k) => ({ tag: k, contents: [] })),
    unique: form.unique,
    permanent: form.permanent,
    doubleSided: false,
    meta: {} as Record<string, any>,
  }

  if (hasClass.value) def.classSymbols = [form.classSymbol]
  if (form.actions.length) {
    // A bare array decodes as AndActions; OrActions has to stay tagged.
    def.actions =
      form.actionsOr && form.actions.length > 1
        ? { tag: 'OrActions', contents: form.actions.map((a) => ({ tag: 'SingleAction', contents: a })) }
        : form.actions
  }

  if (isWeakness.value) def.cardSubType = form.weaknessKind

  if (hasCost.value) setIf(def, 'cost', num(form.cost) === null ? null : { tag: 'StaticCost', contents: num(form.cost) })
  if (hasLevel.value) setIf(def, 'level', num(form.level))
  setIf(def, 'victoryPoints', num(form.victory))

  if (isEnemy.value) {
    setIf(def, 'fight', gameValue(form.fight, false))
    setIf(def, 'health', gameValue(form.health, form.healthPerPlayer))
    setIf(def, 'evade', gameValue(form.evade, false))
    setIf(def, 'healthDamage', gameValue(form.damage, false))
    setIf(def, 'sanityDamage', gameValue(form.horror, false))
    // Prey and spawn are attrs the runner reads back out of meta.
    if (form.prey) def.meta.prey = form.prey
    if (form.spawnAt) def.meta.spawnAt = form.spawnAt
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

  if (form.cardNumber.trim()) def.meta.number = form.cardNumber.trim()
  /* Written for a player card only, and only when it is an id: a deck names a
     card by one, so a typo here is a card that answers to nothing. */
  const buildId = form.arkhamBuildId.trim()
  if (buildId && canClaimArkhamBuildId.value && isArkhamBuildCardId(buildId)) {
    def.meta[ARKHAM_BUILD_ID_KEY] = buildId
  }

  if (isInvestigator.value) {
    if (num(form.elderSign) !== null) def.meta._elderSign = num(form.elderSign)
    if (form.elderSignRevealSteps.length)
      def.meta._elderSignRevealSteps = form.elderSignRevealSteps
    if (form.elderSignSteps.length) def.meta._elderSignSteps = form.elderSignSteps
    if (form.elderSignSuccessSteps.length) def.meta._elderSignSuccessSteps = form.elderSignSuccessSteps
    if (form.elderSignFailureSteps.length) def.meta._elderSignFailureSteps = form.elderSignFailureSteps
    def.meta.health = num(form.investigatorHealth) ?? 0
    def.meta.sanity = num(form.investigatorSanity) ?? 0
    def.meta.willpower = num(form.willpower) ?? 0
    def.meta.intellect = num(form.intellect) ?? 0
    def.meta.combat = num(form.combat) ?? 0
    def.meta.agility = num(form.agility) ?? 0
    // The cards this investigator brings with them, by card code.
    if (form.signatures.length) def.meta._signatures = form.signatures
  }

  // The face is the card's art; the rest ride in meta.
  for (const slot of ART_SLOTS.value) {
    if (slot.key === 'art') continue
    const value = artFor(slot.key)
    if (value) def.meta[slot.key] = value
  }

  if (hasRevelation.value) {
    def.revelation = 'IsRevelation'
    if (hasRevelationPlacement.value) def.meta._revelationPlacement = revelationPlacement.value
    if (form.revelationSteps.length) def.meta._onRevelation = form.revelationSteps
  }

  // Gated the same way the fieldset is, so switching a half-filled card over to
  // an investigator does not leave these behind where nothing can see them.
  if (!isInvestigator.value) {
    if (form.additionalCost) def.additionalCost = form.additionalCost
    if (form.deckRestrictions.length) def.deckRestrictions = form.deckRestrictions
    // Stored as [count, cardCode] pairs, which is how cdBondedWith decodes.
    const bonded = form.bonded
      .filter((b) => b.cardCode.trim())
      .map((b) => [num(b.count) ?? 1, stripCardCodePrefix(b.cardCode.trim())])
    if (bonded.length) def.bondedWith = bonded
  }

  /* Empty keys are dropped rather than saved blank: one with no words is a key
     nobody has written yet, and the editor offers it again from the labels. */
  const text: Record<string, Record<string, string>> = {}
  const named = textUsed.value
  for (const [language, entries] of Object.entries(form.text)) {
    const written = Object.fromEntries(
      Object.entries(entries ?? {})
        // A key no label names any more is not this card's text.
        .filter(([key]) => named.includes(key))
        .filter(([, value]) => typeof value === 'string' && value.trim()),
    )
    if (Object.keys(written).length) text[language] = written
  }
  if (Object.keys(text).length) def.meta[LOCALE_META_KEY] = text

  if (form.onPlaySteps.length) def.meta._onPlay = form.onPlaySteps
  if (form.abilities.length) def.meta._abilities = form.abilities
  if (form.handlers.length) def.meta._handlers = form.handlers
  if (form.modifiers.length) def.meta._modifiers = form.modifiers
  if (form.keywordEffects.length && !isInvestigator.value) def.meta._keywords = form.keywordEffects

  return def
}

/* Whatever the form does not model, kept as it was read and merged back over
 * what the form builds. No longer typed in here -- the raw editor in the head
 * opens the whole def -- but still carried, so a key this editor has never
 * heard of survives a save. */
function mergeRaw(def: Record<string, any>): Record<string, any> {
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


// ------------------------------------------------------------------- art ---

async function takeImage(slot: string, file: File | undefined) {
  if (!file || !file.type.startsWith('image/')) return
  error.value = null
  uploading.value = slot
  let preview: string | null = null
  const previousPreview = artPreviews[slot]
  try {
    const image = await readImage(file)
    preview = URL.createObjectURL(image)
    artPreviews[slot] = preview
    form.artUploaded[slot] = await Api.uploadCustomCardArt(image)
    form.artUrls[slot] = ''
    if (previousPreview) URL.revokeObjectURL(previousPreview)
  } catch (e: any) {
    if (preview) URL.revokeObjectURL(preview)
    if (previousPreview) artPreviews[slot] = previousPreview
    else delete artPreviews[slot]
    console.error(e)
    error.value = e?.response?.data?.message ?? e?.message ?? 'Could not upload that image.'
  } finally {
    uploading.value = null
  }
}

async function onDrop(slot: string, event: DragEvent) {
  dragging.value = null
  await takeImage(slot, event.dataTransfer?.files?.[0])
}

async function onFile(slot: string, event: Event) {
  await takeImage(slot, (event.target as HTMLInputElement).files?.[0])
}

function clearArt(slot: string) {
  if (artPreviews[slot]) URL.revokeObjectURL(artPreviews[slot])
  delete artPreviews[slot]
  form.artUploaded[slot] = null
  form.artUrls[slot] = ''
}

/* The face is the card; the other slots fall back to the placeholder drawn from
 * the def so an empty slot still reads as what it is. A slot that names a
 * printed card is shown as that card's image, so what you get is what you see. */
const slotPreview = (slot: string) => {
  const value = artPreviews[slot] || artFor(slot)
  if (!value) return slot === 'art' ? renderCardPlaceholder(previewDef.value as any) : null
  const reference = cardArtReference(value)
  if (!reference) return value
  return slot.startsWith('portrait') ? imgsrc(`portraits/${reference}.jpg`) : cardImg(reference)
}

// ------------------------------------------------------------- load/save ---

/* Fields the form owns. Anything else on the def is put back into the raw block
 * so editing a card cannot quietly drop what the form cannot express. */
const FORM_KEYS = [
  'cardCode', 'art', 'cardType', 'name', 'classSymbols', 'cardTraits', 'skills', 'keywords',
  'unique', 'permanent', 'doubleSided', 'meta', 'cardSubType', 'cost', 'level', 'victoryPoints', 'actions',
  'revelation',
  'fight', 'health', 'evade', 'healthDamage', 'sanityDamage', 'slots', 'uses',
  'additionalCost', 'bondedWith', 'deckRestrictions',
]
const FORM_META_KEYS = [
  'shroud', 'revealClues', 'health', 'sanity', 'willpower', 'intellect', 'combat', 'agility',
  'backArt', 'portrait', 'portraitBack', 'number', 'set', 'prey', 'spawnAt',
  /* Stamped on by the set, not the form's to edit -- but listed here so the form
     does not put them in the raw block either, which would freeze a stale copy
     onto the card that the set could no longer correct. */
  'setTokens', 'setKeywords',
  // The card's own text, edited in its own fieldset.
  '_locale',
  '_abilities', '_handlers', '_modifiers', '_keywords', '_signatures', '_onRevelation', '_revelationPlacement',
  '_elderSign', '_elderSignRevealSteps', '_elderSignSteps', '_elderSignSuccessSteps',
  '_elderSignFailureSteps', '_onPlay',
]

const gameValueNumber = (v: any) => (v && typeof v.contents === 'number' ? String(v.contents) : '')
const isPerPlayer = (v: any) => v?.tag === 'PerPlayer'

async function loadCard(card: CustomCard) {
  await loadTraits()
  clearArtPreviews()
  const def: Record<string, any> = card.def as any
  const meta = def.meta ?? {}

  Object.assign(form, blankForm())
  loadedCode.value = def.cardCode ?? null
  form.title = def.name?.title ?? ''
  form.subtitle = def.name?.subtitle ?? ''
  form.cardType = def.cardType
  form.classSymbol = def.classSymbols?.[0] ?? 'Neutral'
  form.cost = def.cost?.tag === 'StaticCost' ? String(def.cost.contents) : ''
  form.level = def.level === null || def.level === undefined ? '' : String(def.level)
  form.victory = def.victoryPoints === null || def.victoryPoints === undefined ? '' : String(def.victoryPoints)
  form.unique = !!def.unique
  form.permanent = !!def.permanent
  form.weaknessKind = typeof def.cardSubType === 'string' ? def.cardSubType : ''
  form.revelation = !!def.revelation && def.revelation !== 'NoRevelation'
  form.revelationPlacement = meta._revelationPlacement ?? ''
  form.revelationSteps = meta._onRevelation ?? []
  form.actions = actionList(def.actions)
  form.text =
    meta[LOCALE_META_KEY] && typeof meta[LOCALE_META_KEY] === 'object'
      ? JSON.parse(JSON.stringify(meta[LOCALE_META_KEY]))
      : {}
  form.actionsOr = def.actions?.tag === 'OrActions'
  form.traits = (def.cardTraits ?? []).map((t: string) => traitDisplay.value.get(t) ?? t).join('. ')
  form.icons = (def.skills ?? []).map((s: any) => (s.tag === 'SkillIcon' ? s.contents : 'Wild'))
  form.keywords = (def.keywords ?? []).map((k: any) => k.tag).filter((k: string) => KEYWORDS.includes(k))
  // A card already carrying one half of Permanent gets the other, rather than
  // loading as a form that disagrees with itself.
  if (form.permanent || form.keywords.includes('Permanent')) setPermanent(true)

  form.fight = gameValueNumber(def.fight)
  form.health = gameValueNumber(def.health)
  form.healthPerPlayer = isPerPlayer(def.health)
  form.evade = gameValueNumber(def.evade)
  form.damage = gameValueNumber(def.healthDamage)
  form.horror = gameValueNumber(def.sanityDamage)

  form.shroud = meta.shroud === undefined ? '' : String(meta.shroud)
  form.clues = gameValueNumber(meta.revealClues)
  form.cluesPerPlayer = isPerPlayer(meta.revealClues)

  form.slots = def.slots ?? []
  form.assetHealth = meta.health === undefined ? '' : String(meta.health)
  form.assetSanity = meta.sanity === undefined ? '' : String(meta.sanity)
  form.useType = def.uses?.type ?? ''
  form.useCount = def.uses?.amount === undefined ? '' : String(def.uses.amount)

  form.willpower = meta.willpower === undefined ? '3' : String(meta.willpower)
  form.intellect = meta.intellect === undefined ? '3' : String(meta.intellect)
  form.combat = meta.combat === undefined ? '3' : String(meta.combat)
  form.agility = meta.agility === undefined ? '3' : String(meta.agility)
  form.investigatorHealth = meta.health === undefined ? '7' : String(meta.health)
  form.investigatorSanity = meta.sanity === undefined ? '7' : String(meta.sanity)
  form.signatures = (meta._signatures ?? []).map(stripCardCodePrefix)
  form.cardNumber = meta.number ?? ''
  form.arkhamBuildId = meta[ARKHAM_BUILD_ID_KEY] ?? ''
  // Blank when the card has none, so no Elder sign tab is offered for it.
  form.elderSign = meta._elderSign === undefined ? '' : String(meta._elderSign)
  form.elderSignRevealSteps = meta._elderSignRevealSteps ?? []
  form.elderSignSteps = meta._elderSignSteps ?? []
  form.elderSignSuccessSteps = meta._elderSignSuccessSteps ?? []
  form.elderSignFailureSteps = meta._elderSignFailureSteps ?? []

  form.artUploaded = { art: card.art }
  form.artUrls = {}
  for (const slot of ['backArt', 'portrait', 'portraitBack']) {
    if (meta[slot]) form.artUploaded[slot] = meta[slot]
  }

  form.prey = meta.prey ?? null
  form.spawnAt = meta.spawnAt ?? null
  form.onPlaySteps = meta._onPlay ?? []
  form.abilities = meta._abilities ?? []
  form.handlers = meta._handlers ?? []
  form.modifiers = meta._modifiers ?? []
  form.keywordEffects = meta._keywords ?? []
  form.additionalCost = def.additionalCost ?? null
  form.deckRestrictions = def.deckRestrictions ?? []
  form.bonded = (def.bondedWith ?? []).map((b: any) => ({
    count: String(b?.[0] ?? 1),
    cardCode: b?.[1] ?? '',
  }))

  const leftover: Record<string, any> = {}
  for (const [key, value] of Object.entries(def)) {
    if (!FORM_KEYS.includes(key)) leftover[key] = value
  }
  const leftoverMeta: Record<string, any> = {}
  for (const [key, value] of Object.entries(meta)) {
    if (!FORM_META_KEYS.includes(key)) leftoverMeta[key] = value
  }
  if (Object.keys(leftoverMeta).length) leftover.meta = leftoverMeta
  form.rawJson = Object.keys(leftover).length ? JSON.stringify(leftover, null, 2) : ''

}

function reset() {
  clearArtPreviews()
  Object.assign(form, blankForm())
  loadedCode.value = null
  error.value = null
}

function buildCustomCard(cardCode: string): CustomCard {
  return { def: mergeRaw(buildDef(cardCode)) as any, art: art.value }
}

/* Permanent is the same fact said twice: the `permanent` flag on the def, which is
 * what the engine reads everywhere (`cdPermanent`), and the `Permanent` keyword,
 * which is what the card prints -- nothing reads the keyword. A card carrying one
 * without the other says two different things, so either control sets both. */
function setPermanent(on: boolean) {
  form.permanent = on
  const has = form.keywords.includes('Permanent')
  if (on && !has) form.keywords.push('Permanent')
  if (!on && has) form.keywords = form.keywords.filter((k) => k !== 'Permanent')
}

function toggleKeyword(keyword: { name: string; fromSet: boolean }) {
  if (keyword.fromSet) return toggleSetKeyword(keyword.name)
  if (keyword.name === 'Permanent') return setPermanent(!form.keywords.includes(keyword.name))
  toggle(form.keywords, keyword.name)
}

/* A set keyword is on when the card has an entry for it. Turning it off drops
 * the entry and whatever was filled in; nothing else holds that, so there is no
 * half-on state to keep. */
function toggleSetKeyword(name: string) {
  const index = form.keywordEffects.findIndex((e: any) => e.keyword === name)
  if (index === -1) form.keywordEffects = [...form.keywordEffects, { keyword: name }]
  else form.keywordEffects = form.keywordEffects.filter((_: any, i: number) => i !== index)
}

/* How many of this slot the card takes. A list rather than a set, because
 * "Hand x2" is the hand slot twice -- there is no two-handed slot type. */
const slotCount = (slot: string) => form.slots.filter((s) => s === slot).length

function toggleSlot(slot: string) {
  form.slots = slotCount(slot) > 0 ? form.slots.filter((s) => s !== slot) : [...form.slots, slot]
}

/* The second copy, added and removed on its own so turning the slot off does not
 * have to remember whether it was doubled. */
function toggleDoubleSlot(slot: string) {
  const rest = form.slots.filter((s) => s !== slot)
  const next = slotCount(slot) > 1 ? 1 : 2
  form.slots = [...rest, ...Array(next).fill(slot)]
}

function toggle(list: string[], value: string) {
  const index = list.indexOf(value)
  if (index === -1) list.push(value)
  else list.splice(index, 1)
}

/* What is in the editor right now, as a string to compare against.

The fields themselves rather than the built def: 'buildCustomCard' validates and
can throw on half-typed raw JSON, which is exactly the moment the caller most
needs to know the card is unsaved. Reading `form` and `art` here is also what
makes a caller's computed re-run as they type. */
function snapshot(): string {
  return JSON.stringify({ form, art: art.value })
}

defineExpose({
  loadCard,
  reset,
  buildCustomCard,
  snapshot,
  cardType: computed(() => form.cardType),
  // What the head says it is editing, as it is typed rather than as it was saved.
  cardTitle: computed(() => form.title),
})
</script>
<template>
  <div class="custom-card-body">
    <div class="custom-card-preview">
      <div v-for="slot in ART_SLOTS" :key="slot.key" class="art-slot">
        <span v-if="ART_SLOTS.length > 1" class="slot-label">{{ slot.label }}</span>
        <div
          class="art-dropzone"
          :class="[slot.shape, { dragging: dragging === slot.key, uploading: uploading === slot.key }]"
          @dragover.prevent="dragging = slot.key"
          @dragleave="dragging = null"
          @drop.prevent="onDrop(slot.key, $event)"
        >
          <img v-if="slotPreview(slot.key)" :src="slotPreview(slot.key)!" alt="" />
          <div v-else class="art-empty"></div>
          <span class="art-hint">{{ uploading === slot.key ? 'Uploading…' : 'Drop an image' }}</span>
        </div>
        <label class="file-pick">
          Choose an image
          <input type="file" accept="image/*" @change="onFile(slot.key, $event)" />
        </label>
        <label>
          …or a URL, or a card code to reuse that card's art
          <input
            v-model="form.artUrls[slot.key]"
            type="text"
            placeholder="https://… or 01004"
            @keydown.stop
          />
        </label>
        <button v-if="artFor(slot.key)" type="button" class="link" @click="clearArt(slot.key)">Clear</button>
      </div>
      <p v-if="error" class="custom-card-error">{{ error }}</p>
    </div>

        <div class="custom-card-form">
          <div v-if="!form.cardType" class="type-picker">
            <p class="type-prompt">What kind of card is this?</p>
            <div class="type-grid">
              <button
                v-for="t in CARD_TYPES"
                :key="t.value"
                type="button"
                @click="chooseType(t.value)"
              >
                {{ t.label }}
              </button>
            </div>
          </div>

          <template v-else>
          <div class="chosen-type">
            <strong>{{ CARD_TYPES.find((t) => t.value === form.cardType)?.label }}</strong>
            <button type="button" class="link" @click="form.cardType = ''">Change type</button>
          </div>

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
            <label v-if="canBeWeakness">
              Weakness
              <select v-model="form.weaknessKind">
                <option v-if="weaknessOptional" value="">Not a weakness</option>
                <option value="Weakness">Weakness</option>
                <option value="BasicWeakness">Basic weakness</option>
              </select>
            </label>
            <label v-if="hasClass">
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
            <label v-if="hasLevel">
              Level
              <input v-model="form.level" type="number" @keydown.stop />
            </label>
            <label v-if="!isInvestigator">
              Victory
              <input v-model="form.victory" type="number" @keydown.stop />
            </label>
            <BoolField
              label="Unique"
              v-model="form.unique"
            />
            <BoolField
              v-if="isPlayerCard"
              label="Permanent"
              :modelValue="form.permanent"
              @update:modelValue="setPermanent"
            />
          </div>

          <div v-if="!isInvestigator && signatureOwner" class="row">
            <label>
              Signature of
              <router-link
                class="owner-pill"
                :to="{ name: 'CardBuilder', query: { card: signatureOwner.def.cardCode } }"
              >
                {{ signatureOwner.def.name.title }}
              </router-link>
            </label>
          </div>
          <p v-if="!isInvestigator && signatureOwner" class="hint">
            Only they can take it, and their id is bound as <code>$investigator</code> for this
            card's abilities. The link lives on their signatures, so add or remove it there.
          </p>

          <div class="row">
            <label>
              Card number
              <input v-model="form.cardNumber" type="text" placeholder="1" @keydown.stop />
            </label>
            <!-- The note is about the id, so it stays in the id's column rather
                 than running under the card number as well. -->
            <div v-if="canClaimArkhamBuildId" class="with-note">
              <label>
                arkham.build card id
                <input
                  v-model="form.arkhamBuildId"
                  type="text"
                  placeholder="6afb2ea0-d5c6-434a-aea9-2b1beff8c74a"
                  spellcheck="false"
                  @keydown.stop
                />
              </label>
              <p class="hint" :class="{ bad: badArkhamBuildId }">
                <template v-if="badArkhamBuildId">
                  Not an arkham.build id — a uuid, or the 32- or 8-character form a pack's cards
                  come through with. Left out until it is one.
                </template>
                <template v-else>
                  Optional. If this same card is published on arkham.build, paste its id and a deck
                  built there will find this copy instead of failing as an unimplemented card.
                </template>
              </p>
            </div>
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

          <fieldset v-if="hasSkillIcons">
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

          <fieldset v-if="keywordChoices.length">
            <legend>Keywords</legend>
            <div class="chips">
              <button
                v-for="keyword in keywordChoices"
                :key="keyword.name"
                type="button"
                class="chip"
                :class="{ on: hasKeyword(keyword), 'from-set': keyword.fromSet }"
                :title="keyword.fromSet ? 'Declared by this card set' : undefined"
                @click="toggleKeyword(keyword)"
              >
                {{ keyword.name }}
              </button>
            </div>
            <p v-if="isHidden" class="hint">
              Drawing this secretly puts it into your hand instead of resolving it, and only an
              ability used "In your hand" can get it out again — nothing discards a hidden card.
              An enemy's draw is still announced to the table unless it also prints Peril.
            </p>
          </fieldset>

          <!-- Only the keywords that ask this card for something, and only once
               one is on: the chip above says it is printed, this says what it
               says on this card. -->
          <fieldset v-if="keywordsNeedingInput.length">
            <legend>Keyword details</legend>
            <KeywordEffectEditor
              :setKeywords="props.setKeywords"
              :setTokens="props.setTokens"
              :cardType="form.cardType"
              :queryKinds="QUERY_KINDS"
              v-model="form.keywordEffects"
            />
          </fieldset>

          <fieldset v-if="isInvestigator">
            <legend>Investigator</legend>
            <div class="icon-steppers">
              <div v-for="stat in STATS" :key="stat.key" class="icon-stepper" :title="stat.label">
                <button type="button" :disabled="statValue(stat.key) <= 0" @click="stepStat(stat.key, -1)">−</button>
                <span :class="stat.icon" />
                <span class="icon-count">{{ statValue(stat.key) }}</span>
                <button type="button" @click="stepStat(stat.key, 1)">+</button>
              </div>
            </div>
            <div class="row">
              <label>Health<input v-model="form.investigatorHealth" type="number" @keydown.stop /></label>
              <label>Sanity<input v-model="form.investigatorSanity" type="number" @keydown.stop /></label>
            </div>
          </fieldset>

          <fieldset v-if="form.cardType === 'EventType' || form.cardType === 'EncounterEventType'">
            <legend>Actions</legend>
            <div class="chips">
              <button
                v-for="action in ACTIONS"
                :key="action"
                type="button"
                class="chip"
                :class="{ on: form.actions.includes(action) }"
                @click="toggle(form.actions, action)"
              >
                {{ action }}
              </button>
            </div>
            <BoolField
              v-if="form.actions.length > 1"
              label="Any one of them, rather than all of them"
              v-model="form.actionsOr"
            />
            <p class="hint">What happens when it is played:</p>
            <StepsEditor
              :queryKinds="QUERY_KINDS"
              :bindings="[...cardBindings(form.cardType), ...remembered]"
              :path="'onPlay'"
              :modelValue="form.onPlaySteps"
              @update:modelValue="form.onPlaySteps = $event"
            />
          </fieldset>

          <fieldset v-if="isInvestigator">
            <legend>Signature cards</legend>
            <p v-if="!signatureChoices.length" class="hint">
              Build the cards first and they will be listed here to pick from.
            </p>
            <template v-else>
              <div class="chips">
                <span v-for="code in form.signatures" :key="code" class="chip card-chip">
                  {{ signatureCard(code)?.def.name.title ?? code }}
                  <button type="button" class="chip-remove" @click="removeSignature(code)">×</button>
                </span>
                <button type="button" class="chip add-chip" @click="addingSignature = !addingSignature">+</button>
              </div>
              <select
                v-if="addingSignature"
                @change="addSignature(($event.target as HTMLSelectElement).value)"
              >
                <option value="">Choose a card…</option>
                <option
                  v-for="card in signatureChoices.filter((c) => !isSignature(c.def.cardCode))"
                  :key="card.def.cardCode"
                  :value="card.def.cardCode"
                >
                  {{ card.def.name.title }}
                </option>
              </select>
            </template>
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
              <BoolField
                label="Health per investigator"
                v-model="form.healthPerPlayer"
              />
            </div>
            <ValueEditor
              type="PreyMatcher"
              :bindings="defBindings(!!signatureOwner)"
              label="Prey (defaults to anyone)"
              :modelValue="form.prey"
              @update:modelValue="form.prey = $event"
            />
            <ValueEditor
              type="SpawnAt"
              :bindings="defBindings(!!signatureOwner)"
              label="Spawn (defaults to the usual rules)"
              :modelValue="form.spawnAt"
              @update:modelValue="form.spawnAt = $event"
            />
            <p v-if="signatureOwner" class="hint">
              <code>$investigator</code> is their id, so "Prey — them only" is
              <code>OnlyPrey</code> of <code>InvestigatorWithId</code> <code>$investigator</code>.
            </p>
          </fieldset>

          <fieldset v-if="isLocation">
            <legend>Location</legend>
            <div class="row">
              <label>Shroud<input v-model="form.shroud" type="number" @keydown.stop /></label>
              <label>Clues<input v-model="form.clues" type="number" @keydown.stop /></label>
              <BoolField
                label="Clues per investigator"
                v-model="form.cluesPerPlayer"
              />
            </div>
          </fieldset>

          <fieldset v-if="isAsset">
            <legend>Asset</legend>
            <div class="chips">
              <!-- A slot is a list, not a flag: a two-handed weapon takes the
                   hand slot twice. So the pill is in two parts -- the slot, and
                   a second copy of it. -->
              <span
                v-for="slot in SLOTS"
                :key="slot"
                class="chip segmented"
                :class="{ on: slotCount(slot) > 0 }"
              >
                <button type="button" class="seg" @click="toggleSlot(slot)">
                  {{ slot.replace('Slot', '') }}
                </button>
                <button
                  v-if="slotCount(slot) > 0"
                  type="button"
                  class="seg double"
                  :class="{ on: slotCount(slot) > 1 }"
                  :title="slotCount(slot) > 1 ? 'Takes one of these' : 'Takes two of these'"
                  @click="toggleDoubleSlot(slot)"
                >
                  ×2
                </button>
              </span>
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

          <!-- An investigator is never played, so none of this applies to one. -->
          <fieldset v-if="!isInvestigator">
            <legend>Playing it</legend>
            <p class="hint">
              What the card makes you do beyond paying its cost, checked and taken as part of
              playing it — spend an action, add curse tokens, shuffle bonded cards into your deck.
            </p>
            <ValueEditor
              optional
              type="Cost"
              label="Additional cost (optional)"
              :bindings="defBindings(!!signatureOwner)"
              :modelValue="form.additionalCost"
              @update:modelValue="form.additionalCost = $event"
            />

            <ValueEditor
              type="[DeckRestriction]"
              label="Deck restrictions (optional)"
              :bindings="defBindings(!!signatureOwner)"
              :modelValue="form.deckRestrictions"
              @update:modelValue="form.deckRestrictions = $event ?? []"
            />

            <p class="hint">
              Bonded cards start outside the deck and come with this one. A cost that searches
              your bonded cards is what puts them in.
            </p>
            <div v-for="(b, at) in form.bonded" :key="at" class="row">
              <label>
                How many
                <input v-model="b.count" type="number" min="1" @keydown.stop />
              </label>
              <div class="grow">
                <CardCodeField v-model="b.cardCode" placeholder="Which card" />
              </div>
              <button type="button" class="chip-remove" @click="form.bonded.splice(at, 1)">×</button>
            </div>
            <button
              type="button"
              class="add"
              @click="form.bonded.push({ count: '1', cardCode: '' })"
            >
              + Bonded card
            </button>
          </fieldset>

          <fieldset>
            <legend>Abilities</legend>
            <AbilityEditor
              :cardType="form.cardType"
              :extraBindings="remembered"
              :canRevelation="canHaveRevelation"
              :revelationImplied="revelationImplied"
              :hasRevelationPlacement="hasRevelationPlacement"
              :revelationPlacements="REVELATION_PLACEMENTS"
              :isInvestigator="isInvestigator"
              v-model:abilities="form.abilities"
              v-model:handlers="form.handlers"
              v-model:modifiers="form.modifiers"
              v-model:revelation="form.revelation"
              v-model:revelationPlacement="revelationPlacement"
              v-model:revelationSteps="form.revelationSteps"
              v-model:elderSign="form.elderSign"
              v-model:elderSignRevealSteps="form.elderSignRevealSteps"
              v-model:elderSignSteps="form.elderSignSteps"
              v-model:elderSignSuccessSteps="form.elderSignSuccessSteps"
              v-model:elderSignFailureSteps="form.elderSignFailureSteps"
            />
          </fieldset>

          <!-- What the card's own labels say. The keys come from the card:
               every `$.name` it writes is a line here. -->
          <fieldset v-if="textKeys.length" class="card-text">
            <legend>Text</legend>
            <label class="text-language">
              Language
              <select v-model="textLanguage">
                <option v-for="[code, name] in TEXT_LANGUAGES" :key="code" :value="code">
                  {{ name }}{{ written(code) ? ' •' : '' }}
                </option>
              </select>
            </label>
            <label v-for="key in textKeys" :key="key" class="text-line" :class="{ missing: !textFor(key) }">
              <code>$.{{ key }}</code>
              <input
                :value="textFor(key)"
                placeholder="What it says"
                @input="writeText(key, ($event.target as HTMLInputElement).value)"
                @keydown.stop
              />
            </label>
          </fieldset>
          </template>
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
  z-index: var(--z-index-max);
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
    /* The global button radius would curl the active underline up at both ends. */
    border-radius: 0;
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

/* An investigator card lies sideways, and its portrait is squarer than a card. */
.art-dropzone {
  position: relative;
  border-radius: 8px;
  cursor: copy;
  width: 200px;

  img,
  .art-empty {
    aspect-ratio: 5 / 7;
    object-fit: cover;
  }

  &.sideways img,
  &.sideways .art-empty {
    aspect-ratio: 7 / 5;
  }

  &.portrait img,
  &.portrait .art-empty {
    aspect-ratio: 2 / 3;
  }

  img {
    width: 100%;
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

  /* A field that carries its own note: one column of the row, holding the
     field and what is said about it. */
  > .with-note {
    display: flex;
    flex: 1 1 90px;
    flex-direction: column;
    gap: 0.25rem;
    min-width: 0;
  }
}

label {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
  font-size: 0.85rem;
}

// A field component that should take the rest of a row.
.grow {
  flex: 1 1 12rem;
  min-width: 0;
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

/* The marker is drawn rather than left to the browser, so it matches the one the
 * custom pickers show and sits in from the edge instead of flush against it. */
select {
  -webkit-appearance: none;
  appearance: none;
  background: #111827 var(--select-caret) no-repeat right 0.6rem center;
  background-size: var(--select-caret-size);
  padding: 0.4rem 1.6rem 0.4rem 0.5rem;
}


.type-picker {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.type-prompt {
  margin: 0;
  opacity: 0.85;
}

.type-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
  gap: 0.4rem;

  button {
    background: rgba(255, 255, 255, 0.06);
    border: 1px solid #4b5563;
    border-radius: 6px;
    color: #eee;
    cursor: pointer;
    padding: 0.6rem;

    &:hover {
      background: rgba(255, 255, 255, 0.12);
      border-color: var(--button-highlight);
    }
  }
}

.chosen-type {
  align-items: baseline;
  display: flex;
  gap: 0.5rem;
}

.art-slot {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  margin-bottom: 0.75rem;
}

.slot-label {
  font-size: 0.75rem;
  opacity: 0.7;
}

.art-empty {
  background: #111827;
  border-radius: 8px;
  width: 100%;
}

/* A card this one is resolved against, so it wears the same green a known card
   code does in CardCodeField. */
/* A keyword the card's SET declares, among the engine's own. Marked, because
   which it is decides what happens underneath: an engine keyword is a flag, a
   set's may have a block of its own to fill in. */
.chip.from-set {
  border-style: dashed;

  &.on {
    border-style: solid;
  }
}

.owner-pill {
  align-self: flex-start;
  background: rgba(190, 242, 100, 0.12);
  border: 1px solid #bef264;
  border-radius: 999px;
  color: #bef264;
  font-size: 0.8rem;
  padding: 0.2rem 0.7rem;
  text-decoration: none;

  &:hover {
    background: rgba(190, 242, 100, 0.22);
  }
}

.hint {
  font-size: 0.8rem;
  margin: 0;
  opacity: 0.7;

  /* Said while you are still typing the id, so it warns rather than refuses. */
  &.bad {
    color: color-mix(in srgb, var(--important) 85%, white);
    opacity: 1;
  }
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

/* The skill colours the rest of the app uses, so an icon here reads the same as
 * it does on a card. */
.icon-stepper {
  .willpower-icon {
    color: var(--willpower);
  }

  .intellect-icon {
    color: var(--intellect);
  }

  .combat-icon {
    color: var(--combat);
  }

  .agility-icon {
    color: var(--agility);
  }

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

.chip-remove {
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  margin-left: 0.2rem;
  padding: 0;
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

/* A pill in two parts: the thing, and how many of it. The outer pill keeps the
   shape and the on-state so it reads as one chip among the others; the halves
   are the two things that can be clicked. */
.chip.segmented {
  align-items: stretch;
  display: inline-flex;
  overflow: hidden;
  padding: 0;

  .seg {
    background: transparent;
    border: 0;
    color: inherit;
    cursor: pointer;
    font-family: inherit;
    font-size: inherit;
    padding: 0.25rem 0.6rem;

    &:hover {
      background: rgba(255, 255, 255, 0.14);
    }
  }

  /* Off by default even while the slot is on, because one of a slot is the
     common case and a lit ×2 should mean the card really says so. */
  .seg.double {
    border-left: 1px solid rgba(0, 0, 0, 0.25);
    opacity: 0.45;
    padding-inline: 0.45rem;

    &.on {
      font-weight: 700;
      opacity: 1;
    }
  }
}

/* A chip that names an actual card, as against a chip that toggles a trait or a
   keyword. Same green as a known card code in CardCodeField, so "we found this
   card" looks the same wherever it is said. */
.card-chip {
  background: rgba(190, 242, 100, 0.12);
  border-color: #bef264;
  color: #bef264;

  &:hover {
    background: rgba(190, 242, 100, 0.22);
  }
}

/* Opens the picker; it is not a card itself, so it stays the form's plain grey. */
/* The builder's add button, same as the editors' -- scoped styles mean each
   component that has one needs the rule, and this file never had it. */
.add {
  align-self: flex-start;
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  cursor: pointer;
  font-family: inherit;
  font-size: 0.8rem;
  padding: 0.25rem 0.6rem;

  &:hover {
    background: rgba(255, 255, 255, 0.14);
    border-color: #6b7280;
  }
}

.add-chip {
  background: rgba(255, 255, 255, 0.07);
  border-color: #4b5563;
  color: #d1d5db;
  line-height: 1;
  padding: 0.25rem 0.55rem;

  &:hover {
    background: rgba(255, 255, 255, 0.14);
    border-color: #6b7280;
    color: #eee;
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

.library-edit {
  position: absolute;
  top: 0.35rem;
  left: 0.35rem;
  background: rgba(0, 0, 0, 0.65);
  border: none;
  border-radius: 50%;
  color: #eee;
  cursor: pointer;
  font-size: 0.8rem;
  height: 1.4rem;
  line-height: 1;
  opacity: 0;
  width: 1.4rem;

  .library-card:hover & {
    opacity: 1;
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

/* The card's own words. Keyed the same fuchsia as a label that names one, so
   the two halves of the same thing look like it. */
.card-text {
  border: 1px solid #f0abfc44;
  border-radius: 5px;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  padding: 0.4rem 0.6rem;

  legend {
    color: #f0abfc;
    font-size: 0.72rem;
    padding: 0 0.3rem;
  }
}

.text-language {
  align-items: center;
  display: flex;
  flex-direction: row;
  gap: 0.5rem;
  text-align: left;
}

.text-line {
  /* The surrounding form stacks its labels; a key and its words read as one
     line, so this one does not. */
  align-items: center;
  display: flex;
  flex-direction: row;
  gap: 0.5rem;
  text-align: left;

  code {
    color: #f0abfc;
    flex: none;
    font-size: 0.72rem;
    min-width: 9rem;
  }

  input {
    flex: 1;
  }

  &.missing input {
    border-color: #f0abfc66;
  }
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

.editing-banner {
  background: rgba(170, 221, 255, 0.1);
  border-left: 3px solid #adf;
  font-size: 0.85rem;
  margin: 0 0 0.75rem;
  padding: 0.5rem 0.7rem;
}

.custom-card-error {
  color: #f88;
}
</style>
