/* A slot asks four things about one thing. The label the card shows is a
   sentence and gets its own line; the name, the kind and the switch are each a
   word and share the line below. Written as two rows rather than one wrapping
   one, because a wrapping row stretches its items to the tallest of them and
   the name field grew into a box the height of the whole slot. */
.slot-label,
.slot-meta label {
  display: flex;
  flex-direction: column;
  font-size: 0.72rem;
  gap: 0.15rem;
  min-width: 0;
  opacity: 0.85;

  input,
  select {
    font-size: 0.8rem;
    opacity: 1;
  }
}

.slot-label input {
  width: 100%;
}

.slot-meta {
  align-items: flex-end;
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem 0.6rem;

  label {
    flex: 0 1 9rem;
  }

  /* The checkbox reads as one phrase, so it keeps its word beside it. */
  label.inline {
    align-items: center;
    flex: 0 0 auto;
    flex-direction: row;
    gap: 0.3rem;
    opacity: 1;
    padding-bottom: 0.35rem;
  }
}

<script lang="ts" setup>
/* What an investigator expansion declares, beyond being a pile of cards.
 *
 * A set is ordinarily a name, a blurb and a link. Marking it an investigator
 * expansion says it is a box, and a box brings furniture with it: the symbol it
 * is filed under, the tokens its cards place on themselves, and the keywords
 * its cards print. All three are hidden until the box is ticked -- a set of two
 * events should never be asked about an expansion symbol.
 *
 * This edits a draft and hands the whole of it back on every change. The set it
 * belongs to is saved by whoever owns the form, in one request with the name
 * and the blurb, because it is one form.
 */
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import { readImage } from '@/arkham/customCardArt'
import { keywordBindings, keywordStepSlots } from '@/arkham/customCardBindings'
import AbilityEditor from '@/arkham/components/debug/AbilityEditor.vue'
import ValueEditor from '@/arkham/components/debug/ValueEditor.vue'

const { t } = useI18n()
const K = 'customCardSets.'

export type ExpansionDraft = {
  investigatorExpansion: boolean
  icon: string | null
  tokens: Api.CustomSetToken[]
  keywords: Api.CustomSetKeyword[]
}

const props = defineProps<{ modelValue: ExpansionDraft }>()
const emit = defineEmits<{ 'update:modelValue': [ExpansionDraft] }>()

/* One tab per kind of thing an expansion declares.
 *
 * A list is one tab, so a kind added later is one entry here and one block in
 * the template rather than another section stacked on a page that already
 * scrolls. The count rides on the tab, which is what the old sidebar headings
 * were really for. */
const TABS = [
  { key: 'tokens', label: 'setTokens', count: (d: ExpansionDraft) => d.tokens.length },
  { key: 'keywords', label: 'setKeywords', count: (d: ExpansionDraft) => d.keywords.length },
] as const

const tab = ref<(typeof TABS)[number]['key']>('tokens')

const error = ref<string | null>(null)
/* Which image is in flight, so the one control that is busy says so rather than
 * the whole panel going quiet. */
const uploading = ref<string | null>(null)

const draft = computed(() => props.modelValue)

function patch(changes: Partial<ExpansionDraft>) {
  emit('update:modelValue', { ...props.modelValue, ...changes })
}

/* The slug is what the engine stores and what a card's keyword names, so it has
 * to be stable and safe to type: lowercased, with everything that is not a
 * letter or a digit dropped. Mirrors `keywordSlug` on the server. */
const slugify = (name: string) => name.toLowerCase().replace(/[^a-z0-9]/g, '')

// ----------------------------------------------------------------- icon ---

async function takeIcon(file: File | undefined) {
  if (!file || !file.type.startsWith('image/')) return
  error.value = null
  uploading.value = 'icon'
  try {
    patch({ icon: await Api.uploadCustomCardArt(await readImage(file, 128)) })
  } catch (e: any) {
    console.error(e)
    error.value = e?.response?.data?.message ?? e?.message ?? t(`${K}iconUploadFailed`)
  } finally {
    uploading.value = null
  }
}

// --------------------------------------------------------------- tokens ---

function addToken() {
  patch({ tokens: [...draft.value.tokens, { slug: '', name: '', image: null }] })
}

/* Editing a token's name fills the slug in alongside it, up until somebody
 * types a slug of their own. Changing a slug after cards refer to it would
 * leave those cards naming a token that is not there, so an existing one is
 * never rewritten from the name. */
function editToken(index: number, changes: Partial<Api.CustomSetToken>) {
  const tokens = draft.value.tokens.map((token, i) => {
    if (i !== index) return token
    const next = { ...token, ...changes }
    if (changes.name !== undefined && !token.slug) next.slug = slugify(changes.name)
    return next
  })
  patch({ tokens })
}

function removeToken(index: number) {
  patch({ tokens: draft.value.tokens.filter((_, i) => i !== index) })
}

async function takeTokenImage(index: number, file: File | undefined) {
  if (!file || !file.type.startsWith('image/')) return
  error.value = null
  uploading.value = `token-${index}`
  try {
    // A marker is drawn at about 25px, so 96 is already generous.
    editToken(index, { image: await Api.uploadCustomCardArt(await readImage(file, 96)) })
  } catch (e: any) {
    console.error(e)
    error.value = e?.response?.data?.message ?? e?.message ?? t(`${K}iconUploadFailed`)
  } finally {
    uploading.value = null
  }
}

// ------------------------------------------------------------- keywords ---

function addKeyword() {
  patch({
    keywords: [
      ...draft.value.keywords,
      { name: '', description: '', token: null, slots: [], behavior: null },
    ],
  })
}

/* The shape most keywords that DO something turn out to have: a condition the
 * card names, a result that holds once it is met, and a token to show which.
 * Offered as a starting point rather than built in -- the engine knows nothing
 * about it, and a set is free to write something else entirely.
 *
 * Everything here is Tension's rule said in templates: the condition becomes a
 * silent forced ability (nothing is printed to activate), gated on the token not
 * being there yet so it cannot fire twice, whose steps place the token; and
 * `activeWhen` gates the card's own result on the token being there, which is
 * also why "deactivates if the card leaves play" needs nothing -- tokens go when
 * the card does. */
function conditionResultShape(slug: string): Partial<Api.CustomSetKeyword> {
  const tokensOnThis = (tag: string) => ({
    tag: 'TokensOnThis',
    contents: ['$keywordToken', { tag, contents: { tag: 'Static', contents: 0 } }],
  })
  return {
    slots: [
      { name: 'condition', label: 'Condition', kind: 'window', required: true },
      { name: 'requires', label: '…only when', kind: 'criterion', required: false },
      { name: 'conditionText', label: 'Condition, as printed', kind: 'text', required: false },
      { name: 'resultText', label: 'Result, as printed', kind: 'text', required: false },
      { name: 'steps', label: 'On activating', kind: 'steps', required: false },
    ] satisfies Api.KeywordSlot[],
    behavior: {
      abilities: [
        {
          type: { tag: 'SilentForcedAbility', window: '$condition' },
          criteria: { tag: 'Criteria', contents: [tokensOnThis('EqualTo'), '$requires'] },
          tooltip: '$keywordName',
          steps: [
            {
              push: {
                tag: 'TokenMessage',
                contents: {
                  tag: 'PlaceTokens_',
                  contents: ['$source', '$target', '$keywordToken', 1],
                },
              },
            },
            '$steps',
          ],
        },
      ],
      modifiers: [],
      handlers: [],
      activeWhen: tokensOnThis('GreaterThan'),
    },
    token: slug || null,
  }
}

/* A keyword is rules text until it is given a behaviour, and giving it one is
 * the only thing here that writes a template nobody typed -- so it starts from
 * the shape above rather than from an empty object somebody has to guess at. */
function setKeywordKind(index: number, kind: 'plain' | 'shape') {
  const keyword = draft.value.keywords[index]
  if (kind === 'plain') {
    editKeyword(index, { slots: [], behavior: null })
  } else {
    editKeyword(index, conditionResultShape(slugify(keyword.name)))
  }
}

const keywordKind = (keyword: Api.CustomSetKeyword) => (keyword.behavior ? 'shape' : 'plain')

/* A keyword's behaviour is edited with the same fields a card's abilities are.
 *
 * It has to be: a keyword IS abilities and modifiers, written once for every
 * card that will print it, and asking an author to type that as JSON when the
 * builder already has an editor for every piece of it would be asking them to
 * do by hand what the card form does for them.
 *
 * The only difference is what the fields may refer to. A card's name a value
 * outright; a keyword's name a slot, and each card fills it in -- so the slots
 * are passed down as bindings and the ordinary editors offer them wherever one
 * fits. */
const emptyBehavior = (): Api.KeywordBehavior => ({
  abilities: [],
  modifiers: [],
  handlers: [],
})

function editBehavior(index: number, changes: Partial<Api.KeywordBehavior>) {
  const keyword = draft.value.keywords[index]
  editKeyword(index, { behavior: { ...(keyword.behavior ?? emptyBehavior()), ...changes } })
}

const behaviorOf = (keyword: Api.CustomSetKeyword) => keyword.behavior ?? emptyBehavior()

// --- slots ---

function editSlot(index: number, at: number, changes: Partial<Api.KeywordSlot>) {
  const keyword = draft.value.keywords[index]
  editKeyword(index, {
    slots: (keyword.slots ?? []).map((slot, i) => (i === at ? { ...slot, ...changes } : slot)),
  })
}

function addSlot(index: number) {
  const keyword = draft.value.keywords[index]
  editKeyword(index, {
    slots: [...(keyword.slots ?? []), { name: '', label: '', kind: 'value', required: false }],
  })
}

function removeSlot(index: number, at: number) {
  const keyword = draft.value.keywords[index]
  editKeyword(index, { slots: (keyword.slots ?? []).filter((_, i) => i !== at) })
}

/* What a slot can hold, and what that means for the card filling it in. The
 * kind is the whole of what decides which editor a card gets, so it is worth
 * saying in words rather than offering five bare nouns. */
const SLOT_KINDS = [
  { kind: 'window', label: 'A window', help: 'When it happens. The card picks a timing window.' },
  { kind: 'criterion', label: 'A condition', help: 'Something that has to be true. The card picks a criterion.' },
  { kind: 'text', label: 'Printed text', help: 'Words, shown back to whoever reads the card. The engine never reads them.' },
  { kind: 'steps', label: 'Steps', help: 'Things to do. Insert them with the "Whatever the card fills in" step.' },
  { kind: 'value', label: 'Anything', help: 'Unpinned, so it is offered in every field. Narrow it if you can.' },
]

const slotKindHelp = (kind?: string) => SLOT_KINDS.find((k) => k.kind === kind)?.help ?? ''

/* The `$names` the behaviour refers to.
 *
 * Only asked one way round. A declared slot that nothing refers to is plainly a
 * question every card is asked for no reason. The opposite -- a `$name` with no
 * slot -- cannot be judged from here: most of them are runtime bindings
 * (`$source`, `$controller`) or names a step bound further up, and flagging
 * those would cry wolf at every correct template. */
function referencedNames(behavior: Api.KeywordBehavior | null | undefined): Set<string> {
  const found = new Set<string>()
  const walk = (value: any) => {
    if (typeof value === 'string') {
      if (value.startsWith('$')) found.add(value.slice(1))
    } else if (Array.isArray(value)) {
      value.forEach(walk)
    } else if (value && typeof value === 'object') {
      Object.values(value).forEach(walk)
    }
  }
  walk(behavior ?? {})
  return found
}

const slotUsage = (keyword: Api.CustomSetKeyword) => {
  const referenced = referencedNames(keyword.behavior)
  return {
    /* A `text` slot is the card's own words, shown back to whoever reads it and
     * never read by the engine, so it is doing its job without being referred
     * to anywhere. */
    isSpare: (slot: Api.KeywordSlot) =>
      !!slot.name && slot.kind !== 'text' && !referenced.has(slot.name),
  }
}

function editKeyword(index: number, changes: Partial<Api.CustomSetKeyword>) {
  patch({
    keywords: draft.value.keywords.map((keyword, i) =>
      i === index ? { ...keyword, ...changes } : keyword,
    ),
  })
}

function removeKeyword(index: number) {
  patch({ keywords: draft.value.keywords.filter((_, i) => i !== index) })
}

/* Which token a conditional keyword marks itself with. The slug of its own name
 * is offered as the default, since a set that calls both the keyword and the
 * token Tension needs to say nothing. */
const tokenChoices = computed(() => draft.value.tokens.filter((token) => token.slug))
</script>

<template>
  <div class="expansion">
    <label class="toggle">
      <input
        type="checkbox"
        :checked="draft.investigatorExpansion"
        @change="patch({ investigatorExpansion: ($event.target as HTMLInputElement).checked })"
        @keydown.stop
      />
      <span>
        <strong>{{ t(`${K}investigatorExpansion`) }}</strong>
        <em>{{ t(`${K}investigatorExpansionHint`) }}</em>
      </span>
    </label>

    <div v-if="draft.investigatorExpansion" class="expansion-fields">
      <p v-if="error" class="expansion-error" role="alert">{{ error }}</p>

      <!-- Icon -->
      <section>
        <h4>{{ t(`${K}expansionIcon`) }}</h4>
        <p class="hint">{{ t(`${K}expansionIconHint`) }}</p>
        <div class="icon-row">
          <img v-if="draft.icon" class="icon-preview" :src="draft.icon" alt="" />
          <label class="file">
            <span>{{ uploading === 'icon' ? t(`${K}uploading`) : t(`${K}chooseImage`) }}</span>
            <input
              type="file"
              accept="image/*"
              @click.stop
              @change="takeIcon(($event.target as HTMLInputElement).files?.[0])"
            />
          </label>
          <button v-if="draft.icon" type="button" class="drop" @click="patch({ icon: null })">
            {{ t(`${K}removeImage`) }}
          </button>
        </div>
      </section>

      <!-- One tab per kind of thing the set declares. -->
      <div class="entry-tabs" role="tablist" :aria-label="t(`${K}editExpansion`)">
        <button
          v-for="entry in TABS"
          :key="entry.key"
          type="button"
          role="tab"
          :class="{ on: tab === entry.key }"
          :aria-selected="tab === entry.key"
          @click="tab = entry.key"
        >
          {{ t(`${K}${entry.label}`) }}
          <span class="tab-count">{{ entry.count(draft) }}</span>
        </button>
      </div>

      <template v-if="tab === 'tokens'">
        <!-- Tokens -->
        <section>
          <h4>{{ t(`${K}setTokens`) }}</h4>
          <p class="hint">{{ t(`${K}setTokensHint`) }}</p>
          <ul v-if="draft.tokens.length" class="rows">
            <li v-for="(token, index) in draft.tokens" :key="index" class="row">
              <img v-if="token.image" class="token-preview" :src="token.image" alt="" />
              <span v-else class="token-preview none" aria-hidden="true"></span>
              <input
                class="grow"
                type="text"
                :value="token.name"
                :aria-label="t(`${K}tokenNameLabel`)"
                :placeholder="t(`${K}tokenNameLabel`)"
                @input="editToken(index, { name: ($event.target as HTMLInputElement).value })"
                @keydown.stop
              />
              <input
                class="slug"
                type="text"
                :value="token.slug"
                :aria-label="t(`${K}tokenSlugLabel`)"
                :placeholder="t(`${K}tokenSlugLabel`)"
                @input="editToken(index, { slug: slugify(($event.target as HTMLInputElement).value) })"
                @keydown.stop
              />
              <label class="file small">
                <span>{{ uploading === `token-${index}` ? t(`${K}uploading`) : t(`${K}chooseImage`) }}</span>
                <input
                  type="file"
                  accept="image/*"
                  @click.stop
                  @change="takeTokenImage(index, ($event.target as HTMLInputElement).files?.[0])"
                />
              </label>
              <button type="button" class="drop" @click="removeToken(index)">
                <font-awesome-icon icon="trash" />
              </button>
            </li>
          </ul>
          <button type="button" class="add" @click="addToken">
            <font-awesome-icon icon="plus" /> {{ t(`${K}addToken`) }}
          </button>
        </section>

      </template>

      <template v-if="tab === 'keywords'">
        <!-- Keywords -->
        <section>
          <h4>{{ t(`${K}setKeywords`) }}</h4>
          <p class="hint">{{ t(`${K}setKeywordsHint`) }}</p>
          <ul v-if="draft.keywords.length" class="rows keyword-rows">
            <li v-for="(keyword, index) in draft.keywords" :key="index" class="keyword">
              <div class="row">
                <input
                  class="grow"
                  type="text"
                  :value="keyword.name"
                  :aria-label="t(`${K}keywordNameLabel`)"
                  :placeholder="t(`${K}keywordNameLabel`)"
                  @input="editKeyword(index, { name: ($event.target as HTMLInputElement).value })"
                  @keydown.stop
                />
                <select
                  :value="keywordKind(keyword)"
                  :aria-label="t(`${K}keywordShapeLabel`)"
                  @change="setKeywordKind(index, ($event.target as HTMLSelectElement).value as 'plain' | 'shape')"
                >
                  <option value="plain">{{ t(`${K}keywordShapePlain`) }}</option>
                  <option value="shape">{{ t(`${K}keywordShapeConditional`) }}</option>
                </select>
                <!-- Only a keyword that does something has anything to mark. -->
                <select
                  v-if="keyword.behavior"
                  :value="keyword.token ?? ''"
                  :aria-label="t(`${K}keywordTokenLabel`)"
                  @change="editKeyword(index, { token: ($event.target as HTMLSelectElement).value || null })"
                >
                  <option value="">{{ t(`${K}keywordTokenDefault`, { slug: slugify(keyword.name) }) }}</option>
                  <option v-for="token in tokenChoices" :key="token.slug" :value="token.slug">
                    {{ token.name || token.slug }}
                  </option>
                </select>
                <button type="button" class="drop" @click="removeKeyword(index)">
                  <font-awesome-icon icon="trash" />
                </button>
            </div>

            <textarea
              rows="3"
              :value="keyword.description"
              :aria-label="t(`${K}keywordDescriptionLabel`)"
              :placeholder="t(`${K}keywordDescriptionPlaceholder`)"
              @input="editKeyword(index, { description: ($event.target as HTMLTextAreaElement).value })"
              @keydown.stop
            ></textarea>

            <!-- What the keyword asks each card for, and what it does with the
                 answers. Both are the keyword's, not the engine's. -->
            <details v-if="keyword.behavior" class="mechanics">
              <summary>{{ t(`${K}keywordMechanics`) }}</summary>

              <p class="hint">{{ t(`${K}keywordSlotsHint`) }}</p>

              <ul class="slots">
                <li v-for="(slot, at) in keyword.slots ?? []" :key="at" class="slot">
                  <div class="slot-head">
                    <code class="slot-ref">${{ slot.name || '…' }}</code>
                    <span
                      v-if="slotUsage(keyword).isSpare(slot)"
                      class="slot-unused"
                      v-tooltip="t(`${K}slotUnusedHelp`)"
                    >{{ t(`${K}slotUnused`) }}</span>
                    <span class="spacer"></span>
                    <button
                      type="button"
                      class="drop"
                      v-tooltip="t(`${K}removeSlot`)"
                      :aria-label="t(`${K}removeSlot`)"
                      @click="removeSlot(index, at)"
                    >
                      <font-awesome-icon icon="trash" />
                    </button>
                  </div>

                  <label class="slot-label">
                    {{ t(`${K}slotLabelLabel`) }}
                    <input
                      type="text"
                      :value="slot.label"
                      :placeholder="t(`${K}slotLabelPlaceholder`)"
                      @input="editSlot(index, at, { label: ($event.target as HTMLInputElement).value })"
                      @keydown.stop
                    />
                  </label>

                  <div class="slot-meta">
                    <label>
                      {{ t(`${K}slotNameLabel`) }}
                      <input
                        class="slug"
                        type="text"
                        :value="slot.name"
                        placeholder="condition"
                        @input="editSlot(index, at, { name: ($event.target as HTMLInputElement).value })"
                        @keydown.stop
                      />
                    </label>
                    <label>
                      {{ t(`${K}slotKindLabel`) }}
                      <select
                        :value="slot.kind"
                        @change="editSlot(index, at, { kind: ($event.target as HTMLSelectElement).value as any })"
                      >
                        <option v-for="k in SLOT_KINDS" :key="k.kind" :value="k.kind">{{ k.label }}</option>
                      </select>
                    </label>
                    <label class="inline">
                      <input
                        type="checkbox"
                        :checked="slot.required"
                        @change="editSlot(index, at, { required: ($event.target as HTMLInputElement).checked })"
                      />
                      {{ t(`${K}slotRequired`) }}
                    </label>
                  </div>

                  <p class="hint">{{ slotKindHelp(slot.kind) }}</p>
                  <p v-if="slot.required" class="hint">{{ t(`${K}slotRequiredHelp`) }}</p>
                </li>
              </ul>

              <button type="button" class="add" @click="addSlot(index)">
                <font-awesome-icon icon="plus" /> {{ t(`${K}addSlot`) }}
              </button>

              <h5>{{ t(`${K}keywordBehaviorLabel`) }}</h5>
              <p class="hint">{{ t(`${K}keywordBehaviorHint`) }}</p>

              <!-- The same editors a card's abilities and constants use. What
                   differs is only what the fields may name: this keyword's
                   slots, offered wherever one fits. -->
              <AbilityEditor
                section="abilities"
                :abilities="behaviorOf(keyword).abilities"
                :handlers="[]"
                :modifiers="behaviorOf(keyword).modifiers"
                :extraBindings="keywordBindings(keyword.slots)"
                :slotSteps="keywordStepSlots(keyword.slots)"
                @update:abilities="editBehavior(index, { abilities: $event })"
                @update:modifiers="editBehavior(index, { modifiers: $event })"
              />

              <h5>{{ t(`${K}keywordHandlers`) }}</h5>
              <p class="hint">{{ t(`${K}keywordHandlersHint`) }}</p>
              <AbilityEditor
                section="listeners"
                :abilities="behaviorOf(keyword).abilities"
                :modifiers="behaviorOf(keyword).modifiers"
                :handlers="behaviorOf(keyword).handlers"
                :extraBindings="keywordBindings(keyword.slots)"
                :slotSteps="keywordStepSlots(keyword.slots)"
                @update:handlers="editBehavior(index, { handlers: $event })"
              />

              <h5>{{ t(`${K}keywordActiveWhen`) }}</h5>
              <p class="hint">{{ t(`${K}keywordActiveWhenHint`) }}</p>
              <ValueEditor
                optional
                type="Criterion"
                :label="t(`${K}keywordActiveWhen`)"
                :bindings="keywordBindings(keyword.slots)"
                :modelValue="behaviorOf(keyword).activeWhen"
                @update:modelValue="editBehavior(index, { activeWhen: $event })"
              />
            </details>
          </li>
        </ul>
        <button type="button" class="add" @click="addKeyword">
          <font-awesome-icon icon="plus" /> {{ t(`${K}addKeyword`) }}
        </button>
      </section>
      </template>
    </div>
  </div>
</template>

<style scoped lang="scss">
.expansion {
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
}

.toggle {
  align-items: flex-start;
  cursor: pointer;
  display: flex;
  gap: 0.5rem;

  input {
    margin-top: 0.25rem;
  }

  span {
    display: flex;
    flex-direction: column;
  }

  em {
    font-size: 0.78rem;
    font-style: normal;
    opacity: 0.68;
  }
}

.expansion-fields {
  border-left: 2px solid color-mix(in srgb, var(--title) 22%, transparent);
  display: flex;
  flex-direction: column;
  gap: 1rem;
  padding-left: 0.85rem;
}

/* The same tab row the ability editor wears, so one kind of thing being edited
   looks like another. */
.entry-tabs {
  border-bottom: 1px solid var(--box-border);
  display: flex;
  flex-wrap: wrap;
  gap: 0.25rem;

  button {
    align-items: center;
    background: none;
    border: 1px solid transparent;
    border-bottom: 0;
    border-radius: 4px 4px 0 0;
    color: inherit;
    cursor: pointer;
    display: flex;
    font-size: 0.8rem;
    gap: 0.35rem;
    margin-bottom: -1px;
    opacity: 0.7;
    padding: 0.3rem 0.6rem;
  }

  button.on {
    background: color-mix(in srgb, var(--title) 8%, transparent);
    border-color: var(--box-border);
    opacity: 1;
  }
}

.tab-count {
  background: color-mix(in srgb, var(--title) 14%, transparent);
  border-radius: 100vw;
  font-size: 0.68rem;
  min-width: 1.1rem;
  padding: 0 0.3rem;
  text-align: center;
}

h4 {
  font-size: 0.9rem;
  margin: 0 0 0.15rem;
}

.hint {
  font-size: 0.76rem;
  margin: 0 0 0.45rem;
  opacity: 0.68;
}

.expansion-error {
  background: color-mix(in srgb, var(--delete) 16%, transparent);
  border-radius: 4px;
  font-size: 0.8rem;
  margin: 0;
  padding: 0.35rem 0.6rem;
}

.rows {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  list-style: none;
  margin: 0 0 0.5rem;
  padding: 0;
}

.row {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem;
}

.grow {
  flex: 1 1 8rem;
  min-width: 0;
}

.slug {
  flex: 0 1 7rem;
  font-family: monospace;
  font-size: 0.8rem;
  min-width: 0;
}

.keyword {
  background: color-mix(in srgb, var(--title) 6%, transparent);
  border-radius: 5px;
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
  padding: 0.5rem;

  textarea {
    width: 100%;
  }
}

.icon-row {
  align-items: center;
  display: flex;
  gap: 0.5rem;
}

.icon-preview {
  height: 2.4rem;
  object-fit: contain;
  width: 2.4rem;
}

.token-preview {
  height: 1.6rem;
  object-fit: contain;
  width: 1.6rem;

  &.none {
    border: 1px dashed color-mix(in srgb, var(--title) 35%, transparent);
    border-radius: 50%;
  }
}

/* The native file input is replaced by its label, which is the only way to
   style one; the input itself stays in the DOM so the label still opens it. */
.file {
  background: color-mix(in srgb, var(--title) 12%, transparent);
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.8rem;
  padding: 0.3rem 0.6rem;

  input {
    display: none;
  }

  &.small {
    font-size: 0.75rem;
    padding: 0.25rem 0.5rem;
  }
}

.add {
  align-self: flex-start;
  font-size: 0.8rem;
}

/* The keyword's mechanics: collapsed, because most of the time what is being
   edited is the name and the rules text, and this is the half that is written
   once and then left alone. */
.mechanics {
  border-left: 2px solid color-mix(in srgb, var(--title) 20%, transparent);
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  padding-left: 0.5rem;

  summary {
    cursor: pointer;
    font-size: 0.78rem;
    margin-bottom: 0.35rem;
    opacity: 0.8;
  }

  h5 {
    font-size: 0.82rem;
    margin: 0.5rem 0 0;
    opacity: 0.85;
  }
}

/* A checkbox and its word on one line, against the column layout labels use
   everywhere else here. */
label.inline {
  align-items: center;
  flex-direction: row;
  gap: 0.25rem;
}

/* A slot is four questions about one thing, so it gets a block rather than a
   row: squeezed onto one line the name, the label and the kind all became boxes
   too small to read, and nothing had room to say what it was for. */
.slots {
  display: flex;
  flex-direction: column;
  gap: 0.45rem;
  list-style: none;
  margin: 0 0 0.5rem;
  padding: 0;
}

.slot {
  background: color-mix(in srgb, var(--title) 5%, transparent);
  border-radius: 5px;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  padding: 0.45rem 0.55rem;

  .hint {
    margin: 0;
  }
}

.slot-head {
  align-items: center;
  display: flex;
  gap: 0.4rem;
}

/* The name as the templates below write it, so the two halves read as one
   thing rather than as a table and some JSON that happen to share words. */
.slot-ref {
  background: color-mix(in srgb, var(--select) 18%, transparent);
  border-radius: 3px;
  font-size: 0.75rem;
  padding: 0.1rem 0.35rem;
}

.slot-unused {
  border: 1px solid color-mix(in srgb, var(--important) 45%, transparent);
  border-radius: 3px;
  color: color-mix(in srgb, var(--important) 85%, white);
  font-size: 0.68rem;
  padding: 0.05rem 0.3rem;
}

.slot-warning {
  background: color-mix(in srgb, var(--important) 12%, transparent);
  border: 1px solid color-mix(in srgb, var(--important) 35%, transparent);
  border-radius: 4px;
  font-size: 0.75rem;
  margin: 0 0 0.4rem;
  padding: 0.3rem 0.5rem;
}

.spacer {
  flex: 1;
}

/* The label the card shows gets the width, because it is a sentence; the name,
   the kind and the switch are each a word and sit together on the line below.
   Every one of them stacks its own caption, or the captions and the controls
   interleave and nothing lines up. */
.slot-fields {
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem 0.6rem;

  label {
    display: flex;
    flex: 1 1 100%;
    flex-direction: column;
    font-size: 0.72rem;
    gap: 0.15rem;
    min-width: 0;
    opacity: 0.85;

    input,
    select {
      font-size: 0.8rem;
      opacity: 1;
      width: 100%;
    }
  }

  label.narrow {
    flex: 0 1 9rem;
  }

  /* The checkbox reads as one phrase, so it keeps its word beside it and lines
     up with the bottom of the controls it sits next to. */
  label.inline {
    align-items: center;
    align-self: flex-end;
    flex: 0 0 auto;
    flex-direction: row;
    gap: 0.3rem;
    padding-bottom: 0.4rem;

    input {
      width: auto;
    }
  }
}

.drop {
  /* `base.css` gives every button 11px of side padding over border-box, which
     squeezes an icon-only button to nothing. */
  padding: 0.3rem;
}
</style>
