<script lang="ts" setup>
/* A keyword this card prints, out of the ones its set declares.
 *
 * The set says what a keyword means and what it does; the card says only what is
 * printed on it. Which fields that comes to is the keyword's business, not this
 * editor's -- a keyword declares its slots, and one field is rendered per slot,
 * with the editor its `kind` asks for. So a set can invent a keyword with
 * entirely different parts and this form follows without being touched.
 *
 * `modifiers` and `abilities` are the exception, and are offered to every
 * keyword that declares an `activeWhen`: they are the card's own half of the
 * result, gated by the keyword rather than substituted into it.
 */
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { CustomSetKeyword, CustomSetToken, KeywordSlot } from '@/arkham/api'
import { cardBindings } from '@/arkham/customCardBindings'
import AbilityEditor from '@/arkham/components/debug/AbilityEditor.vue'
import StepsEditor from '@/arkham/components/debug/StepsEditor.vue'
import ValueEditor from '@/arkham/components/debug/ValueEditor.vue'

const { t } = useI18n()
const K = 'customCardSets.'

const props = defineProps<{
  modelValue: any[]
  /** What the set this card belongs to declares. Empty until one is declared. */
  setKeywords: CustomSetKeyword[]
  setTokens: CustomSetToken[]
  /** So `$id` knows what kind of id it is, as elsewhere in the builder. */
  cardType?: string
  /** Which query kinds the step editor offers, passed through unchanged. */
  queryKinds: Record<string, string>
}>()

const emit = defineEmits<{ 'update:modelValue': [v: any[]] }>()

const entries = computed(() => props.modelValue ?? [])
const scope = computed(() => cardBindings(props.cardType))

function setEntry(index: number, changes: Record<string, any>) {
  emit(
    'update:modelValue',
    entries.value.map((entry, i) => (i === index ? { ...entry, ...changes } : entry)),
  )
}

const declared = (name: string) => props.setKeywords.find((k) => k.name === name)

const slotsOf = (entry: any): KeywordSlot[] => declared(entry.keyword)?.slots ?? []

/* Whether this keyword asks the card for anything. One that does not is said by
 * its chip alone. A keyword the set no longer declares always does: it is on the
 * card, and saying so is the only way an author finds out. */
const needsInput = (entry: any) =>
  !declared(entry.keyword) || slotsOf(entry).length > 0 || hasResult(entry)

/* What the card PRINTS, and how it WORKS, asked in that order.
 *
 * They are different jobs. The printed clause is the card's words, copied off
 * the art; the rest is the window and the criteria that make it happen. Run
 * together -- which is what one field per slot in declaration order gave --
 * "Condition, as printed" sat between "…only when" and "On activating", and
 * neither half read as a whole. */
const printedSlots = (entry: any) => slotsOf(entry).filter((s) => s.kind === 'text')
const workingSlots = (entry: any) => slotsOf(entry).filter((s) => s.kind !== 'text')

/* The clause as the card prints it, from whatever the text slots hold.
 *
 * Joined with the punctuation the keyword's own rules use -- "Tension - <a>:
 * <b>" -- because that is the shape a keyword with a condition and a result
 * has. A keyword with one text slot reads as `Keyword - <a>`, which is still
 * right; one with three is unlikely and reads as a list, which is no worse than
 * showing nothing. */
const printedClause = (entry: any) => {
  const parts = printedSlots(entry)
    .map((slot) => String(entry[slot.name] ?? '').trim())
    .filter(Boolean)
  return parts.length ? parts.join(': ') : ''
}

/* Whether the keyword's result lasts, which is what makes the card's own
 * modifiers and abilities worth offering: without an `activeWhen` there is
 * nothing to gate them on, and they would hold unconditionally. */
const hasResult = (entry: any) => !!declared(entry.keyword)?.behavior?.activeWhen

/* Which token a keyword marks itself with, when the set has more than one to
 * choose between. Otherwise the keyword's own default is the only answer. */
const tokenChoices = computed(() => props.setTokens.filter((token) => token.slug))

/* `ValueEditor` is told a type; a slot's `kind` is what the keyword author
 * meant. `value` falls back to a free JSON editor, which is right for a slot
 * whose kind nobody narrowed. */
const EDITOR_TYPES: Record<string, string> = {
  window: 'WindowMatcher',
  criterion: 'Criterion',
  value: 'Value',
}
</script>

<template>
  <div class="keyword-effects">
    <!-- Which keywords are printed is said by the chips above; this is only what
         each of them needs from THIS card. An entry with nothing to ask is
         skipped, so turning on a keyword that takes no inputs adds no block. -->
    <div v-for="(entry, index) in entries" :key="index">
     <div v-if="needsInput(entry)" class="block">
      <div class="block-head">
        <strong>{{ entry.keyword }}</strong>
        <span v-if="!declared(entry.keyword)" class="orphan">
          {{ t(`${K}keywordNotDeclared`) }}
        </span>
      </div>

      <!-- What the set says this keyword means. Collapsed: it is read once and
           then known, and it is long enough to bury the fields under it. -->
      <details v-if="declared(entry.keyword)?.description" class="rule">
        <summary>{{ t(`${K}keywordWhatItMeans`) }}</summary>
        <p class="declared">{{ declared(entry.keyword)?.description }}</p>
      </details>

      <!-- What the card says. The clause above the fields, so what is being
           typed can be read back the way it is printed. -->
      <section v-if="printedSlots(entry).length" class="group">
        <h5>{{ t(`${K}keywordAsPrinted`) }}</h5>
        <p class="clause" :class="{ empty: !printedClause(entry) }">
          <strong>{{ entry.keyword }}</strong>
          <template v-if="printedClause(entry)"> — {{ printedClause(entry) }}</template>
          <template v-else> — {{ t(`${K}keywordClausePlaceholder`) }}</template>
        </p>
        <label v-for="slot in printedSlots(entry)" :key="slot.name">
          {{ slot.label }}<span v-if="slot.required" class="required">*</span>
          <input
            type="text"
            :value="entry[slot.name] ?? ''"
            :placeholder="slot.label"
            @input="setEntry(index, { [slot.name]: ($event.target as HTMLInputElement).value })"
            @keydown.stop
          />
        </label>
      </section>

      <!-- And how it actually happens. -->
      <section v-if="workingSlots(entry).length" class="group">
        <h5>{{ t(`${K}keywordHowItWorks`) }}</h5>
        <template v-for="slot in workingSlots(entry)" :key="slot.name">
          <template v-if="slot.kind === 'steps'">
            <h6>{{ slot.label }}<span v-if="slot.required" class="required">*</span></h6>
            <StepsEditor
              :queryKinds="queryKinds"
              :bindings="scope"
              :path="`keyword-${index}-${slot.name}`"
              :modelValue="entry[slot.name] ?? []"
              @update:modelValue="setEntry(index, { [slot.name]: $event })"
            />
          </template>

          <ValueEditor
            v-else
            :optional="!slot.required"
            :type="EDITOR_TYPES[slot.kind] ?? 'Value'"
            :label="slot.label"
            :bindings="scope"
            :modelValue="entry[slot.name]"
            @update:modelValue="setEntry(index, { [slot.name]: $event })"
          />
        </template>
      </section>

      <!-- Which token marks it. Only worth asking when the set has more than
           one; otherwise the keyword's own default is the only answer. -->
      <label v-if="hasResult(entry) && tokenChoices.length > 1">
        {{ t(`${K}keywordTokenLabel`) }}
        <select
          :value="entry.token ?? ''"
          @change="setEntry(index, { token: ($event.target as HTMLSelectElement).value || undefined })"
        >
          <option value="">{{ declared(entry.keyword)?.token ?? '—' }}</option>
          <option v-for="token in tokenChoices" :key="token.slug" :value="token.slug">
            {{ token.name || token.slug }}
          </option>
        </select>
      </label>

      <!-- The card's own half of the result, which the keyword gates. -->
      <template v-if="hasResult(entry)">
        <h5>{{ t(`${K}keywordWhileActive`) }}</h5>
        <p class="hint">{{ t(`${K}keywordWhileActiveHint`) }}</p>
        <AbilityEditor
          section="abilities"
          :cardType="cardType"
          :abilities="entry.abilities ?? []"
          :handlers="[]"
          :modifiers="entry.modifiers ?? []"
          @update:abilities="setEntry(index, { abilities: $event })"
          @update:modifiers="setEntry(index, { modifiers: $event })"
        />
      </template>
     </div>
    </div>
  </div>
</template>

<style scoped lang="scss">
.keyword-effects {
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
}

.hint {
  font-size: 0.78rem;
  margin: 0;
  opacity: 0.72;

  &.muted {
    opacity: 0.55;
  }
}

.block {
  background: color-mix(in srgb, var(--title) 6%, transparent);
  border-radius: 6px;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  padding: 0.6rem;
}

/* A keyword on the card that the set no longer declares. Its slots cannot be
   shown -- nothing says what they are -- so saying so is all that is left. */
.orphan {
  color: var(--important);
  font-size: 0.72rem;
}

.block-head {
  align-items: center;
  display: flex;
  justify-content: space-between;
}

/* What the set says the keyword means: reference, not a field. */
.rule {
  summary {
    cursor: pointer;
    font-size: 0.76rem;
    opacity: 0.7;
  }
}

.declared {
  border-left: 2px solid color-mix(in srgb, var(--title) 25%, transparent);
  font-size: 0.78rem;
  margin: 0.35rem 0 0;
  opacity: 0.78;
  padding-left: 0.5rem;
  white-space: pre-wrap;
}

/* The two halves of a clause -- what it says, and how it happens -- kept apart,
   because they are copied from different places and checked against different
   things. */
.group {
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
}

/* The clause as it reads on the card, built from the fields below it. Set in
   the card's own face so it is checked against the art rather than against the
   form. */
.clause {
  background: color-mix(in srgb, var(--title) 8%, transparent);
  border-radius: 4px;
  font-size: 0.82rem;
  line-height: 1.35;
  margin: 0;
  padding: 0.4rem 0.55rem;

  &.empty {
    opacity: 0.5;
  }
}

h6 {
  font-size: 0.78rem;
  margin: 0.25rem 0 0;
  opacity: 0.8;
}

h5 {
  font-size: 0.82rem;
  margin: 0.3rem 0 0;
  opacity: 0.85;
}

label {
  display: flex;
  flex-direction: column;
  font-size: 0.8rem;
  gap: 0.2rem;
}

/* A slot the keyword insists on: leaving it empty means the keyword contributes
   nothing to this card at all, so it is worth marking. */
.required {
  color: var(--important);
  margin-left: 0.15rem;
}

.drop {
  padding: 0.3rem;
}
</style>
