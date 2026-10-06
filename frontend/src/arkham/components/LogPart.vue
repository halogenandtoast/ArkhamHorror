<script lang="ts" setup>
/* One piece of a log entry's sentence.
 *
 * Replaces the regex-and-h() render function in GameMessage.vue. Parts arrive
 * already parsed, so this is a plain template -- and a LogI18n part renders
 * through <i18n-t>, which is what finally lets a localized sentence carry card
 * chips. The old system made you pick one or the other, which is why most log
 * lines were hardcoded English.
 */
import { computed } from 'vue'
import { cardArt } from '@/arkham/cardImages'
import { chaosTokenImage } from '@/arkham/types/ChaosToken'
import { useDbCardStore } from '@/stores/dbCards'
import { customCardDef, isCustomCardCode } from '@/arkham/customCards'
import type { LogPart, LogRef } from '@/arkham/types/GameLog'
import { formatKey, logKeyTitle } from '@/arkham/types/Log'
import { investigatorClass, type CssClassFlags } from '@/arkham/helpers'

const props = defineProps<{ part: LogPart }>()

const dbCards = useDbCardStore()

/* Skill glyphs come from the global classes in styles/icons.css, named
 * "<skill>-icon" as Investigator.vue and HistoryPanel.vue spell them. The
 * template renders that class rather than carrying its own glyph set. */

/* The card database wins over the name the server sent.
 *
 * The narrator reads only messages, so it has no names to send and puts the
 * card code in `name` as a fallback. Resolving here is also better where the
 * server does have a name: this one is localized, and it follows a card whose
 * identity changed after the entry was written. */
function refName(ref: LogRef): string {
  if (ref.cardCode) {
    /* A custom card is in nobody's ArkhamDB index — look it up in the local
     * registry instead, or a custom investigator renders as its '*'-prefixed
     * code. */
    if (isCustomCardCode(ref.cardCode)) {
      const def = customCardDef(ref.cardCode)
      if (def?.name?.title) return def.name.title
    } else {
      /* cardArt() strips the 'c' the engine prefixes onto a card code; the
       * store is keyed without it, as Question.vue and useCardOptions do. */
      const code = cardArt(ref.cardCode)
      /* An act, agenda or double-sided card carries its side as a trailing
       * letter (03047a), which ArkhamDB does not record separately -- the index
       * is keyed on the bare 03047. getDbCard already tries an added 'a' for
       * split-card fronts, so the missing direction is stripping one. Without
       * this the log printed the raw code: "takes 1 damage from c03047a". */
      const card = dbCards.getDbCard(code) ?? dbCards.getDbCard(code.replace(/[a-h]$/, ''))
      if (card?.name) return card.name
    }
  }
  return ref.name
}

/* The art to show on hover. Prefers the specific copy, then the printed code,
 * and honours faceDown -- which the server now resolves, rather than the
 * renderer reaching into game state mid-render as it used to. */
/* An investigator's name takes their class colour, the way the rest of the app
   tints an investigator. Only investigators: a location or an enemy has no
   class, and every other ref kind already has a colour of its own below. */
function refClass(ref: LogRef): CssClassFlags {
  if (ref.kind !== 'RefInvestigator' || !ref.cardCode) return {}
  return investigatorClass(cardArt(ref.cardCode))
}

function refImageId(ref: LogRef): string | undefined {
  if (ref.cardCode) return cardArt(ref.cardCode, ref.faceDown ? 'b' : '')
  return ref.cardId ?? ref.entityId ?? undefined
}

const i18nKey = computed(() =>
  props.part.tag === 'LogI18n' ? props.part.contents[0] : '',
)
const i18nVars = computed(() =>
  props.part.tag === 'LogI18n' ? props.part.contents[1] : {},
)

/* A `count` variable drives pluralization, matching the backend's `countVar`.
 * vue-i18n needs the number on the component, not just among the slots, or a
 * message written with `|` branches never picks one. Null when there is no
 * count, so a non-plural message is unaffected. */
const hasVars = computed(() => Object.keys(i18nVars.value).length > 0)

const plural = computed(() => {
  const count = i18nVars.value['count']
  return count?.tag === 'LogNumber' ? count.contents : null
})
</script>

<template>
  <span v-if="part.tag === 'LogText'">{{ part.contents }}</span>

  <!-- No variables: plain text. Nesting an <i18n-t> inside another one's slot
       leaves the outer placeholder unsubstituted, so a leaf i18n part must not
       render as a component. -->
  <span v-else-if="part.tag === 'LogI18n' && !hasVars">{{ $t(i18nKey) }}</span>

  <i18n-t
    v-else-if="part.tag === 'LogI18n'"
    :keypath="i18nKey"
    :plural="plural ?? undefined"
    tag="span"
    scope="global"
  >
    <template v-for="(value, name) in i18nVars" #[name] :key="name">
      <LogPart :part="value" />
    </template>
  </i18n-t>

  <span
    v-else-if="part.tag === 'LogRefPart'"
    class="log-ref"
    :class="[`log-ref--${part.contents.kind}`, refClass(part.contents)]"
    :data-image-id="refImageId(part.contents)"
    >{{ refName(part.contents) }}</span
  >

  <span v-else-if="part.tag === 'LogNumber'" class="log-num">{{ part.contents }}</span>

  <span
    v-else-if="part.tag === 'LogDelta'"
    class="log-delta"
    :class="part.contents < 0 ? 'log-delta--down' : 'log-delta--up'"
    >{{ part.contents < 0 ? '−' : '+' }}{{ Math.abs(part.contents) }}</span
  >

  <img
    v-else-if="part.tag === 'LogToken'"
    class="log-token"
    :src="chaosTokenImage(part.contents)"
    :alt="part.contents"
    width="23"
  />

  <i v-else-if="part.tag === 'LogIcon'" class="log-icon" :class="`${part.contents}-icon`" />

  <!-- The key's own i18n path and humanised fallback both come from the campaign
       log's own helpers, so a recorded entry reads the same here as it does on
       that screen. -->
  <span v-else-if="part.tag === 'LogCampaignKey'">{{
    logKeyTitle(formatKey(part.contents), $t)
  }}</span>

  <!-- The locale joins the list, so "a, b, and c" is not built server-side. -->
  <span v-else-if="part.tag === 'LogList'">
    <template v-for="(item, i) in part.contents" :key="i">
      <span v-if="i > 0">{{
        i === part.contents.length - 1 ? $t('log.listLast') : $t('log.listSeparator')
      }}</span>
      <LogPart :part="item" />
    </template>
  </span>
</template>

<style scoped>
.log-ref {
  color: #bbb;
  cursor: pointer;
}

/* Lightened class colours, not the --guardian/--survivor tokens themselves.
   Those are built to be fills and borders; as text on this panel they fail
   badly -- survivor is 3.45:1 on the plain background and 2.28:1 on the green
   result band, where a name is read most often.

   These are the smallest lift toward white that clears 4.5:1 on every surface a
   name can land on: the panel, a group block, its header and its result band
   (green, red or neutral), a chat card, a Record entry and the turn banner. The
   green pass band is the binding case at 4.5; everything else is 5+. The
   fallback keeps --multiclass, which already passes.

   If a new tinted surface gets an investigator name on it, re-check these. */
.log-ref--RefInvestigator { color: var(--multiclass); }
.log-ref--RefInvestigator.guardian { color: #81c5fd; }
.log-ref--RefInvestigator.seeker { color: #f2b263; }
.log-ref--RefInvestigator.rogue { color: #8cce90; }
.log-ref--RefInvestigator.mystic { color: #d4b0f7; }
.log-ref--RefInvestigator.survivor { color: #f7acb0; }
.log-ref--RefInvestigator.neutral { color: var(--neutral); }
.log-ref--RefLocation { color: #9ecbe8; }
.log-ref--RefEnemy { color: #e39b94; }

.log-num {
  font-variant-numeric: tabular-nums;
}

.log-delta {
  font-variant-numeric: tabular-nums;
  padding: 0 3px;
  border-radius: 3px;
}

.log-delta--up { color: #a8d06a; background: rgba(135, 156, 90, 0.18); }
.log-delta--down { color: #e39b94; background: rgba(174, 66, 54, 0.2); }

.log-icon {
  font-style: normal;
}

img.log-token {
  display: inline-block;
  vertical-align: text-top;
}
</style>
