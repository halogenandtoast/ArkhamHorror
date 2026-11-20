import { cardArt } from '@/arkham/cardImages'
import { customCardDef, isCustomCardCode } from '@/arkham/customCards'
import type { useDbCardStore } from '@/stores/dbCards'
import type { LogRef } from '@/arkham/types/GameLog'

export type DbCards = ReturnType<typeof useDbCardStore>

/* The name to show for a log ref.
 *
 * The card database wins over the name the server sent. The narrator reads only
 * messages, so it often has no name to send and puts the card code in `name` as
 * a fallback. Resolving here is also better where the server does have a name:
 * this one is localized, and it follows a card whose identity changed after the
 * entry was written.
 *
 * Lives here rather than in LogPart.vue because the transcript export renders
 * the same entries to text, and a second copy of this would drift.
 *
 * Takes only the two fields it reads, not a whole 'LogRef', so a caller holding
 * just a card code can name it without inventing the rest of a log entry --
 * resourceProhibitions.ts names a modifier's source that way.
 */
export function logRefName(
  ref: Pick<LogRef, 'cardCode' | 'name'> & Partial<LogRef>,
  dbCards: DbCards,
): string {
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
    return ref.name
  }
  /* No code at all (a legacy campaign-log record names its card by title):
   * the localized database can still answer by English name, with or without
   * the subtitle the title carries. */
  const byName = dbCards.getCardName(ref.name)
  if (byName !== ref.name) return byName
  const bare = ref.name.split(': ')[0] ?? ref.name
  const bareName = dbCards.getCardName(bare)
  return bareName !== bare ? bareName : ref.name
}
