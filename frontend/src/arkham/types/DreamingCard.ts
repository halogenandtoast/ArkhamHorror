import * as JsonDecoder from 'ts.data.json';
import { v2Optional, withDefault } from '@/arkham/parser';
import { Card, cardDecoder, toCardContents } from '@/arkham/types/Card';
import { Tokens, tokenPairsDecoder } from '@/arkham/types/Token';

/* A card placed facedown in an investigator's dream zone, counting down its
 * dreams. Mirrors `Arkham.DreamingCard`, which encodes as
 * `{"card": <Card>, "tokens": [["Dream", 2]]}`.
 *
 * The list is append-only and removed by `CardId`, never re-sorted, so `cardId`
 * is a stable key for a cell and for its `view-transition-name`.
 *
 * `tokens` is read here rather than off the card: `cardContentsDecoder` pins
 * every card's `tokens` to `{}`, so a dreaming card decoded as a plain `Card`
 * shows zero dreams while holding three.
 */
export type DreamingCard = {
  cardId: string
  tokens: Tokens
  /* Face up in the zone.
   *
   * The engine does not send this today: `FlipDreamingCardFaceup` REMOVES the
   * record and puts the card into play, so a dreaming card is only ever
   * facedown here. Kept because the renderer needs the face art regardless (the
   * owner hovers it), so honouring an in-zone face-up beat costs a class
   * toggle -- and because a card may yet want one. */
  flipped: boolean
  card?: Card
}

const dreamTokensDecoder = withDefault<Tokens, Tokens>({}, tokenPairsDecoder)
const flippedDecoder = withDefault<boolean, boolean>(false, JsonDecoder.boolean())

/* The engine's shape: the whole card, inline. */
const byCard = JsonDecoder.object<{ card: Card; tokens: Tokens; flipped: boolean }>(
  {
    card: cardDecoder,
    tokens: dreamTokensDecoder,
    flipped: flippedDecoder,
  },
  'DreamingCard',
).map(({ card, tokens, flipped }) => ({ cardId: toCardContents(card).id, tokens, flipped, card }))

/* Only the id, for a payload that slims the card out and leaves it to be looked
 * up in the `game.cards` registry. */
const byCardId = JsonDecoder.object<DreamingCard>(
  {
    cardId: JsonDecoder.string(),
    tokens: dreamTokensDecoder,
    flipped: flippedDecoder,
    card: v2Optional(cardDecoder),
  },
  'DreamingCardById',
)

export const dreamingCardDecoder: JsonDecoder.Decoder<DreamingCard> =
  JsonDecoder.oneOf<DreamingCard>([byCard, byCardId], 'DreamingCard')

/* Absent or null decodes to an empty zone -- the investigator field had not
 * landed when this was written, and every game predating it has no such key.
 * A *mismatched* shape still fails loudly, which is what we want: an
 * object-encoded `tokens` must not quietly render a card's dreams as zero. */
export const dreamingCardsDecoder = withDefault<DreamingCard[], DreamingCard[]>(
  [],
  JsonDecoder.array<DreamingCard>(dreamingCardDecoder, 'DreamingCard[]'),
)
