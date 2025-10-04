import * as JsonDecoder from 'ts.data.json';
import { Tokens } from '@/arkham/types/Token';
import { customizationsDecoder, Customization } from '@/arkham/types/Customization';
import { v2Optional } from '@/arkham/parser';

export type Card = PlayerCard | EncounterCard | VengeanceCard;

function cardIsFlipped(card: Card) {
  switch (card.tag) {
    case 'PlayerCard':
      return card.contents.isFlipped ?? false;
    case 'EncounterCard':
      return card.contents.isFlipped ?? false;
    case 'VengeanceCard':
      return cardIsFlipped(card.contents);
  }
}

function cardArt(card: Card): string | undefined {
  switch (card.tag) {
    case 'PlayerCard':
      return card.contents.art
    case 'EncounterCard':
      return card.contents.art
    case 'VengeanceCard':
      return cardArt(card.contents);
  }
}

export function asCardCode(card: Card): string {
  switch (card.tag) {
    case 'PlayerCard':
      return card.contents.cardCode
    case 'EncounterCard':
      return card.contents.cardCode
    case 'VengeanceCard':
      return asCardCode(card.contents);
  }
}

export function cardImage(card: Card) {
  const side = cardIsFlipped(card) ? 'b' : ''
  // TODO, send art with cards next to
  const art = cardArt(card) || asCardCode(card).replace('c', '')
  return `cards/${art}${side}.avif`
}

export function toCardContents(card: Card | CardContents): CardContents {
  if (card.tag === 'CardContents') {
    return card
  }

  switch (card.tag) {
    case 'PlayerCard':
      return card.contents
    case 'EncounterCard':
      return card.contents
    case 'VengeanceCard':
      return toCardContents(card.contents);
  }
}

export type CardContents = {
  tag: "CardContents"
  id: string
  cardCode: string
  isFlipped?: boolean
  tokens: Tokens
  art?: string
  customizations?: Customization[]
  mutated?: string 
}

export type VengeanceCard = {
  tag: 'VengeanceCard';
  contents: PlayerCard | EncounterCard;
}

export type PlayerCard = {
  tag: 'PlayerCard';
  contents: CardContents;
}

export type EncounterCard = {
  tag: 'EncounterCard';
  contents: CardContents;
}

export const cardContentsDecoder = JsonDecoder.object<CardContents>(
  {
    tag: JsonDecoder.constant('CardContents'),
    id: JsonDecoder.string(),
    cardCode: JsonDecoder.string(),
    isFlipped: v2Optional(JsonDecoder.boolean()),
    tokens: JsonDecoder.constant({}),
    art: v2Optional(JsonDecoder.string()),
    customizations: v2Optional(customizationsDecoder),
    mutated: v2Optional(JsonDecoder.string()),
  },
  'CardContents',
);

export const playerCardDecoder = JsonDecoder.object<PlayerCard>(
  {
    tag: JsonDecoder.literal('PlayerCard'),
    contents: cardContentsDecoder,
  },
  'PlayerCard',
);

export const encounterCardDecoder = JsonDecoder.object<EncounterCard>(
  {
    tag: JsonDecoder.literal('EncounterCard'),
    contents: cardContentsDecoder,
  },
  'EncounterCard',
);

export const vengeanceCardDecoder = JsonDecoder.object<VengeanceCard>(
  {
    tag: JsonDecoder.literal('VengeanceCard'),
    contents: JsonDecoder.oneOf<PlayerCard | EncounterCard>([playerCardDecoder, encounterCardDecoder], 'VengeanceCardContents')
  },
  'EncounterCard',
);

export const cardDecoder = JsonDecoder.oneOf<Card>(
  [
    playerCardDecoder,
    encounterCardDecoder,
    vengeanceCardDecoder,
  ],
  'Card',
);
