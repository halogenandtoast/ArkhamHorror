import * as JsonDecoder from 'ts.data.json';
import { Card, cardDecoder, } from '@/arkham/types/Card';

export type Search = {
  searchFoundCards: Record<string, Card[]>;
}

export const searchDecoder = JsonDecoder.object<Search>({
  searchFoundCards: JsonDecoder.record<Card[]>(JsonDecoder.array(cardDecoder, 'Card[]'), 'Dict<string, Card[]>'),
}, 'Search');

