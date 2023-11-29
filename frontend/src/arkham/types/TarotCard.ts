import { JsonDecoder } from "ts.data.json";

export type TarotScope = { tag: 'GlobalTarot' } | { tag: 'InvestigatorTarot', contents: string };

export const tarotScopeDecoder = JsonDecoder.oneOf<TarotScope>([
  JsonDecoder.object({ tag: JsonDecoder.isExactly('GlobalTarot') }, 'GlobalTarot'),
  JsonDecoder.object<TarotScope>({ tag: JsonDecoder.isExactly('InvestigatorTarot'), contents: JsonDecoder.string }, 'InvestigatorTarot'),
], 'TarotScope');

export type TarotCardFacing = 'Upright' | 'Reversed';
export type TarotCardArcana
  = 'TheFool0'
  | 'TheMagicianI'
  | 'TheHighPriestessII'
  | 'TheEmpressIII'
  | 'TheEmperorIV'
  | 'TheHierophantV'
  | 'TheLoversVI'
  | 'TheChariotVII'
  | 'StrengthVIII'
  | 'TheHermitIX'
  | 'WheelOfFortuneX'
  | 'JusticeXI'
  | 'TheHangedManXII'
  | 'DeathXIII'
  | 'TemperanceXIV'
  | 'TheDevilXV'
  | 'TheTowerXVI'
  | 'TheMoonXVIII'
  | 'TheStarXVII'
  | 'TheSunXIX'
  | 'JudgementXX'
  | 'TheWorldXXI'

export const tarotCardArcanaDecoder = JsonDecoder.oneOf<TarotCardArcana>([
  JsonDecoder.isExactly('TheFool0'),
  JsonDecoder.isExactly('TheMagicianI'),
  JsonDecoder.isExactly('TheHighPriestessII'),
  JsonDecoder.isExactly('TheEmpressIII'),
  JsonDecoder.isExactly('TheEmperorIV'),
  JsonDecoder.isExactly('TheHierophantV'),
  JsonDecoder.isExactly('TheLoversVI'),
  JsonDecoder.isExactly('TheChariotVII'),
  JsonDecoder.isExactly('StrengthVIII'),
  JsonDecoder.isExactly('TheHermitIX'),
  JsonDecoder.isExactly('WheelOfFortuneX'),
  JsonDecoder.isExactly('JusticeXI'),
  JsonDecoder.isExactly('TheHangedManXII'),
  JsonDecoder.isExactly('DeathXIII'),
  JsonDecoder.isExactly('TemperanceXIV'),
  JsonDecoder.isExactly('TheDevilXV'),
  JsonDecoder.isExactly('TheTowerXVI'),
  JsonDecoder.isExactly('TheMoonXVIII'),
  JsonDecoder.isExactly('TheStarXVII'),
  JsonDecoder.isExactly('TheSunXIX'),
  JsonDecoder.isExactly('JudgementXX'),
  JsonDecoder.isExactly('TheWorldXXI'),
], 'TarotCardArcana')


export const tarotCardFacingDecoder = JsonDecoder.oneOf<TarotCardFacing>([
  JsonDecoder.isExactly('Upright'),
  JsonDecoder.isExactly('Reversed'),
], 'TarotCardFacing')

export type TarotCard = {
  arcana: TarotCardArcana
  facing: TarotCardFacing
  scope: TarotScope
}

export const tarotCardDecoder = JsonDecoder.object<TarotCard>({
  arcana: tarotCardArcanaDecoder,
  facing: tarotCardFacingDecoder,
  scope: JsonDecoder.constant({ tag: 'GlobalTarot' }),
}, 'TarotCard')

export const tarotCardImage = (card: TarotCard) => tarotArcanaImage(card.arcana)

export const tarotArcanaImage = (arcana: TarotCardArcana) => {
  switch (arcana) {
    case 'TheFool0': return 'tarot-0.jpg'
    case 'TheMagicianI': return 'tarot-1.jpg'
    case 'TheHighPriestessII': return 'tarot-2.jpg'
    case 'TheEmpressIII': return 'tarot-3.jpg'
    case 'TheEmperorIV': return 'tarot-4.jpg'
    case 'TheHierophantV': return 'tarot-5.jpg'
    case 'TheLoversVI': return 'tarot-6.jpg'
    case 'TheChariotVII': return 'tarot-7.jpg'
    case 'StrengthVIII': return 'tarot-8.jpg'
    case 'TheHermitIX': return 'tarot-9.jpg'
    case 'WheelOfFortuneX': return 'tarot-10.jpg'
    case 'JusticeXI': return 'tarot-11.jpg'
    case 'TheHangedManXII': return 'tarot-12.jpg'
    case 'DeathXIII': return 'tarot-13.jpg'
    case 'TemperanceXIV': return 'tarot-14.jpg'
    case 'TheDevilXV': return 'tarot-15.jpg'
    case 'TheTowerXVI': return 'tarot-16.jpg'
    case 'TheStarXVII': return 'tarot-17.jpg'
    case 'TheMoonXVIII': return 'tarot-18.jpg'
    case 'TheSunXIX': return 'tarot-19.jpg'
    case 'JudgementXX': return 'tarot-20.jpg'
    case 'TheWorldXXI': return 'tarot-21.jpg'
    default : throw new Error("No such tarot card: " + arcana)
  }
}
