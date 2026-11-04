import { CARD_TYPES, cardKind } from '@/arkham/customCards'

export function localizedCardKind(cardType: string, t: (key: string) => string): string {
  const kind = cardKind(cardType)
  return CARD_TYPES.some((entry) => cardKind(entry.value) === kind) ? t(`ui.enums.cardKinds.${kind}`) : kind
}
