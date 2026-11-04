type Translate = (key: string, named?: Record<string, unknown>) => string

const symbols = new Set(['Skull', 'Cultist', 'Tablet', 'ElderThing', 'ElderSign', 'AutoFail', 'Bless', 'Curse', 'Frost'])
const numbers: Record<string, string> = {
  PlusOne: '+1', Zero: '0', MinusOne: '−1', MinusTwo: '−2', MinusThree: '−3',
  MinusFour: '−4', MinusFive: '−5', MinusSix: '−6', MinusSeven: '−7', MinusEight: '−8',
}

export function localizedTokenFace(face: string, t: Translate): string {
  const normalized = face.replace(/Token$/, '')
  if (numbers[normalized]) return numbers[normalized]
  if (symbols.has(normalized)) return t(`ui.enums.tokenFaces.${normalized}`)
  return face.includes(':') ? face.split(':').filter(Boolean).join(' / ') : normalized.replace(/([a-z])([A-Z])/g, '$1 $2')
}
