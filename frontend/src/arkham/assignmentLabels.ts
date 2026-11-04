type Translate = (key: string, params: Record<string, number>) => string

// The engine also uses these English totals to identify pending damage tokens.
// Localize their presentation without changing the saved question or its counts.
export function localizedAssignmentLabel(label: string, t: Translate): string | null {
  const both = label.match(/^Assign (\d+) damage and (\d+) horror$/)
  if (both) return t('label.assignDamageAndHorror', { damage: Number(both[1]), horror: Number(both[2]) })
  const damage = label.match(/^Assign (\d+) damage$/)
  if (damage) return t('label.assignDamage', { damage: Number(damage[1]) })
  const horror = label.match(/^Assign (\d+) horror$/)
  if (horror) return t('label.assignHorror', { horror: Number(horror[1]) })
  return null
}
