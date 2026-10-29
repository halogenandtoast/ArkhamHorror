import type { Investigator } from '@/arkham/types/Investigator'

/** Effective location lists can contain an investigator at multiple locations.
 * Only the miniature at their actual placement owns the movement animation. */
export function investigatorMovementName(
  investigator: Pick<Investigator, 'id' | 'placement'>,
  locationId: string,
): string | undefined {
  return investigator.placement.tag === 'AtLocation' && investigator.placement.contents === locationId
    ? `investigator-${investigator.id}`
    : undefined
}
