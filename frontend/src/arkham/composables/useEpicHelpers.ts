import { useI18n } from 'vue-i18n'
import type { GroupDigest } from '@/arkham/types/EpicEvent'

// Shared helpers for the Epic Multiplayer organizer surfaces (dashboard + in-game
// bar). `groupLabel` formats a group's display name, falling back to an i18n
// ordinal label ("Group N") when the group has no custom name.
export function useEpicHelpers() {
  const { t } = useI18n()

  function groupLabel(group: GroupDigest): string {
    const name = group.name?.trim()
    return name ? name : t('event.group', { ordinal: group.ordinal + 1 })
  }

  return { groupLabel }
}
