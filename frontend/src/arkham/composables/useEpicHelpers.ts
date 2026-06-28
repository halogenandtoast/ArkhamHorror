import { useI18n } from 'vue-i18n'
import type { GroupDigest } from '@/arkham/types/EpicEvent'

// Shared helpers for the Epic Multiplayer organizer surfaces (dashboard + in-game
// bar). `groupLabel` formats a group's display name, falling back to an i18n
// letter label ("Group A"). Older events may have persisted default names like
// "Group 1"; normalize those for display without changing backend data.
export function useEpicHelpers() {
  const { t } = useI18n()

  function groupLetter(ordinal: number): string {
    let index = ordinal
    let label = ''
    do {
      label = String.fromCharCode(65 + (index % 26)) + label
      index = Math.floor(index / 26) - 1
    } while (index >= 0)
    return label
  }

  function defaultGroupLabel(ordinal: number): string {
    return t('event.group', { ordinal: groupLetter(ordinal) })
  }

  function groupLabel(group: GroupDigest): string {
    const name = group.name?.trim()
    if (!name || /^Group \d+$/i.test(name)) return defaultGroupLabel(group.ordinal)
    return name
  }

  return { groupLabel, groupLetter, defaultGroupLabel }
}
