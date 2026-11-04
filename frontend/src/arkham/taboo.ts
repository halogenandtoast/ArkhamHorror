import { TabooList } from '@/arkham/types/TabooList'

const tabooVersions: Partial<Record<TabooList, [string, string]>> = {
  TabooList15: ['1.5', '2019-04-23'],
  TabooList16: ['1.6', '2019-09-27'],
  TabooList18: ['1.8', '2020-10-15'],
  TabooList19: ['1.9', '2021-06-28'],
  TabooList20: ['2.0', '2022-08-26'],
  TabooList21: ['2.1', '2023-08-30'],
  TabooList22: ['2.2', '2024-02-20'],
  TabooList23: ['2.3', '2024-10-24'],
  TabooList24: ['2.4', '2025-07-11'],
  TabooList25: ['2.5', '2026-02-21'],
}

export function displayTabooList(taboo: TabooList, locale = 'en', unknownLabel = 'Unknown Taboo List'): string {
  const entry = tabooVersions[taboo]
  if (!entry) return unknownLabel
  const [version, date] = entry
  const formattedDate = new Intl.DateTimeFormat(locale, {
    year: 'numeric', month: 'short', day: 'numeric', timeZone: 'UTC',
  }).format(new Date(`${date}T00:00:00Z`))
  return `${version} (${formattedDate})`
}

function tabooListIdToTabooList(tabooId: number): TabooList | null {
  switch (tabooId) {
    case 1: return "TabooList15"
    case 2: return "TabooList16"
    case 3: return "TabooList18"
    case 4: return "TabooList19"
    case 5: return "TabooList20"
    case 6: return "TabooList21"
    case 7: return "TabooList22"
    case 8: return "TabooList23"
    case 9: return "TabooList24"
    case 10: return "TabooList25"
    default: return null
  }
}

export function displayTabooId(tabooId: number, locale = 'en', unknownLabel = 'Unknown Taboo List'): string {
  const taboo = tabooListIdToTabooList(tabooId)
  return taboo ? displayTabooList(taboo, locale, unknownLabel) : unknownLabel
}
