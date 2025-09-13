import { TabooList } from '@/arkham/types/TabooList'

export function displayTabooList(taboo: TabooList): string {
  switch (taboo) {
    case "TabooList15": return "1.5 (Apr 23, 2019)"
    case "TabooList16": return "1.6 (Sep 27, 2019)"
    case "TabooList18": return "1.8 (Oct 15, 2020)"
    case "TabooList19": return "1.9 (Jun 28, 2021)"
    case "TabooList20": return "2.0 (Aug 26, 2022)"
    case "TabooList21": return "2.1 (Aug 30, 2023)"
    case "TabooList22": return "2.2 (Feb 20, 2024)"
    case "TabooList23": return "2.3 (Oct 24, 2024)"
    case "TabooList24": return "2.4 (Jul 11, 2025)"
    default: return "Unknown Taboo List"
  }
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
    default: return null
  }
}

export function displayTabooId(tabooId: number): string {
  const taboo = tabooListIdToTabooList(tabooId)
  return taboo ? displayTabooList(taboo) : "Unknown Taboo List"
}
