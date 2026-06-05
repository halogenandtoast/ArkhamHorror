import type { GameDetails } from '@/arkham/types/Game'

const gameScopedKeyPattern = /^game:([^:]+):(.+)$/
const gameHostKeyPattern = /^gameHost_(.+)$/
const gameClaimedKeyPattern = /^gameClaimed_(.+)$/
const legacySelectedTabKeyPattern = /^selected-tab:(.+)$/
const legacyGlobalGameSettingKeys = new Set(['showSidebar', 'showOtherPlayersHands'])

type GameAvailability = Pick<GameDetails, 'id' | 'hasOpenSeats'>

export function gameLocalStorageKey(gameId: string, setting: string) {
  return `game:${gameId}:${setting}`
}

export function getGameLocalStorageItem(gameId: string, setting: string) {
  return localStorage.getItem(gameLocalStorageKey(gameId, setting))
}

export function setGameLocalStorageItem(gameId: string, setting: string, value: string) {
  localStorage.setItem(gameLocalStorageKey(gameId, setting), value)
}

export function removeGameLocalStorageItem(gameId: string, setting: string) {
  localStorage.removeItem(gameLocalStorageKey(gameId, setting))
}

function getLocalStorageKeys(): string[] {
  try {
    return Object.keys(localStorage)
  } catch {
    return []
  }
}

function removeLocalStorageItem(key: string) {
  try {
    localStorage.removeItem(key)
  } catch {
    // Ignore storage access errors (private mode, disabled storage, etc.)
  }
}

export function removeGameLocalStorage(gameId: string) {
  for (const key of getLocalStorageKeys()) {
    const scopedMatch = key.match(gameScopedKeyPattern)
    if (scopedMatch?.[1] === gameId) {
      removeLocalStorageItem(key)
      continue
    }

    const hostMatch = key.match(gameHostKeyPattern)
    if (hostMatch?.[1] === gameId) {
      removeLocalStorageItem(key)
      continue
    }

    const claimedMatch = key.match(gameClaimedKeyPattern)
    if (claimedMatch?.[1] === gameId) {
      removeLocalStorageItem(key)
      continue
    }

    const legacySelectedTabMatch = key.match(legacySelectedTabKeyPattern)
    if (legacySelectedTabMatch?.[1] === gameId) removeLocalStorageItem(key)
  }
}

export function cullGameLocalStorage(availableGames: GameAvailability[]) {
  const availableGameIds = new Set(availableGames.map((game) => game.id))
  const gamesWithOpenSeats = new Set(availableGames.filter((game) => game.hasOpenSeats).map((game) => game.id))

  for (const key of getLocalStorageKeys()) {
    const scopedMatch = key.match(gameScopedKeyPattern)
    if (scopedMatch) {
      const [, gameId, setting] = scopedMatch
      if (!availableGameIds.has(gameId) || (setting === 'host' && !gamesWithOpenSeats.has(gameId))) {
        removeLocalStorageItem(key)
      }
      continue
    }

    const hostMatch = key.match(gameHostKeyPattern)
    if (hostMatch) {
      if (!gamesWithOpenSeats.has(hostMatch[1])) removeLocalStorageItem(key)
      continue
    }

    // This was only needed during the seat-claim flow and is no longer read by the app.
    if (gameClaimedKeyPattern.test(key)) {
      removeLocalStorageItem(key)
      continue
    }

    // Legacy location for per-game selected investigator tabs; new keys use game:<id>:selected-tab.
    if (legacySelectedTabKeyPattern.test(key)) {
      removeLocalStorageItem(key)
      continue
    }

    // These used to be global, but are game UI settings now.
    if (legacyGlobalGameSettingKeys.has(key)) removeLocalStorageItem(key)
  }
}
