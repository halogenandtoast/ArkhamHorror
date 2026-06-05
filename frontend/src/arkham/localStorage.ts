import type { GameDetails } from '@/arkham/types/Game'

const gameScopedKeyPattern = /^game:([^:]+):/
const gameHostKeyPattern = /^gameHost_(.+)$/
const gameClaimedKeyPattern = /^gameClaimed_(.+)$/

type GameAvailability = Pick<GameDetails, 'id' | 'hasOpenSeats'>

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
    if (claimedMatch?.[1] === gameId) removeLocalStorageItem(key)
  }
}

export function cullGameLocalStorage(availableGames: GameAvailability[]) {
  const availableGameIds = new Set(availableGames.map((game) => game.id))
  const gamesWithOpenSeats = new Set(availableGames.filter((game) => game.hasOpenSeats).map((game) => game.id))

  for (const key of getLocalStorageKeys()) {
    const scopedMatch = key.match(gameScopedKeyPattern)
    if (scopedMatch) {
      if (!availableGameIds.has(scopedMatch[1])) removeLocalStorageItem(key)
      continue
    }

    const hostMatch = key.match(gameHostKeyPattern)
    if (hostMatch) {
      if (!gamesWithOpenSeats.has(hostMatch[1])) removeLocalStorageItem(key)
      continue
    }

    // This was only needed during the seat-claim flow and is no longer read by the app.
    if (gameClaimedKeyPattern.test(key)) removeLocalStorageItem(key)
  }
}
