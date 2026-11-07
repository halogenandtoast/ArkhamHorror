import type { Point } from './locationGraph'

const boards = new Map([
  // Night of the Zealot
  ['01120', 'night-of-the-zealot/midnight-masks'],
  ['50025', 'night-of-the-zealot/midnight-masks'],
  ['01142', 'night-of-the-zealot/devourer-below'],
  ['50032', 'night-of-the-zealot/devourer-below'],

  // The Dunwich Legacy (Season 2)
  ['02041', 'the-dunwich-legacy/extracurricular-activity'],
  ['51012', 'the-dunwich-legacy/extracurricular-activity'],
  ['02062', 'the-dunwich-legacy/the-house-always-wins'],
  ['51016', 'the-dunwich-legacy/the-house-always-wins'],
  ['02118', 'the-dunwich-legacy/the-miskatonic-museum'],
  ['51025', 'the-dunwich-legacy/the-miskatonic-museum'],
  ['02159', 'the-dunwich-legacy/the-essex-county-express'],
  ['51031', 'the-dunwich-legacy/the-essex-county-express'],
  ['02195', 'the-dunwich-legacy/blood-on-the-altar'],
  ['51032', 'the-dunwich-legacy/blood-on-the-altar'],
  ['51036', 'the-dunwich-legacy/blood-on-the-altar'],
  ['02236', 'the-dunwich-legacy/undimensioned-and-unseen'],
  ['51041', 'the-dunwich-legacy/undimensioned-and-unseen'],
  ['02274', 'the-dunwich-legacy/where-doom-awaits'],
  ['51047', 'the-dunwich-legacy/where-doom-awaits'],
  ['02311', 'the-dunwich-legacy/lost-in-time-and-space'],
  ['51051', 'the-dunwich-legacy/lost-in-time-and-space'],

  // The Feast of Hemlock Vale
  ['10704', 'hemlock-vale/vale-day'],
  ['10677a', 'hemlock-vale/vale-day'],
  ['10679a', 'hemlock-vale/vale-day'],
  ['10679b', 'hemlock-vale/vale-night'],
  ['10523', 'hemlock-vale/hemlock-house'],
  ['10549', 'hemlock-vale/silent-heath'],
  ['10569', 'hemlock-vale/lost-sister'],
  ['10588', 'hemlock-vale/thing-in-the-depths'],
  ['10605', 'hemlock-vale/twisted-hollow'],
  ['10626', 'hemlock-vale/longest-night'],
  ['10651', 'hemlock-vale/fate-of-the-vale'],

  // The Scarlet Keys
  ['09501', 'the-scarlet-keys/riddles-and-rain'],
  ['09520', 'the-scarlet-keys/dead-heat'],
  ['09545', 'the-scarlet-keys/sanguine-shadows'],
  ['09566', 'the-scarlet-keys/dealings-in-the-dark'],
  ['09591', 'the-scarlet-keys/dancing-mad'],
  ['09609', 'the-scarlet-keys/on-thin-ice'],
  ['09635', 'the-scarlet-keys/dogs-of-war'],
  ['09660', 'the-scarlet-keys/shades-of-suffering'],
  ['09681', 'the-scarlet-keys/without-a-trace'],
  ['09694', 'the-scarlet-keys/congress-of-the-keys'],

  // The Drowned City
  ['11501', 'the-drowned-city/one-last-job'],
  ['11517', 'the-drowned-city/the-western-wall'],
  ['11536', 'the-drowned-city/the-drowned-quarter'],
  ['11553', 'the-drowned-city/the-apiary'],
  ['11587', 'the-drowned-city/the-grand-vault'],
  ['11612', 'the-drowned-city/court-of-the-ancients'],
  ['11639', 'the-drowned-city/obsidian-canyons'],
  ['11673', 'the-drowned-city/sepulchre-of-the-sleeper'],
  ['11682', 'the-drowned-city/the-doom-of-arkham-part-1'],
  ['11688a', 'the-drowned-city/the-doom-of-arkham-part-2'],
  ['11688b', 'the-drowned-city/the-doom-of-arkham-part-2'],
])

// Places to stand in front of the painted buildings, shared by the day and night plates.
const villageAnchors: Record<string, Point> = {
  '10705': [51, 88], // Boarding House
  '10706': [50, 59], // Crossroads
  '10707': [49, 26], // Chapel
  '10708': [21, 85], // Old Mill
  '10709': [16, 47], // Atwood House
  '10710': [83, 27], // General Store
  '10711': [84, 87], // Schoolhouse
  '10712': [81, 55], // Commons
  '10690': [50, 59], // Crossroads on the final day
  '10691': [21, 85], // Old Mill on the final day
  // The Midnight Masks (3x3 Arkham City grid)
  '01134': [18, 18], // Northside
  '01130': [50, 18], // Downtown (First Bank)
  '01131': [50, 18], // Downtown (Arkham Asylum)
  '01132': [82, 18], // Easttown
  '01129': [18, 50], // Miskatonic University
  '01125': [50, 50], // Rivertown
  '01133': [82, 50], // Graveyard
  '01128': [18, 82], // St. Mary's Hospital
  '01126': [50, 82], // Southside (Historical Society)
  '01127': [50, 82], // Southside (Ma's Boarding House)
  '01124': [82, 82], // Your House
}

export function hemlockVillagePosition(code: string): Point | undefined {
  return villageAnchors[code.replace(/^c/, '').replace(/[ab]$/, '')]
}

const houseRows: Record<string, number> = {
  '03': 13, '02': 31, '01': 50, '00': 75, n01: 92,
}

export function hemlockHousePosition(label: string): Point | undefined {
  const cell = /^pos(n01|00|01)(n01|00|01|02|03)$/.exec(label)
  if (!cell) return undefined
  const [, column, row] = cell
  if ((row === '03' || row === 'n01') && column !== '00') return undefined
  return [column === 'n01' ? 18 : column === '01' ? 82 : 50, houseRows[row]]
}

export type HemlockOverview = {
  image: string
  fixedPassages: boolean
  village?: boolean
  house?: boolean
}

export function hemlockOverview(id: string, reference: string, layout?: readonly string[] | null): HemlockOverview | undefined {
  const scenario = id.replace(/^c/, '')
  if (scenario === '10501') {
    // The reference flips when the cave-in replaces the caves with rails.
    const fixedPassages = !reference.endsWith('b')
    const art = fixedPassages ? 'written-in-rock-descent' : 'written-in-rock-maze'
    return { image: `img/scenes/hemlock-vale/${art}.webp`, fixedPassages, village: true }
  }
  // Some branches of the finale leave the Abyss and rebuild the village board.
  const isNightVale = scenario === '10651' && layout?.some((row) => row.trim().split(/\s+/).includes('diamond'))
  const art = isNightVale ? 'hemlock-vale/vale-night' : boards.get(scenario)
  if (!art) return undefined
  return {
    image: `img/scenes/${art}.webp`,
    fixedPassages: false,
    village: true,
    house: art.endsWith('hemlock-house'),
  }
}
