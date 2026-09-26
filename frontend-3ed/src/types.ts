/* eslint-disable @typescript-eslint/no-explicit-any */
// Shapes of the /api/v1/3ed contract. The engine's game JSON is only typed as
// far as the UI reads it; anything aeson-tagged stays loose.

export interface User {
  username: string
  email: string
  beta: boolean
  admin: boolean
}

export type GameMode = 'StandardMode' | 'StoryMode' | 'ChallengeMode'

export interface ScenarioInfo {
  code: string
  name: string
  expansion: string
  playable: boolean
  anomalySet: string | null
}

export interface InvestigatorDef {
  name: string
  occupation: string
  abilityText: string
  health: number
  sanity: number
  focusLimit?: number | null
  skills: Record<string, number>
  roles?: string[]
}

export interface Catalog {
  scenarios: ScenarioInfo[]
  expansions: string[]
  investigatorNames: Record<string, string>
  investigatorDefs: Record<string, InvestigatorDef>
}

export interface Seat {
  player: number
  username: string | null
}

export interface TableOptions {
  expansions: string[]
  mode: GameMode
  debug: boolean
}

export interface TableSummary {
  id: string
  name: string
  hostName: string
  seats: Seat[]
  options: TableOptions
  started: boolean
  createdAt: string
}

export interface GameView {
  game: Game
  cardNames: Record<string, string>
  cardCodes: Record<string, string>
  eventNeighborhoods: Record<string, string>
  unstable: string[]
  massive: number[]
  actionAllowance: Record<string, number>
}

export interface TableView extends Omit<TableSummary, 'createdAt'> {
  version: number
  // what produced this copy: 'answer', 'undo', 'debug', 'join', ...; null when fetched
  cause: string | null
  canUndo: boolean
  view: GameView | null
}

export type TableMessage = { tag: 'TableUpdate'; contents: TableView } | { tag: 'TableClosed' }

export type CardId = number

export interface Tagged {
  tag: string
  contents?: any
}

export interface Choice {
  label: Tagged
  messages?: Tagged[]
}

export interface Question {
  prompt: string
  choices: Choice[]
}

export interface Marker {
  faceUp: boolean
  color: string
}

export interface Space {
  id: string
  name: string
  neighborhood?: string | null
  kind?: Tagged
  doom: number
  clues: number
  markers: Marker[]
}

export interface Neighborhood {
  name: string
  clues: number
  anomaly: boolean
  terror: number
  markers: Marker[]
}

export interface Layout {
  tiles: { neighborhood: string; x: number; y: number }[]
  streets: { space: string; x: number; y: number; angle: number }[]
  anchors: { space: string; x: number; y: number }[]
}

export interface Board {
  spaces: Record<string, Space>
  neighborhoods: Record<string, Neighborhood>
  layout?: Layout | null
  borders?: Record<string, Record<string, unknown>>
}

export interface Investigator {
  id: string
  space: string | null
  money: number
  damage: number
  horror: number
  clues: number
  remnants: number
  focus: [string, number][] | Record<string, number>
  assets: CardId[]
  status: string
  active: boolean
  delayed: boolean
  actionsTaken?: number
}

export interface Monster {
  card: CardId
  space: string | null
  state: { tag: string; contents?: any }
  damage?: number
}

export interface Asset {
  damage?: number
  horror?: number
  flipped?: boolean
  owner?: string | null
}

export interface Die {
  value: number
  removed?: boolean
}

export interface SkillTest {
  investigator: string
  skill: string
  kind?: Tagged
  step: string
  dice: Die[]
  modifier?: number
  bonusDice?: number
  addedSuccesses?: number
  chosenAssets?: CardId[]
}

export interface Encounter {
  card: CardId
  investigator: string
  deck?: Tagged
  section?: [number, number] | null
}

export interface CodexEntry {
  number: number
  card: CardId
  flipped: boolean
  tokens?: { clues?: number }
}

export interface Decks {
  neighborhoods: Record<string, CardId[]>
  street: CardId[]
  anomaly: CardId[]
  ally: CardId[]
  spell: CardId[]
  item: CardId[]
  headline: CardId[]
  headlineDiscard: CardId[]
  monster: CardId[]
  event: CardId[]
  eventDiscard: CardId[]
  display: CardId[]
}

export interface Game {
  scenario: string | null
  phase: string
  phasesEntered?: string[]
  round: number
  status: string | Tagged
  mode: string
  expansions: string[]
  debug?: boolean
  players: { id: number; investigator: string | null }[]
  leader?: number | null
  turn?: string | null
  questions: Record<string, Question>
  board: Board
  investigators: Record<string, Investigator>
  monsters: Record<string, Monster>
  assets: Record<string, Asset>
  decks: Decks
  test?: SkillTest | null
  encounter?: Encounter | null
  activeCard?: CardId | null
  revealedEvent?: CardId | null
  activeToken?: string | null
  drawnTokens: string[]
  cup: string[]
  codex: CodexEntry[]
  rumor?: { card: CardId; doom?: number } | null
  log: string[]
  sheetDoom: number
  sheetClues: number
  sheetMarkers: number
}
