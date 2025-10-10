import type { Difficulty } from '@/arkham/types/Difficulty'

export interface Scenario {
  id: string
  name: string
  returnTo?: string
  returnToName?: string
  beta?: boolean
  alpha?: boolean
  standaloneDifficulties?: Difficulty[]
  show?: boolean
  campaign?: string
}

export interface Campaign {
  id: string
  name: string
  beta?: boolean
  alpha?: boolean
  dev?: boolean
  settings?: string[]
  returnTo?: {
    id: string
    name: string
    beta?: boolean
    alpha?: boolean
  }
}
