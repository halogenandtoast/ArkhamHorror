import * as JsonDecoder from 'ts.data.json'
import { withDefault } from '@/arkham/parser'

// One row per user per achievement (entity format: {id, ...fields} with the
// "arkhamAchievement" prefix stripped). `earnedAt` is null while the row only
// tracks progress; `arkhamGameId` degrades to null when the game is deleted.
export type Achievement = {
  id: string
  userId: number | null
  achievement: string
  earnedAt: string | null
  arkhamGameId: string | null
  progress: unknown
}

export const achievementDecoder = JsonDecoder.object<Achievement>(
  {
    id: JsonDecoder.string(),
    userId: withDefault<number | null>(null, JsonDecoder.number()),
    achievement: JsonDecoder.string(),
    earnedAt: withDefault<string | null>(null, JsonDecoder.string()),
    arkhamGameId: withDefault<string | null>(null, JsonDecoder.string()),
    progress: JsonDecoder.succeed(),
  },
  'Achievement',
)
