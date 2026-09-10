import * as JsonDecoder from 'ts.data.json'
import { type ChaosToken, type TokenFace } from '@/arkham/types/ChaosToken'

export type TokenBagMeta = {
  bagTokens?: ChaosToken[]
  bagSetAside?: ChaosToken[]
  bagCurrentToken?: ChaosToken | null
  bagCancelNext?: boolean
  bagDebugNext?: TokenFace | null
}

export type TokenBag = {
  tokens: ChaosToken[]
  setAside: ChaosToken[]
  currentToken: ChaosToken | null
  cancelNext: boolean
  debugNext: TokenFace | null
}

function token(value: unknown): ChaosToken | null {
  if (!value || typeof value !== 'object') return null
  const v = value as Record<string, unknown>
  const face = v.bagTokenFace ?? v.face ?? v.infestationTokenFace ?? v.predationTokenFace
  const id = v.bagTokenId ?? v.id ?? v.infestationTokenId ?? v.predationTokenId
  return typeof face === 'string' && typeof id === 'string' ? { id, face } : null
}

export const bagTokenDecoder: JsonDecoder.Decoder<ChaosToken> = JsonDecoder.succeed().flatMap((value: unknown) => {
  const parsed = token(value)
  return parsed ? JsonDecoder.constant(parsed) : JsonDecoder.fail('Expected a custom bag token')
})

// Also normalize old story metadata before the owner has processed a message.
export function readTokenBag(value: unknown): TokenBag | null {
  if (!value || typeof value !== 'object' || Array.isArray(value)) return null
  const meta = value as Record<string, unknown>
  const prefix = ['bag', 'infestation', 'predation'].find(p => Array.isArray(meta[`${p}Tokens`]))
  if (!prefix) return null
  const tokens = (key: string): ChaosToken[] => {
    const values = meta[`${prefix}${key}`]
    return Array.isArray(values) ? values.flatMap(v => token(v) ?? []) : []
  }
  return {
    tokens: tokens('Tokens'),
    setAside: tokens('SetAside'),
    currentToken: token(meta[`${prefix}CurrentToken`]),
    cancelNext: meta[`${prefix}CancelNext`] === true,
    debugNext: typeof meta.bagDebugNext === 'string' ? meta.bagDebugNext : null,
  }
}
