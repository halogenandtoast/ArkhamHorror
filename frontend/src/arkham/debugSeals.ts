/* Debug sealing: the wire format for sealing a chaos token on a card and
 * releasing it again, shared by every debug window that shows sealed tokens.
 *
 * Sealing goes through the backend's `DebugSealChaosToken`, which looks the
 * token up by id among the bag's tokens -- so the picker only ever offers
 * tokens that are in the bag. Releasing is the plain `UnsealChaosToken` every
 * card runner already answers; "to the pool" chains a `ReturnChaosTokensToPool`
 * after it in one `Run`, so it is a single undo step. */
import { compareTokenFaces, tokenOrder, type ChaosToken, type TokenFace } from '@/arkham/types/ChaosToken'

export type SealableTarget = {
  tag:
    | 'AssetTarget'
    | 'EnemyTarget'
    | 'LocationTarget'
    | 'InvestigatorTarget'
    | 'EventTarget'
    | 'SkillTarget'
    | 'TreacheryTarget'
    | 'StoryTarget'
  contents: string
}

/* `UnsealChaosToken` and `ReturnChaosTokensToPool` take the whole record, and
 * `Eq ChaosToken` compares ids, so the other fields only have to be well formed. */
const tokenRecord = (token: ChaosToken) => ({
  chaosTokenId: token.id,
  chaosTokenFace: token.face,
  chaosTokenRevealedBy: null,
  chaosTokenCancelled: false,
  chaosTokenSealed: true,
})

export const unsealMessage = (token: ChaosToken) => ({
  tag: 'UnsealChaosToken',
  contents: tokenRecord(token),
})

/* Unsealing puts the token back in the bag; the pool return then takes it out
 * of the bag again, so a released bless or curse leaves play instead. */
export const releaseToPoolMessage = (token: ChaosToken) => ({
  tag: 'Run',
  contents: [
    unsealMessage(token),
    { tag: 'ChaosBagMessage', contents: { tag: 'ReturnChaosTokensToPool_', contents: [tokenRecord(token)] } },
  ],
})

export const sealMessage = (tokenId: string, target: SealableTarget) => ({
  tag: 'DebugSealChaosToken',
  contents: [tokenId, target],
})

const pooledOfficialFaces: TokenFace[] = ['BlessToken', 'CurseToken', 'FrostToken', 'BloodToken']

/* The faces that live in a token pool when they are out of the bag. Mirrors
 * `pooledChaosTokenFaces` (Arkham.Homebrew.Tokens): the four official pooled
 * faces plus every homebrew token, which is anything outside `tokenOrder`. */
export function isPooledFace(face: TokenFace): boolean {
  return pooledOfficialFaces.includes(face) || !(tokenOrder as readonly string[]).includes(face)
}

export type FaceGroup = { face: TokenFace, tokens: ChaosToken[] }

/* The bag as the picker shows it: one entry per face, in bag order, holding the
 * actual tokens so a click can seal a real one by id. */
export function groupByFace(tokens: ChaosToken[]): FaceGroup[] {
  const groups = new Map<TokenFace, ChaosToken[]>()
  for (const token of tokens) {
    const group = groups.get(token.face)
    if (group) group.push(token)
    else groups.set(token.face, [token])
  }
  return [...groups.entries()]
    .map(([face, grouped]) => ({ face, tokens: grouped }))
    .sort((a, b) => compareTokenFaces(a.face, b.face))
}
