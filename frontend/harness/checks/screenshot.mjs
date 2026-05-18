import { mkdir, writeFile, readFile, stat } from 'node:fs/promises'
import { existsSync } from 'node:fs'
import { createHash } from 'node:crypto'
import { dirname, resolve } from 'node:path'
import { BASELINE_DIR, CURRENT_DIR } from '../lib/config.mjs'

// Screenshot lifecycle
// --------------------
// The harness does not take screenshots directly — that's Chrome DevTools MCP's
// job, driven by the Claude session. The harness owns the *filesystem layout*:
//
//   snapshots/baseline/<name>.png   captured before a refactor (committed,
//                                   represents the known-good state)
//   snapshots/current/<name>.png    captured after a refactor (gitignored,
//                                   regenerated each run)
//
// `save(kind, name, base64)` writes the PNG payload returned by
// take_screenshot. `diff(name)` returns a structural comparison: byte length,
// sha256 match, and the two paths so the operator can eyeball them.
//
// A pixel-level diff (pixelmatch) is a follow-up — this MVP relies on hash
// equality + visual review.

export async function save(kind, name, base64) {
  if (kind !== 'baseline' && kind !== 'current') {
    throw new Error(`save: kind must be 'baseline' or 'current', got ${kind}`)
  }
  const dir = kind === 'baseline' ? BASELINE_DIR : CURRENT_DIR
  const path = resolve(dir, `${name}.png`)
  await mkdir(dirname(path), { recursive: true })
  const buf = Buffer.from(base64, 'base64')
  await writeFile(path, buf)
  return { path, bytes: buf.length, sha256: sha256(buf) }
}

function sha256(buf) {
  return createHash('sha256').update(buf).digest('hex')
}

export async function diff(name) {
  const a = resolve(BASELINE_DIR, `${name}.png`)
  const b = resolve(CURRENT_DIR, `${name}.png`)
  const aExists = existsSync(a)
  const bExists = existsSync(b)
  if (!aExists && !bExists) return { name, status: 'missing-both' }
  if (!aExists) return { name, status: 'missing-baseline', current: b }
  if (!bExists) return { name, status: 'missing-current', baseline: a }

  const [aBuf, bBuf] = await Promise.all([readFile(a), readFile(b)])
  const aHash = sha256(aBuf)
  const bHash = sha256(bBuf)
  if (aHash === bHash) {
    return { name, status: 'identical', baseline: a, current: b }
  }
  const [aStat, bStat] = await Promise.all([stat(a), stat(b)])
  return {
    name,
    status: 'differs',
    baseline: a,
    current: b,
    baselineBytes: aStat.size,
    currentBytes: bStat.size,
  }
}
