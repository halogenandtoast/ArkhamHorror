import { readFile, writeFile } from 'node:fs/promises'
import { existsSync } from 'node:fs'
import { STATE_FILE } from './config.mjs'

// Tiny on-disk K/V store for the harness: auth token, most-recent game id, etc.
// Lives at frontend/harness/.state.json and is gitignored.

async function read() {
  if (!existsSync(STATE_FILE)) return {}
  try {
    return JSON.parse(await readFile(STATE_FILE, 'utf-8'))
  } catch {
    return {}
  }
}

async function write(obj) {
  await writeFile(STATE_FILE, JSON.stringify(obj, null, 2))
}

export async function getState(key) {
  const s = await read()
  return key ? s[key] : s
}

export async function setState(key, value) {
  const s = await read()
  s[key] = value
  await write(s)
}

export async function clearState() {
  await write({})
}
