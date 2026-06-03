import { fileURLToPath } from 'node:url'
import { dirname, resolve } from 'node:path'

const here = dirname(fileURLToPath(import.meta.url))

export const HARNESS_ROOT = resolve(here, '..')
export const STATE_FILE = resolve(HARNESS_ROOT, '.state.json')
export const SNAPSHOTS_DIR = resolve(HARNESS_ROOT, 'snapshots')
export const BASELINE_DIR = resolve(SNAPSHOTS_DIR, 'baseline')
export const CURRENT_DIR = resolve(SNAPSHOTS_DIR, 'current')

export const API_BASE = process.env.HARNESS_API_BASE || 'http://localhost:3002/api/v1'
export const FRONTEND_BASE = process.env.HARNESS_FRONTEND_BASE || 'http://localhost:8080'

// Fixed harness user. Created on first run via /register, then reused.
export const HARNESS_USER = {
  username: process.env.HARNESS_USER || 'harness',
  email: process.env.HARNESS_EMAIL || 'harness@example.com',
  password: process.env.HARNESS_PASSWORD || 'harness-test-pw-12345',
}
