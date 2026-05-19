#!/usr/bin/env node
// Harness CLI. Usage:
//   node harness/cli.mjs login                  # bootstrap auth, cache token
//   node harness/cli.mjs whoami
//   node harness/cli.mjs new-game <campaign|scenario> <id>
//   node harness/cli.mjs state                  # dump .state.json
//   node harness/cli.mjs raw <gameId> <json>    # send a raw debug Message
//   node harness/cli.mjs save-shot <kind> <name> <base64|@file>
//   node harness/cli.mjs diff [name]            # diff one or all snapshots
//   node harness/cli.mjs game-url               # print frontend URL for the
//                                                 current game (use in nav)
//
// All commands are designed to be called from a Claude session — the model
// drives the browser separately via Chrome DevTools MCP and uses this CLI to
// set up state and persist snapshot results.

import { ensureLogin } from './flows/login.mjs'
import { createGame, sendRaw } from './flows/newGame.mjs'
import { loadRandomDeck } from './flows/randomDeck.mjs'
import { api } from './lib/api.mjs'
import { FRONTEND_BASE } from './lib/config.mjs'
import { getState, clearState } from './lib/state.mjs'
import { save, diff } from './checks/screenshot.mjs'
import { readFile } from 'node:fs/promises'
import { existsSync, readdirSync } from 'node:fs'
import { resolve } from 'node:path'
import { BASELINE_DIR } from './lib/config.mjs'

const [, , cmd, ...args] = process.argv

function usage(msg) {
  if (msg) console.error(`error: ${msg}\n`)
  console.error('Usage: node harness/cli.mjs <command> [args]\n')
  console.error('Commands:')
  console.error('  login                                 bootstrap auth, cache token')
  console.error('  whoami                                print current user')
  console.error('  state                                 dump .state.json')
  console.error('  clear-state                           wipe .state.json')
  console.error('  load-deck                             load a random fixture deck for the harness user')
  console.error('  new-game scenario|campaign <id> [--deck]   create a game; --deck loads a random fixture deck first')
  console.error('  raw <gameId> <jsonMessage>            send a raw debug Message')
  console.error('  game-url [gameId]                     print frontend URL')
  console.error('  save-shot baseline|current <name> @<file>|<base64>')
  console.error('  diff [name]                           compare baseline vs current')
  process.exit(msg ? 1 : 0)
}

function out(v) {
  if (typeof v === 'string') console.log(v)
  else console.log(JSON.stringify(v, null, 2))
}

try {
  switch (cmd) {
    case undefined:
    case '-h':
    case '--help':
      usage()
      break

    case 'login': {
      const token = await ensureLogin({ force: args.includes('--force') })
      out({ ok: true, token: token.slice(0, 12) + '…' })
      break
    }

    case 'whoami': {
      await ensureLogin()
      const me = await api.whoami(await getState('token'))
      out(me)
      break
    }

    case 'state': {
      out(await getState())
      break
    }

    case 'clear-state': {
      await clearState()
      out({ ok: true })
      break
    }

    case 'new-game': {
      const [kind, id] = args
      const wantDeck = args.includes('--deck')
      if (!kind || !id) usage('new-game needs <scenario|campaign> <id>')
      await ensureLogin()
      const payload = kind === 'scenario' ? { scenarioId: id } : { campaignId: id }
      if (wantDeck) {
        const deck = await loadRandomDeck()
        payload.deckIds = [deck.id]
        payload.playerCount = 1
      }
      const game = await createGame(payload)
      out({
        id: game.id,
        url: `${FRONTEND_BASE}/#/games/${game.id}`,
        ...(wantDeck ? { deckId: payload.deckIds[0] } : {}),
      })
      break
    }

    case 'load-deck': {
      await ensureLogin()
      const deck = await loadRandomDeck()
      out({ id: deck.id, name: deck.name ?? deck.list?.name ?? '(unknown)' })
      break
    }

    case 'raw': {
      const [gameId, json] = args
      if (!gameId || !json) usage('raw needs <gameId> <jsonMessage>')
      await ensureLogin()
      await sendRaw(gameId, JSON.parse(json))
      out({ ok: true })
      break
    }

    case 'game-url': {
      const gameId = args[0] || await getState('gameId')
      if (!gameId) usage('no game id in state; pass one or run new-game first')
      out(`${FRONTEND_BASE}/#/games/${gameId}`)
      break
    }

    case 'save-shot': {
      const [kind, name, payload] = args
      if (!kind || !name || !payload) usage('save-shot needs <kind> <name> <base64|@file>')
      let base64
      if (payload.startsWith('@')) {
        base64 = (await readFile(payload.slice(1), 'utf-8')).trim()
      } else {
        base64 = payload
      }
      out(await save(kind, name, base64))
      break
    }

    case 'diff': {
      const [name] = args
      if (name) {
        out(await diff(name))
      } else {
        if (!existsSync(BASELINE_DIR)) {
          out({ ok: false, reason: 'no baselines yet' })
          break
        }
        const names = readdirSync(BASELINE_DIR)
          .filter((f) => f.endsWith('.png'))
          .map((f) => f.replace(/\.png$/, ''))
        const results = []
        for (const n of names) results.push(await diff(n))
        out(results)
      }
      break
    }

    default:
      usage(`unknown command: ${cmd}`)
  }
} catch (e) {
  console.error('error:', e.message)
  if (process.env.HARNESS_DEBUG) console.error(e.stack)
  process.exit(1)
}
