import assert from 'node:assert/strict'
import { readdirSync, readFileSync } from 'node:fs'
import { test } from 'node:test'
import ts from 'typescript'

const source = readFileSync(new URL('../src/arkham/sceneSprites.ts', import.meta.url), 'utf8')
const cardImages = readFileSync(new URL('../src/arkham/cardImages.ts', import.meta.url), 'utf8')
const cardArt = cardImages.match(/export function cardArt\([\s\S]*?\n}/)?.[0]
assert.ok(cardArt)
const js = ts.transpile(source.replace(/^import .*$/m, cardArt), {
  target: ts.ScriptTarget.ES2022,
  module: ts.ModuleKind.ES2022,
})
const { spriteFor } = await import('data:text/javascript;base64,' + Buffer.from(js).toString('base64'))

test('generated sprites resolve by exact card code and load from public assets', () => {
  const actors = new URL('../public/img/scenes/actors/', import.meta.url)
  const files = readdirSync(actors).filter((file) => /^\d{5}[a-z]?\.webp$/.test(file))
  assert.ok(files.includes('01002.webp'))
  for (const file of files) {
    const code = file.slice(0, -'.webp'.length)
    const path = spriteFor(`c${code}`)
    assert.equal(path, `img/scenes/actors/${code}.webp`)
    const asset = readFileSync(new URL(`../public/${path}`, import.meta.url))
    assert.equal(asset.toString('ascii', 8, 12), 'WEBP')
  }
})

test('an unregistered card or back falls through to its own card art', () => {
  for (const code of ['01002b', 'xpolyp', 'constructor']) {
    assert.equal(spriteFor(code), null)
  }
})

test('Hunting Nightgaunt and Screeching Byakhee do not share a sprite', () => {
  assert.ok(spriteFor('01172'))
  assert.equal(spriteFor('01175'), 'img/scenes/actors/01175.webp')
  assert.notEqual(spriteFor('01175'), spriteFor('01172'))
})
