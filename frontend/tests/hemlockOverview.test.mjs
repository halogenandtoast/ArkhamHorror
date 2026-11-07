import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { test } from 'node:test'
import ts from 'typescript'

const source = readFileSync(new URL('../src/arkham/hemlock-overview.ts', import.meta.url), 'utf8')
const js = ts.transpile(source, { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ES2022 })
const { hemlockOverview, hemlockVillagePosition, hemlockHousePosition } = await import('data:text/javascript;base64,' + Buffer.from(js).toString('base64'))

const scenarios = {
  '10501': 'written-in-rock-descent',
  '10523': 'hemlock-house',
  '10549': 'silent-heath',
  '10569': 'lost-sister',
  '10588': 'thing-in-the-depths',
  '10605': 'twisted-hollow',
  '10626': 'longest-night',
  '10651': 'fate-of-the-vale',
  '10704': 'vale-day',
  '10677a': 'vale-day',
  '10679a': 'vale-day',
  '10679b': 'vale-night',
}

function expectWebp(overview, art) {
  const expected = `img/scenes/hemlock-vale/${art}.webp`
  assert.equal(overview?.image, expected)
  const bytes = readFileSync(new URL(`../public/${overview.image}`, import.meta.url))
  assert.equal(bytes.toString('ascii', 8, 12), 'WEBP')
}

test('every playable Hemlock Vale board selects a shipped overview', () => {
  for (const [id, art] of Object.entries(scenarios)) {
    expectWebp(hemlockOverview('c' + id, 'c' + id), art)
  }
  assert.equal(hemlockOverview('c01104', 'c01104'), undefined)
})

test('Written in Rock selects the rail board after the cave-in on either reference printing', () => {
  for (const reference of ['c10501', 'c10502']) {
    assert.equal(hemlockOverview('c10501', reference).fixedPassages, true)
    const maze = hemlockOverview('c10501', reference + 'b')
    assert.equal(maze.fixedPassages, false)
    expectWebp(maze, 'written-in-rock-maze')
  }
})

test('Fate of the Vale changes backdrop only when the game lays out the nighttime village', () => {
  expectWebp(hemlockOverview('c10651', 'c10651', ['nest1 theAbyss nest2']), 'fate-of-the-vale')
  expectWebp(hemlockOverview('c10651', 'c10651', ['moon  diamond  star']), 'vale-night')
})

test('the village anchors cover all eight landmarks on both printed faces', () => {
  const anchors = new Set()
  for (let code = 10705; code <= 10712; code += 1) {
    const position = hemlockVillagePosition(`c${code}a`)
    assert.ok(position)
    assert.deepEqual(hemlockVillagePosition(`${code}b`), position)
    anchors.add(position.join(','))
  }
  assert.equal(anchors.size, 8)
  assert.deepEqual(hemlockVillagePosition('c10690a'), hemlockVillagePosition('c10706a'))
  assert.deepEqual(hemlockVillagePosition('c10691b'), hemlockVillagePosition('c10708b'))
  assert.equal(hemlockVillagePosition('c10543'), undefined)
})

test('the house keeps the foyer and attic on their painted floors as the board changes', () => {
  assert.deepEqual(hemlockHousePosition('pos0000'), [50, 75])
  assert.deepEqual(hemlockHousePosition('pos0003'), [50, 13])
  assert.deepEqual(hemlockHousePosition('posn0102'), [18, 31])
  assert.deepEqual(hemlockHousePosition('pos00n01'), [50, 92])
  assert.equal(hemlockHousePosition('posn0103'), undefined)
})

test('Night of the Zealot scenarios select their overview boards', () => {
  assert.equal(hemlockOverview('c01120', 'c01120')?.image, 'img/scenes/night-of-the-zealot/midnight-masks.webp')
  assert.equal(hemlockOverview('c01142', 'c01142')?.image, 'img/scenes/night-of-the-zealot/devourer-below.webp')
})

test('Dunwich Legacy scenarios select their overview boards', () => {
  assert.equal(hemlockOverview('c02041', 'c02041')?.image, 'img/scenes/the-dunwich-legacy/extracurricular-activity.webp')
  assert.equal(hemlockOverview('c02062', 'c02062')?.image, 'img/scenes/the-dunwich-legacy/the-house-always-wins.webp')
  assert.equal(hemlockOverview('c02118', 'c02118')?.image, 'img/scenes/the-dunwich-legacy/the-miskatonic-museum.webp')
  assert.equal(hemlockOverview('c02159', 'c02159')?.image, 'img/scenes/the-dunwich-legacy/the-essex-county-express.webp')
  assert.equal(hemlockOverview('c02195', 'c02195')?.image, 'img/scenes/the-dunwich-legacy/blood-on-the-altar.webp')
  assert.equal(hemlockOverview('c02236', 'c02236')?.image, 'img/scenes/the-dunwich-legacy/undimensioned-and-unseen.webp')
  assert.equal(hemlockOverview('c02274', 'c02274')?.image, 'img/scenes/the-dunwich-legacy/where-doom-awaits.webp')
  assert.equal(hemlockOverview('c02311', 'c02311')?.image, 'img/scenes/the-dunwich-legacy/lost-in-time-and-space.webp')
})

test('The Scarlet Keys scenarios select their overview boards', () => {
  assert.equal(hemlockOverview('c09501', 'c09501')?.image, 'img/scenes/the-scarlet-keys/riddles-and-rain.webp')
  assert.equal(hemlockOverview('c09520', 'c09520')?.image, 'img/scenes/the-scarlet-keys/dead-heat.webp')
  assert.equal(hemlockOverview('c09545', 'c09545')?.image, 'img/scenes/the-scarlet-keys/sanguine-shadows.webp')
  assert.equal(hemlockOverview('c09566', 'c09566')?.image, 'img/scenes/the-scarlet-keys/dealings-in-the-dark.webp')
  assert.equal(hemlockOverview('c09591', 'c09591')?.image, 'img/scenes/the-scarlet-keys/dancing-mad.webp')
  assert.equal(hemlockOverview('c09609', 'c09609')?.image, 'img/scenes/the-scarlet-keys/on-thin-ice.webp')
  assert.equal(hemlockOverview('c09635', 'c09635')?.image, 'img/scenes/the-scarlet-keys/dogs-of-war.webp')
  assert.equal(hemlockOverview('c09660', 'c09660')?.image, 'img/scenes/the-scarlet-keys/shades-of-suffering.webp')
  assert.equal(hemlockOverview('c09681', 'c09681')?.image, 'img/scenes/the-scarlet-keys/without-a-trace.webp')
  assert.equal(hemlockOverview('c09694', 'c09694')?.image, 'img/scenes/the-scarlet-keys/congress-of-the-keys.webp')
})

test('The Drowned City scenarios select their overview boards', () => {
  assert.equal(hemlockOverview('c11501', 'c11501')?.image, 'img/scenes/the-drowned-city/one-last-job.webp')
  assert.equal(hemlockOverview('c11517', 'c11517')?.image, 'img/scenes/the-drowned-city/the-western-wall.webp')
  assert.equal(hemlockOverview('c11536', 'c11536')?.image, 'img/scenes/the-drowned-city/the-drowned-quarter.webp')
  assert.equal(hemlockOverview('c11553', 'c11553')?.image, 'img/scenes/the-drowned-city/the-apiary.webp')
  assert.equal(hemlockOverview('c11587', 'c11587')?.image, 'img/scenes/the-drowned-city/the-grand-vault.webp')
  assert.equal(hemlockOverview('c11612', 'c11612')?.image, 'img/scenes/the-drowned-city/court-of-the-ancients.webp')
  assert.equal(hemlockOverview('c11639', 'c11639')?.image, 'img/scenes/the-drowned-city/obsidian-canyons.webp')
  assert.equal(hemlockOverview('c11673', 'c11673')?.image, 'img/scenes/the-drowned-city/sepulchre-of-the-sleeper.webp')
  assert.equal(hemlockOverview('c11682', 'c11682')?.image, 'img/scenes/the-drowned-city/the-doom-of-arkham-part-1.webp')
  assert.equal(hemlockOverview('c11688a', 'c11688a')?.image, 'img/scenes/the-drowned-city/the-doom-of-arkham-part-2.webp')
  assert.equal(hemlockOverview('c11688b', 'c11688b')?.image, 'img/scenes/the-drowned-city/the-doom-of-arkham-part-2.webp')
})
