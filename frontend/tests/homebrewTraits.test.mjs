import assert from 'node:assert/strict'
import test from 'node:test'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath, URL } from 'node:url'

/* Homebrew trait labels.
 *
 * A campaign supplies them as a top-level `trait` object in one of its locale
 * JSON files; `homebrewMessages` hoists those to the ROOT `trait` namespace,
 * because the engine's homebrew traits live in one flat universe and the UI
 * looks them up as `trait.<Name>` with no campaign in hand.
 *
 * These read the files directly rather than importing the loader, which needs
 * Vite's import.meta.glob. */

const root = fileURLToPath(new URL('..', import.meta.url))
const HOMEBREW = path.join(root, 'homebrew')
const TRAIT_HS_GLOB = path.join(
  root,
  '../backend/arkham-api/library/Arkham/Homebrew',
)

const localeFiles = (locale) => {
  if (!fs.existsSync(HOMEBREW)) return []
  return fs
    .readdirSync(HOMEBREW)
    .flatMap((camp) => {
      const dir = path.join(HOMEBREW, camp, 'locales', locale)
      if (!fs.existsSync(dir)) return []
      return fs
        .readdirSync(dir)
        .filter((f) => f.endsWith('.json'))
        .map((f) => ({ camp, file: path.join(dir, f) }))
    })
}

const traitBlocks = (locale) =>
  localeFiles(locale).flatMap(({ camp, file }) => {
    const data = JSON.parse(fs.readFileSync(file, 'utf8'))
    const trait = data.trait
    return trait && typeof trait === 'object' && !Array.isArray(trait)
      ? [{ camp, file, trait }]
      : []
  })

/* The declared trait universe, so a label cannot name a trait that does not
 * exist -- a typo there is silent, since the UI just falls back to the split. */
const declaredHomebrewTraits = () => {
  if (!fs.existsSync(TRAIT_HS_GLOB)) return null
  const names = new Set()
  for (const camp of fs.readdirSync(TRAIT_HS_GLOB)) {
    const f = path.join(TRAIT_HS_GLOB, camp, 'Traits.hs')
    if (!fs.existsSync(f)) continue
    const m = /declareHomebrewTraits\s*\[([\s\S]*?)\]/.exec(fs.readFileSync(f, 'utf8'))
    if (!m) continue
    for (const q of m[1].matchAll(/"([^"]+)"/g)) names.add(q[1])
  }
  return names
}

test('a homebrew trait label names a trait that actually exists', () => {
  const declared = declaredHomebrewTraits()
  if (!declared) return // backend not checked out

  for (const { camp, trait } of traitBlocks('en')) {
    for (const name of Object.keys(trait)) {
      assert.ok(
        declared.has(name),
        `${camp} labels "${name}", which no declareHomebrewTraits call declares`,
      )
    }
  }
})

test('two campaigns do not disagree about the same trait', () => {
  // Tatterdemalion and Victim are each declared by two campaigns, so the same
  // name can legitimately be labelled twice -- but not differently, since the
  // namespace is flat and whichever sorts last would silently win.
  for (const locale of ['en', 'ko', 'vi', 'zh-cn']) {
    const byName = new Map()
    for (const { camp, trait } of traitBlocks(locale)) {
      for (const [name, label] of Object.entries(trait)) {
        const prev = byName.get(name)
        assert.ok(
          prev === undefined || prev.label === label,
          `${locale}: ${prev?.camp} and ${camp} both label "${name}" but disagree ` +
            `(${JSON.stringify(prev?.label)} vs ${JSON.stringify(label)})`,
        )
        byName.set(name, { camp, label })
      }
    }
  }
})

test('the English override fixes a split the engine gets wrong', () => {
  const blocks = traitBlocks('en')
  const usher = blocks.find((b) => b.trait.HouseOfUsher)
  assert.ok(usher, 'expected the Usher campaign to label HouseOfUsher')
  // The camel split renders "House Of Usher"; the label is why this seam exists.
  assert.equal(usher.trait.HouseOfUsher, 'House of Usher')
})

test('hoisting trait labels cannot wipe the core trait map', async () => {
  // The merge order that matters: core first, homebrew on top, never a bare
  // replace. This mirrors homebrewMessages without needing import.meta.glob.
  const core = JSON.parse(
    fs.readFileSync(path.join(root, 'src/locales/en/traits.json'), 'utf8'),
  ).trait

  const merged = { ...core }
  for (const { trait } of traitBlocks('en')) Object.assign(merged, trait)

  assert.equal(merged.AncientOne, 'Ancient One', 'a core trait was lost')
  assert.equal(merged.HouseOfUsher, 'House of Usher', 'the homebrew label did not apply')
  assert.ok(Object.keys(merged).length >= Object.keys(core).length)
})
