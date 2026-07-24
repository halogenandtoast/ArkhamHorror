import assert from 'node:assert/strict'
import { existsSync, readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import test from 'node:test'

const englishCatalogPath = resolve('src/locales/en/gameBoard/ultimatumsAndBoons.json')
const chineseCatalogPath = resolve('src/locales/zh/gameBoard/ultimatumsAndBoons.json')
const chineseGameBoardPath = resolve('src/locales/zh/gameBoard/gameBoard.ts')
const gameOptionsPath = resolve('src/arkham/components/NewCampaign/GameOptions.vue')
const chineseLabelPath = resolve('src/locales/zh/label.json')

function leafEntries(value, prefix = '') {
  if (typeof value === 'string') return [[prefix, value]]

  return Object.entries(value).flatMap(([key, child]) =>
    leafEntries(child, prefix ? `${prefix}.${key}` : key),
  )
}

function placeholders(value) {
  return [...value.matchAll(/\{([A-Za-z0-9_]+)\}/g)].map((match) => match[1]).sort()
}

test('Chinese game creation translates every Ultimatum and Boon option', () => {
  assert.equal(
    existsSync(chineseCatalogPath),
    true,
    'missing Chinese Ultimatums and Boons catalog',
  )

  const englishCatalog = JSON.parse(readFileSync(englishCatalogPath, 'utf8'))
  const chineseCatalog = JSON.parse(readFileSync(chineseCatalogPath, 'utf8'))
  const englishLeaves = new Map(leafEntries(englishCatalog))
  const chineseLeaves = new Map(leafEntries(chineseCatalog))

  assert.deepEqual(
    [...chineseLeaves.keys()].sort(),
    [...englishLeaves.keys()].sort(),
    'Chinese catalog must cover the same messages as English',
  )

  const gameOptions = readFileSync(gameOptionsPath, 'utf8')
  const optionTags = [
    ...new Set(
      [...gameOptions.matchAll(/'(BoonOf[A-Za-z]+|UltimatumOf[A-Za-z]+)'/g)].map(
        (match) => match[1],
      ),
    ),
  ].sort()

  assert.deepEqual(
    Object.keys(chineseCatalog.entries).sort(),
    optionTags,
    'every option rendered by GameOptions must have a Chinese entry',
  )

  for (const [path, englishValue] of englishLeaves) {
    const chineseValue = chineseLeaves.get(path)
    assert.equal(typeof chineseValue, 'string', `${path} must be a string`)
    assert.notEqual(chineseValue, englishValue, `${path} still falls back to English`)
    assert.match(chineseValue, /[\u3400-\u9fff]/, `${path} must contain Chinese text`)
    assert.deepEqual(
      placeholders(chineseValue),
      placeholders(englishValue),
      `${path} must preserve interpolation placeholders`,
    )
  }
})

test('Chinese game board bundles the Ultimatums and Boons catalog', () => {
  const source = readFileSync(chineseGameBoardPath, 'utf8')

  assert.match(
    source,
    /import ultimatumsAndBoons from '@\/locales\/zh\/gameBoard\/ultimatumsAndBoons\.json'/,
  )
  assert.match(source, /export default \{[^}]*\bultimatumsAndBoons\b[^}]*\}/s)
})

test('Chinese game creation translates the Morrigan weakness choice label', () => {
  const labels = JSON.parse(readFileSync(chineseLabelPath, 'utf8'))

  assert.equal(
    labels.ultimatumsAndBoons?.returnWeakness,
    '选择一张要放回收藏的弱点',
  )
})
