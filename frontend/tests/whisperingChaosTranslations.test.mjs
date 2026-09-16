import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import test from 'node:test'

const cards = JSON.parse(
  readFileSync(new URL('../public/cards_zh-cn.json', import.meta.url), 'utf8'),
)

test('Simplified Chinese Whispering Chaos cards reference their matching tower twice', () => {
  const directions = new Map([
    ['06314', '北'],
    ['06315', '东'],
    ['06316', '南'],
    ['06317', '西'],
  ])

  for (const [code, direction] of directions) {
    const card = cards.find((candidate) => candidate.code === code)
    assert.ok(card, `missing card ${code}`)
    assert.equal(card.subname, direction)
    assert.deepEqual(
      card.text.match(/[北东南西]塔/g),
      [`${direction}塔`, `${direction}塔`],
      `${code} should only reference the ${direction} tower`,
    )
  }
})
