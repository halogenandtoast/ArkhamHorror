import assert from 'node:assert/strict'
import test from 'node:test'

import cardDataFiles from '../scripts/card-data-files.cjs'

const { isCardDataFilename } = cardDataFiles

test('card data filenames support a language-region suffix', () => {
  assert.equal(isCardDataFilename('cards_zh-cn.json'), true)
  assert.equal(isCardDataFilename('cards_zh.json'), true)
})

test('card data filenames reject unrelated and unsafe paths', () => {
  assert.equal(isCardDataFilename('cards_zh-cn.json.tmp'), false)
  assert.equal(isCardDataFilename('../cards_zh-cn.json'), false)
})
