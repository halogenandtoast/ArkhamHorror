import assert from 'node:assert/strict'
import { resolve } from 'node:path'
import test from 'node:test'
import { createServer } from 'vite'

test('card picker filters and sorts without changing answer indices', async (t) => {
  const server = await createServer({
    root: resolve('.'),
    server: { middlewareMode: true },
    appType: 'custom',
    logLevel: 'silent',
  })
  t.after(() => server.close())
  const { filterCardChoices } = await server.ssrLoadModule('/src/arkham/cardChoiceFilter.ts')
  const base = {
    subtitle: '',
    traits: ['Practiced'],
    type: 'skill',
    text: 'Discover one additional clue.',
    pack: 'Core',
    custom: false,
  }
  const cards = [
    { ...base, index: 17, code: '01039', name: 'Deduction', classes: ['seeker'], level: 0 },
    {
      ...base,
      index: 4,
      code: '*custom0',
      name: 'A custom deduction',
      classes: ['seeker', 'mystic'],
      level: 0,
      custom: true,
    },
    { ...base, index: 23, code: '02150', name: 'Deduction', classes: ['seeker'], level: 2 },
  ]
  const filters = {
    query: '',
    className: '',
    type: '',
    level: '',
    trait: '',
    pack: '',
    includeCustom: false,
    sort: 'name',
  }
  assert.deepEqual(
    filterCardChoices(cards, filters).map((c) => c.index),
    [17, 23],
  )
  assert.deepEqual(
    filterCardChoices(cards, { ...filters, includeCustom: true, level: '0' }).map((c) => c.index),
    [4, 17],
  )
  assert.deepEqual(
    filterCardChoices(cards, { ...filters, includeCustom: true, className: 'mystic' }).map(
      (c) => c.index,
    ),
    [4],
  )
  assert.deepEqual(
    filterCardChoices(cards, {
      ...filters,
      query: 'clue practiced',
      level: '0',
      type: 'skill',
      trait: 'Practiced',
      pack: 'Core',
    }).map((c) => c.index),
    [17],
  )
  assert.deepEqual(
    filterCardChoices(cards, { ...filters, query: '02150' }).map((c) => c.index),
    [23],
  )
  assert.deepEqual(filterCardChoices(cards, { ...filters, query: 'missing' }), [])
  assert.deepEqual(
    cards.map((c) => c.index),
    [17, 4, 23],
  )
})

test('only collection searches collapse the skill test', async (t) => {
  const server = await createServer({
    root: resolve('.'),
    server: { middlewareMode: true },
    appType: 'custom',
    logLevel: 'silent',
  })
  t.after(() => server.close())
  const { skillTestHasCollectionSearch } = await server.ssrLoadModule(
    '/src/arkham/skillTestPanel.ts',
  )
  const prompt = {
    tag: 'ChooseOne',
    choices: [{ tag: 'CardLabel', cardCode: 'c01039', flippable: false }],
  }
  const collection = {
    tag: 'QuestionLabel',
    label: '$questionFilter.collectionTitle',
    question: prompt,
  }
  const game = {
    skillTest: { step: 'ApplySkillTestResultsStep' },
    question: { player: collection },
  }
  assert.equal(skillTestHasCollectionSearch(game, 'player'), true)
  assert.equal(skillTestHasCollectionSearch(game, 'other'), false)
  assert.equal(skillTestHasCollectionSearch({ ...game, skillTest: null }, 'player'), false)
  assert.equal(
    skillTestHasCollectionSearch({ ...game, question: { player: prompt } }, 'player'),
    false,
  )
  assert.equal(
    skillTestHasCollectionSearch(
      { ...game, question: { player: { ...collection, label: '$another.effect' } } },
      'player',
    ),
    false,
  )
  assert.equal(
    skillTestHasCollectionSearch(
      { ...game, question: { player: { tag: 'QuestionWithSource', question: collection } } },
      'player',
    ),
    true,
  )
  assert.equal(
    skillTestHasCollectionSearch(
      { ...game, question: { player: { tag: 'PayCostQuestion', question: collection } } },
      'player',
    ),
    false,
  )
  for (const choices of [
    [],
    [{ tag: 'SkillTestApplyResultsButton' }],
    [{ tag: 'StartSkillTestButton' }],
    [{ tag: 'ChaosTokenLabel' }],
  ]) {
    assert.equal(
      skillTestHasCollectionSearch(
        { ...game, question: { player: { ...collection, question: { ...prompt, choices } } } },
        'player',
      ),
      false,
    )
  }
})
