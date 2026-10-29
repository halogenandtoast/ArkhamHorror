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

test('collection searches use a separate modal during and outside skill tests', async (t) => {
  const server = await createServer({
    root: resolve('.'),
    server: { middlewareMode: true },
    appType: 'custom',
    logLevel: 'silent',
  })
  t.after(() => server.close())
  const { isCollectionSearch } = await server.ssrLoadModule(
    '/src/arkham/collectionSearch.ts',
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
  assert.equal(isCollectionSearch(game, 'player'), true)
  assert.equal(isCollectionSearch(game, 'other'), false)
  assert.equal(isCollectionSearch({ ...game, skillTest: null }, 'player'), true)
  assert.equal(
    isCollectionSearch({ ...game, question: { player: prompt } }, 'player'),
    false,
  )
  assert.equal(
    isCollectionSearch(
      { ...game, question: { player: { ...collection, label: '$another.effect' } } },
      'player',
    ),
    false,
  )
  assert.equal(
    isCollectionSearch(
      { ...game, question: { player: { tag: 'QuestionWithSource', question: collection } } },
      'player',
    ),
    true,
  )
  assert.equal(
    isCollectionSearch(
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
      isCollectionSearch(
        { ...game, question: { player: { ...collection, question: { ...prompt, choices } } } },
        'player',
      ),
      false,
    )
  }
})

test('compact collection questions decode into indexed card choices and retain source highlighting', async (t) => {
  const server = await createServer({
    root: resolve('.'),
    server: { middlewareMode: true },
    appType: 'custom',
    logLevel: 'silent',
  })
  t.after(() => server.close())
  const { questionDecoder } = await server.ssrLoadModule('/src/arkham/types/Question.ts')
  const { choices, choicesSource } = await server.ssrLoadModule('/src/arkham/types/Game.ts')
  const { isCollectionSearch } = await server.ssrLoadModule(
    '/src/arkham/collectionSearch.ts',
  )
  const source = { tag: 'LocationSource', contents: '71278af9-d064-48b6-801b-7a83d8599be5' }
  const question = await questionDecoder.decodePromise({
    tag: 'ChooseFromCollection',
    investigatorId: 'c01002',
    source,
    cardCodes: ['c01039', 'c*custom0', 'c01039'],
  })
  const game = { question: { player: question }, skillTest: { step: 'ApplySkillTestResultsStep' } }
  assert.deepEqual(
    choices(game, 'player').map((c) => c.cardCode),
    ['c01039', 'c*custom0', 'c01039'],
  )
  assert.ok(choices(game, 'player').every((c) => c.tag === 'CardLabel' && !c.flippable))
  assert.deepEqual(choicesSource(game, 'player'), question.source)
  assert.equal(isCollectionSearch(game, 'player'), true)
  assert.equal(
    isCollectionSearch(
      { ...game, question: { player: { ...question, cardCodes: [] } } },
      'player',
    ),
    false,
  )
  const oldQuestion = await questionDecoder.decodePromise({
    tag: 'ChooseOne',
    choices: [{ tag: 'CardLabel', cardCode: 'c01039', flippable: false }],
  })
  assert.equal(
    choices({ ...game, question: { player: oldQuestion } }, 'player')[0].cardCode,
    'c01039',
  )
})
