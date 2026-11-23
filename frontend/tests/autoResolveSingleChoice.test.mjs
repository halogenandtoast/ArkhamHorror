import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath } from 'node:url'
import { createServer } from 'vite'

async function loadChoiceIndex(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return (await server.ssrLoadModule('/src/arkham/autoResolveSingleChoice.ts'))
    .onlyActionableChoiceIndex
}

test('onlyActionableChoiceIndex resolves one action through display wrappers', async (t) => {
  const onlyActionableChoiceIndex = await loadChoiceIndex(t)
  const question = {
    tag: 'QuestionWithSource',
    question: {
      tag: 'QuestionLabel',
      question: {
        tag: 'ChooseOne',
        choices: [
          { tag: 'Info' },
          { tag: 'Label', label: 'Enter the only location' },
          { tag: 'InvalidLabel', label: 'Unavailable action' },
        ],
      },
    },
  }

  assert.equal(onlyActionableChoiceIndex(question), 1)
})

test('onlyActionableChoiceIndex leaves ambiguous or non-action questions alone', async (t) => {
  const onlyActionableChoiceIndex = await loadChoiceIndex(t)

  assert.equal(
    onlyActionableChoiceIndex({
      tag: 'ChooseOne',
      choices: [{ tag: 'Label' }, { tag: 'Done' }],
    }),
    null,
  )
  assert.equal(
    onlyActionableChoiceIndex({
      tag: 'ChooseOne',
      choices: [{ tag: 'Info' }, { tag: 'InvalidLabel' }],
    }),
    null,
  )
  assert.equal(
    onlyActionableChoiceIndex({
      tag: 'ChooseOneAtATime',
      choices: [{ tag: 'Label' }],
    }),
    null,
  )
  assert.equal(onlyActionableChoiceIndex(null), null)
})
