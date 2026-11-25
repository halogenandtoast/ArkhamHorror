import assert from 'node:assert/strict'
import { resolve } from 'node:path'
import test from 'node:test'
import { createServer } from 'vite'

test('the draw shortcut finds the skill-test token draw without matching other choices', async (t) => {
  const server = await createServer({
    root: resolve('.'),
    server: { middlewareMode: true },
    appType: 'custom',
    logLevel: 'silent',
  })
  t.after(() => server.close())
  const { chaosTokenDrawChoiceIndex } = await server.ssrLoadModule(
    '/src/arkham/chaosTokenDrawChoice.ts',
  )

  assert.equal(
    chaosTokenDrawChoiceIndex([
      { tag: 'SkipTriggersButton' },
      { tag: 'StartSkillTestButton', investigatorId: 'investigator-1' },
    ]),
    1,
  )
  assert.equal(
    chaosTokenDrawChoiceIndex([{ tag: 'SkipTriggersButton' }, { tag: 'ChaosTokenLabel' }]),
    -1,
  )
})
