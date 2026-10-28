import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath } from 'node:url'
import { createServer } from 'vite'

test('the Ritual Circle ability-matching additional action decodes', async t => {
  const server = await createServer({ root: fileURLToPath(new URL('..', import.meta.url)), appType: 'custom', logLevel: 'silent', server: { middlewareMode: true, hmr: false } })
  t.after(() => server.close())
  const { additionalActionDecoder } = await server.ssrLoadModule('/src/arkham/types/Investigator.ts')
  const action = {
    label: '$agesUnwound.ritualCircleThePresentFractured.additionalAction',
    source: { tag: 'AbilitySource', contents: [{ tag: 'LocationSource', contents: 'ab0450f0-9313-419e-ae70-4083ba3f412b' }, 1] },
    kind: { tag: 'AbilityMatchingAdditionalAction', contents: { tag: 'AbilityMatches', contents: [
      { tag: 'AbilityOnLocation', contents: { tag: 'LocationWithId', contents: 'ab0450f0-9313-419e-ae70-4083ba3f412b' } },
      { tag: 'AbilityIsAction', contents: 'Investigate' },
    ] } },
  }
  assert.deepEqual(await additionalActionDecoder.decodeToPromise(action), { kind: { tag: 'AbilityMatchingAdditionalAction' } })
})
