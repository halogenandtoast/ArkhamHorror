import assert from 'node:assert/strict'
import { resolve } from 'node:path'
import test from 'node:test'
import { createServer } from 'vite'

test('only the actual location owns an investigator movement transition name', async (t) => {
  const server = await createServer({ root: resolve('.'), server: { middlewareMode: true }, appType: 'custom', logLevel: 'silent' })
  t.after(() => server.close())
  const { investigatorMovementName } = await server.ssrLoadModule('/src/arkham/investigatorMovement.ts')
  const investigator = { id: 'c01002', placement: { tag: 'AtLocation', contents: 'study' } }
  assert.deepEqual(
    ['study', 'as-if-location'].map(id => investigatorMovementName(investigator, id)),
    ['investigator-c01002', undefined],
  )
  const moved = { ...investigator, placement: { tag: 'AtLocation', contents: 'hall' } }
  assert.equal(investigatorMovementName(moved, 'study'), undefined)
  assert.equal(investigatorMovementName(moved, 'hall'), 'investigator-c01002')
  assert.equal(investigatorMovementName({ ...investigator, placement: { tag: 'InVehicle', contents: 'car' } }, 'study'), undefined)
})
