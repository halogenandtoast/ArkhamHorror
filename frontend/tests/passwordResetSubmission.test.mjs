import assert from 'node:assert/strict'
import { readFile } from 'node:fs/promises'
import test from 'node:test'
import ts from 'typescript'

const source = await readFile(new URL('../src/passwordResetSubmission.ts', import.meta.url), 'utf8')
const { outputText } = ts.transpileModule(source, {
  compilerOptions: { module: ts.ModuleKind.ES2022, target: ts.ScriptTarget.ES2022 },
})
const { submitPasswordReset } = await import(
  `data:text/javascript;base64,${Buffer.from(outputText).toString('base64')}`
)

test('marks the request submitted only after the API succeeds', async () => {
  const calls = []

  const result = await submitPasswordReset('Player@example.com', async (email) => {
    calls.push(email)
  })

  assert.equal(result, 'submitted')
  assert.deepEqual(calls, ['Player@example.com'])
})

test('reports API failure so the form can remain available for retry', async () => {
  const result = await submitPasswordReset('player@example.com', async () => {
    throw new Error('mail service unavailable')
  })

  assert.equal(result, 'failed')
})
