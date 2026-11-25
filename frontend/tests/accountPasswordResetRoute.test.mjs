import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath } from 'node:url'
import { createServer } from 'vite'

test('account password reset is authenticated without changing the public reset route', async (t) => {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())

  const { default: routes } = await server.ssrLoadModule('/src/routes/index.ts')
  const accountResetRoute = routes.find(({ name }) => name === 'AccountPasswordReset')
  const publicResetRoute = routes.find(({ name }) => name === 'PasswordReset')

  assert.equal(accountResetRoute.path, '/account/password-reset')
  assert.equal(accountResetRoute.meta.requiresAuth, true)
  assert.equal(accountResetRoute.meta.guest, undefined)
  assert.equal(publicResetRoute.path, '/password-reset')
  assert.equal(publicResetRoute.meta.guest, true)
})
