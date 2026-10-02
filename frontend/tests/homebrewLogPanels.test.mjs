import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createServer } from 'vite'

/* A homebrew campaign's recorded-set panel is matched on the campaign's i18n
scope plus the key's last segment, because the path a homebrew key formats to is
NOT reliably scoped: Circus Ex Mortis serializes its keys unscoped, so
`HomebrewCampaignLogKey "Destinies"` formats to `homebrewCampaignLog.key.destinies`.
Match on the formatted prefix instead and the Destinies panel silently never
renders — which is only visible in a browser, so this is that check. */
async function load(t, path) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return server.ssrLoadModule(path)
}

test('an unscoped homebrew key formats without its campaign scope', async (t) => {
  const { formatKey } = await load(t, '/src/arkham/types/Log.ts')
  assert.equal(
    formatKey({ tag: 'HomebrewCampaignLogKey', contents: 'Destinies' }),
    'homebrewCampaignLog.key.destinies',
  )
})

test('Circus Ex Mortis supplies a panel for its Destinies recorded set', async (t) => {
  const { homebrewLogPanel } = await load(t, '/src/arkham/homebrewLogPanels.ts')
  const key = 'homebrewCampaignLog.key.destinies'

  assert.ok(homebrewLogPanel('circusExMortis', key), 'destinies.vue was not discovered')
  // Scoped keys (how a campaign that namespaces its keys serializes) also match.
  assert.ok(homebrewLogPanel('circusExMortis', 'circusExMortis.key.destinies'))
  // An official campaign has no scope, and another campaign's same-named set is
  // not borrowed.
  assert.equal(homebrewLogPanel(undefined, key), undefined)
  assert.equal(homebrewLogPanel('darkMatter', key), undefined)
  assert.equal(homebrewLogPanel('circusExMortis', 'homebrewCampaignLog.key.nosuchset'), undefined)
})
