import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createServer } from 'vite'

/* The campaign log's achievements tab shows only the mini-campaign in play. It
works that out by reading `campaignMode` straight out of `campaign.meta`, which
is typed `any` (Campaign.ts) — so nothing type-checks the read, and a mismatch
between the Haskell constructor names and the catalog's part keys silently
renders an empty list instead of failing. These tests are that missing check.

The meta shape asserted here is pinned backend-side by the spec
"campaignMode meta encoding" in TheDreamEatersSpec. */
async function loadAchievements(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return server.ssrLoadModule('/src/arkham/achievements.ts')
}

test('a partial campaign maps to the catalog part key it filters on', async (t) => {
  const { activeAchievementPart, achievementCatalog } = await loadAchievements(t)

  // The Haskell constructor names are NOT the catalog keys; the mapping is the
  // whole point of the helper.
  const web = activeAchievementPart({ tag: 'PartialMode', contents: 'TheWebOfDreams' })
  const dream = activeAchievementPart({ tag: 'PartialMode', contents: 'TheDreamQuest' })
  assert.equal(web, 'theWebOfDreams')
  assert.equal(dream, 'theDreamQuest')

  // ...and the keys it returns must actually select entries, which is what
  // regressed: a case mismatch left the achievements tab blank.
  const forPart = (part) =>
    achievementCatalog.filter((entry) => entry.campaignId === '06' && entry.part === part)
  assert.equal(forPart(web).length, 13)
  assert.equal(forPart(dream).length, 15)
})

test('campaigns sort with Return Tos first, then in release order', async (t) => {
  const { achievementCatalog, compareAchievementCampaignIds } = await loadAchievements(t)
  const campaignIds = [...new Set(achievementCatalog.map((entry) => entry.campaignId))]

  assert.deepEqual(
    campaignIds.sort(compareAchievementCampaignIds),
    ['50', '51', '52', '53', '54', '06', '07', '08', '09', '10', '11'],
  )
})

test('the full campaign and unknown shapes show every section', async (t) => {
  const { activeAchievementPart } = await loadAchievements(t)

  assert.equal(activeAchievementPart({ tag: 'FullMode' }), null)
  assert.equal(activeAchievementPart(undefined), null)
  assert.equal(activeAchievementPart(null), null)
  // A part the frontend does not know about must fall back to showing
  // everything rather than blanking the tab.
  assert.equal(activeAchievementPart({ tag: 'PartialMode', contents: 'SomeNewHalf' }), null)
  assert.equal(activeAchievementPart({ tag: 'PartialMode' }), null)
})
