import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'
import { createServer } from 'vite'

async function load(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return { server, ...await server.ssrLoadModule('/src/arkham/artVariants.ts') }
}

const cards = [
  { art: '01115', backArtVariants: { revised: '01615b' } },
  { art: '01145', backArtVariants: { revised: '01645b' } },
  { art: '01157', artVariants: { revised: '01657' } },
  { art: '01158', artVariants: { revised: '01658' } },
  { art: '01149', artVariants: { revised: '01649' }, backArtVariants: { revised: '01649b' } },
  { art: '01174', artVariants: { revised: '01674' } },
  { art: '01153', artVariants: { revised: '01653' } },
]

test('all revised faces resolve, without changing the other faces', async t => {
  const { registerArtVariants, variantArt, originalArt } = await load(t)
  registerArtVariants(cards)
  for (const [original, revised] of [
    ['01115b', '01615b'], ['01145b', '01645b'], ['01157', '01657'],
    ['01158', '01658'], ['01149', '01649'], ['01149b', '01649b'],
    ['01174', '01674'], ['01153', '01653'],
  ]) {
    assert.equal(variantArt(original, ['revised']), revised)
    assert.equal(originalArt(revised), original)
    assert.equal(variantArt(original, []), original)
    assert.equal(variantArt(original, ['unknown']), original)
  }
  for (const original of ['01115', '01145', '01153b', '01116']) {
    assert.equal(variantArt(original, ['revised']), original)
  }
})

test('preferences are ordered, fall through, and do not remap selected art', async t => {
  const { registerArtVariants, variantArt } = await load(t)
  registerArtVariants([{ art: 'test', artVariants: { revised: 'revision', foil: 'foil' } }])
  assert.equal(variantArt('test', ['missing', 'foil', 'revised']), 'foil')
  assert.equal(variantArt('test', ['revised', 'foil']), 'revision')
  assert.equal(variantArt('revision', ['revised']), 'revision')
})

test('late metadata updates existing reactive image computations', async t => {
  const { registerArtVariants, variantArt } = await load(t)
  const { computed } = await import('vue')
  const image = computed(() => variantArt('late', ['revised']))
  assert.equal(image.value, 'late')
  registerArtVariants([{ art: 'late', artVariants: { revised: 'late-revised' } }])
  assert.equal(image.value, 'late-revised')
  registerArtVariants([{ art: 'unrelated' }])
  assert.equal(image.value, 'late-revised')
})


test('the player preference persists and resolves both cardImage and raw Art paths', async t => {
  const storage = new Map()
  const previous = globalThis.localStorage
  globalThis.localStorage = {
    getItem: key => storage.get(key) ?? null,
    setItem: (key, value) => storage.set(key, value),
  }
  t.after(() => { globalThis.localStorage = previous })
  const { server, registerArtVariants } = await load(t)
  const { createPinia, setActivePinia } = await import('pinia')
  setActivePinia(createPinia())
  const { useSettings } = await server.ssrLoadModule('/src/stores/settings.ts')
  const { imgsrc } = await server.ssrLoadModule('/src/arkham/helpers.ts')
  const { cardImage } = await server.ssrLoadModule('/src/arkham/cardImages.ts')
  registerArtVariants(cards)
  const settings = useSettings()
  assert.deepEqual(settings.useVariants, [])
  assert.match(cardImage('01149'), /cards\/01149\.avif$/)
  settings.setUseVariants(['unknown', 'revised', 'revised'])
  assert.equal(storage.get('arkhamUseVariants'), '["unknown","revised"]')
  assert.match(cardImage('c01149', 'b'), /cards\/01649b\.avif$/)
  assert.match(imgsrc('cards/01145b.avif'), /cards\/01645b\.avif$/)
  assert.match(cardImage('01115'), /cards\/01115\.avif$/)
  setActivePinia(createPinia())
  assert.deepEqual(useSettings().useVariants, ['unknown', 'revised'])
  useSettings().setUseVariants([])
  assert.match(imgsrc('cards/01145b.avif'), /cards\/01145b\.avif$/)
  storage.set('arkhamUseVariants', 'not json')
  setActivePinia(createPinia())
  assert.deepEqual(useSettings().useVariants, [])
})
