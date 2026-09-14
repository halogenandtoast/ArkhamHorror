import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createServer } from 'vite'
import { createI18n } from 'vue-i18n'

/* Icons are written into locale JSON as `{elderThing}`, but that is also
vue-i18n's syntax for a named interpolation — with no such parameter supplied it
renders as the empty string, and the placeholder is gone long before
`formatContent` could turn it into an icon span. That is how issue #5597 lost the
token from "Add 1 {elderThing} token to the chaos bag": the line rendered without
saying which token.

`loadLocaleMessages` now escapes the plain form into vue-i18n's literal form, so
both spellings survive. These tests pin that both still reach `formatContent`,
and that real named parameters are left alone. */
async function loadModules(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  const [messages, helpers, icons] = await Promise.all([
    server.ssrLoadModule('/src/locales/messages.ts'),
    server.ssrLoadModule('/src/arkham/helpers.ts'),
    server.ssrLoadModule('/src/arkham/icons.ts'),
  ])
  return { ...messages, ...helpers, ...icons }
}

function translator(messages) {
  return createI18n({ legacy: false, locale: 'en', warnHtmlMessage: false, messages: { en: messages } }).global.t
}

test('a plain icon placeholder survives translation and renders', async (t) => {
  const { escapeIconPlaceholders, formatContent } = await loadModules(t)

  const raw = { plain: escapeIconPlaceholders('Add 1 {elderThing} token to the chaos bag.') }
  const t9n = translator(raw)

  assert.equal(
    formatContent(t9n('plain')),
    'Add 1 <span class="elder-thing-icon"></span> token to the chaos bag.',
  )
})

test('the hand-escaped spelling renders identically', async (t) => {
  const { escapeIconPlaceholders, formatContent } = await loadModules(t)

  const messages = {
    plain: escapeIconPlaceholders('Add 1 {elderThing} token.'),
    escaped: escapeIconPlaceholders("Add 1 {'{'}elderThing{'}'} token."),
  }
  const t9n = translator(messages)

  assert.equal(formatContent(t9n('escaped')), formatContent(t9n('plain')))
  assert.equal(formatContent(t9n('escaped')), 'Add 1 <span class="elder-thing-icon"></span> token.')
})

test('an icon named by a parameter still renders', async (t) => {
  // base.json's shared `addToken`, driven by `withVars ["token" .= String "elderThing"]`.
  const { escapeIconPlaceholders, formatContent } = await loadModules(t)

  const t9n = translator({ addToken: escapeIconPlaceholders("Add 1 {'{'}{token}{'}'} chaos token.") })

  assert.equal(
    formatContent(t9n('addToken', { token: 'elderThing' })),
    'Add 1 <span class="elder-thing-icon"></span> chaos token.',
  )
})

test('homebrew icons and runes are escaped too', async (t) => {
  const { escapeIconPlaceholders, formatContent } = await loadModules(t)

  const t9n = translator({
    moon: escapeIconPlaceholders('Place 1 {moon}.'),
    rune: escapeIconPlaceholders('Resolve {runeA}.'),
  })

  assert.equal(formatContent(t9n('moon')), 'Place 1 <span class="moon-icon"></span>.')
  assert.equal(formatContent(t9n('rune')), 'Resolve <span class="rune-A"></span>.')
})

test('real named parameters are not escaped', async (t) => {
  const { escapeIconPlaceholders, formatContent } = await loadModules(t)

  const t9n = translator({
    xp: escapeIconPlaceholders('Experience: {xp}'),
    counted: escapeIconPlaceholders('Spend {count} clues'),
  })

  assert.equal(formatContent(t9n('xp', { xp: 3 })), 'Experience: 3')
  assert.equal(formatContent(t9n('counted', { count: 2 })), 'Spend 2 clues')
})

test('the Doom of Arkham Part II intro names the token it adds (#5597)', async (t) => {
  const { loadLocaleMessages, formatContent } = await loadModules(t)

  const { messages } = await loadLocaleMessages('en')
  const t9n = translator(messages)

  const rendered = formatContent(t9n('theDrownedCity.theDoomOfArkhamPartII.intro.addToken'))
  assert.match(rendered, /<span class="elder-thing-icon"><\/span>/)
})

test('a message vue-i18n cannot compile is left alone', async (t) => {
  // The Blob's Reality Acid table carries an inline <style> block, and CSS braces
  // are not placeholders — the compiler gives up and hands back the raw source,
  // where a plain `{autoFail}` already reaches formatContent. Escaping it there
  // would put the escape itself on screen.
  const { escapeIconPlaceholders } = await loadModules(t)

  const withStyle = "<style>.t{width:100%}</style><td>{autoFail}</td>"
  assert.equal(escapeIconPlaceholders(withStyle), withStyle)
})

test('every icon placeholder the locales ship renders as an icon', async (t) => {
  const { loadLocaleMessages, supportedLocales, iconPlaceholderPattern, formatContent } = await loadModules(t)

  for (const locale of supportedLocales) {
    const { messages } = await loadLocaleMessages(locale)
    const t9n = translator(messages)

    const unrendered = []
    const walk = (value, path) => {
      if (typeof value === 'string') {
        let rendered
        try {
          rendered = formatContent(t9n(path, { imgPath: '', setImgPath: '', xp: 0, count: 0, token: 'skull' }))
        } catch {
          return
        }
        if (iconPlaceholderPattern().test(rendered) || rendered.includes("{'{'}")) unrendered.push(path)
      } else if (Array.isArray(value)) {
        value.forEach((entry, i) => walk(entry, `${path}[${i}]`))
      } else if (value && typeof value === 'object') {
        for (const [key, entry] of Object.entries(value)) walk(entry, path ? `${path}.${key}` : key)
      }
    }
    walk(messages, '')

    assert.deepEqual(unrendered, [], `${locale} leaves placeholders on screen`)
  }
})
