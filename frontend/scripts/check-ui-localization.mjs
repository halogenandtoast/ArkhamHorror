import { readFileSync, readdirSync } from 'node:fs'
import { relative } from 'node:path'
import { fileURLToPath, URL } from 'node:url'
import { parse as parseSfc } from '@vue/compiler-sfc'
import { parse as parseTemplate } from '@vue/compiler-dom'
import ts from 'typescript'

const root = fileURLToPath(new URL('../src/', import.meta.url))
const displayAttribute = /(label|hint|help|description|caption|heading|legend|placeholder|title|aria-label|alt|text)$/i
const brands = new Set(['Arkham Horror: The Card Game™', 'Fantasy Flight Games (FFG)', 'Patreon', 'arkham.build', 'ArkhamDB'])
const codeExamples = new Set(['EnemyDamaged', 'TraitsLearned', 'sid', 'skillTestId', 'location', 'card', 'chosen', 'SomeField', 'Tome'])
const findings = []
let count = 0

function files(directory) {
  return readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = `${directory}/${entry.name}`
    return entry.isDirectory() ? files(path) : entry.name.endsWith('.vue') ? [path] : []
  })
}

function ignoredLiteral(node) {
  let child = node
  for (let parent = node.parent; parent; child = parent, parent = parent.parent) {
    // Function arguments include translation keys and code identifiers passed
    // to formatters. Comparisons and conditions are internal option values.
    if (ts.isCallExpression(parent)) return true
    if (ts.isConditionalExpression(parent) && child === parent.condition) return true
    if (ts.isPropertyAssignment(parent) && child === parent.name) return true
    if (ts.isBinaryExpression(parent) && [
      ts.SyntaxKind.EqualsEqualsToken, ts.SyntaxKind.EqualsEqualsEqualsToken,
      ts.SyntaxKind.ExclamationEqualsToken, ts.SyntaxKind.ExclamationEqualsEqualsToken,
    ].includes(parent.operatorToken.kind)) return true
  }
  return false
}

for (const file of files(root)) {
  const template = parseSfc(readFileSync(file, 'utf8')).descriptor.template
  if (!template) continue
  count++
  const path = relative(root, file).replaceAll('\\', '/')
  function report(text, line, parent, attribute = '') {
    text = text.replace(/\s+/g, ' ').trim()
    if (!/[A-Za-z]{3}/.test(text) || /\p{Script=Han}/u.test(text) || brands.has(text)) return
    if (parent === 'code' || parent === 'kbd') return
    if (path === 'components/SettingsForm.vue' && parent === 'option') return // Native language names.
    if (path.includes('/debug/')) {
      if (attribute === 'placeholder' && (/^\$[\w]+$/.test(text) || /^[a-f0-9-]{36}$/.test(text) || codeExamples.has(text))) return
      if (codeExamples.has(text) && (attribute || ['strong', 'em'].includes(parent))) return
      if (text === 'found' && parent === 'em') return // A named step block.
      if (text === 'skill' && parent === 'em') return // The engine target kind.
    }
    findings.push(`${path}:${line}: ${attribute ? `${attribute}: ` : ''}${text}`)
  }
  function expression(value, parent, attribute) {
    if (!value) return
    const ast = ts.createSourceFile('expression.ts', `(${value.content})`, ts.ScriptTarget.Latest, true)
    function walk(node) {
      if (!ignoredLiteral(node)) {
        const text = ts.isStringLiteral(node) || ts.isNoSubstitutionTemplateLiteral(node) ? node.text
          : ts.isTemplateExpression(node) ? node.head.text + node.templateSpans.map((span) => span.literal.text).join('') : null
        if (text) report(text, template.loc.start.line + value.loc.start.line - 1, parent, attribute)
      }
      ts.forEachChild(node, walk)
    }
    walk(ast)
  }
  function walk(node, parent) {
    const line = template.loc.start.line + node.loc.start.line - 1
    if (node.type === 2) report(node.content, line, parent?.tag)
    if (node.type === 6 && displayAttribute.test(node.name)) report(node.value?.content ?? '', line, parent?.tag, node.name)
    if (node.type === 5) expression(node.content, parent?.tag, '')
    if (node.type === 7 && (node.name === 'tooltip' || node.name === 'bind' && displayAttribute.test(node.arg?.content ?? ''))) {
      expression(node.exp, parent?.tag, node.arg?.content ?? node.name)
    }
    for (const child of node.children ?? []) walk(child, node)
    for (const property of node.props ?? []) walk(property, node)
  }
  walk(parseTemplate(template.content))
}

if (findings.length) {
  console.error(findings.join('\n'))
  process.exitCode = 1
} else {
  console.log(`Checked ${count} Vue templates: no untranslated interface literals (code examples and names preserved).`)
}
