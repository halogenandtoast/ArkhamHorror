import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import test from 'node:test'

const library = path.resolve(import.meta.dirname, '../../backend/arkham-api/library/Arkham')
const scenarioSourceDirectories = [
  'Homebrew',
  'Act/Cards',
  'Agenda/Cards',
  'Campaign/Campaigns',
  'Location/Cards',
  'Scenario/Scenarios',
  'Scenarios',
  'Story/Cards',
]

const scenarioCardTypes = [
  ['Asset/Cards', 'Asset/Assets', ['storyAsset']],
  ['Enemy/CardDefs', 'Enemy/Cards', ['enemy']],
  ['Treachery/CardDefs', 'Treachery/Cards', ['treachery']],
]

function haskellFiles(directory) {
  return fs.readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const file = path.join(directory, entry.name)
    return entry.isDirectory() ? haskellFiles(file) : file.endsWith('.hs') ? [file] : []
  })
}

function lineNumber(source, index) {
  return source.slice(0, index).split('\n').length
}

function scenarioCardFiles(definitionDirectory, implementationDirectory, constructors) {
  const cardNames = new Set()

  for (const file of haskellFiles(path.join(library, definitionDirectory))) {
    const source = fs.readFileSync(file, 'utf8')
    const declarations = [...source.matchAll(/^([a-z][A-Za-z0-9_']*)\s*::\s*CardDef\s*$/gm)]
    declarations.forEach((declaration, index) => {
      const definition = source.slice(declaration.index, declarations[index + 1]?.index)
      if (constructors.some((constructor) => new RegExp(`\\b${constructor}\\b`).test(definition))) {
        cardNames.add(declaration[1])
      }
    })
  }

  return haskellFiles(path.join(library, implementationDirectory)).filter((file) => {
    const moduleName = path.basename(file, '.hs')
    const cardName = moduleName[0].toLowerCase() + moduleName.slice(1)
    return cardNames.has(cardName)
  })
}

test('scenario text uses locale keys instead of hardcoded English', () => {
  const hardcoded = []
  const patterns = [
    /\bft\s*\(?\s*"[A-Za-z][^"]*\s[^"]*"/g,
    /\bBasicEntry\s+"[A-Za-z][^"]*\s[^"]*"/g,
    /\bsetFlavorTitle\b[\s\S]{0,120}?"[A-Za-z][^"]*\s[^"]*"/g,
    /\bstoryWithContinue(?!')[\s\S]{0,250}?"(?!\$)[A-Za-z][^"]*\s[^"]*"/g,
    /(?<![.\w])(?:labeled|questionLabeled)(?!')\s*(?:\$\s*|\(\s*)?"(?!\$)[A-Za-z][^"]*"/g,
    /\bLabel\s*(?:\(\s*)?"(?!\$)[A-Za-z][^"]*"/g,
    /\bwithTooltip\s*(?:\$\s*)?"[^"]*[A-Za-z][^"]*"/g,
    /\bquestionLabel\b[\s\S]{0,120}?"(?!\$)[A-Za-z][^"]*\s[^"]*"/g,
    /\bchooseAmounts?(?!')\b\s+(?:\([^)]*\)|[A-Za-z_][\w'.]*)\s+"(?!\$)[A-Za-z][^"]*"/g,
    /\bchooseSome1?M(?!')\b\s+(?:\([^)]*\)|[A-Za-z_][\w'.]*)\s+"(?!\$)[^"]*[A-Za-z][^"]*"/g,
    /\bchooseUpToNM(?!')\b[\s\S]{0,100}?"(?!\$)[^"]*[A-Za-z][^"]*"/g,
    /\b(?:XPModifier|WithBonus)\s+"[A-Za-z][^"]*"/g,
    /\bAdditionalActions\s+"(?!\$)[^"]*[A-Za-z][^"]*"/g,
    /\bsend\s*(?:\$\s*)?"(?!\$)[^"]*[A-Za-z][^"]*"/g,
    /\bsend\s*\$\s*[^"\n]{1,100}"[^"]*\s[A-Za-z][^"]*"/g,
    /\bprompt\s+(?:\([^)]*\)|[A-Za-z_][\w'.]*)\s+"(?!\$)[^"]*[A-Za-z][^"]*"/g,
  ]

  const files = [
    ...scenarioSourceDirectories.flatMap((directory) => haskellFiles(path.join(library, directory))),
    ...scenarioCardTypes.flatMap(([definitions, implementations, constructors]) =>
      scenarioCardFiles(definitions, implementations, constructors),
    ),
  ]

  for (const file of files) {
    const source = fs.readFileSync(file, 'utf8')
    for (const pattern of patterns) {
      for (const match of source.matchAll(pattern)) {
        hardcoded.push(`${path.relative(library, file)}:${lineNumber(source, match.index)}`)
      }
    }
  }

  assert.deepEqual([...new Set(hardcoded)].sort(), [])
})
