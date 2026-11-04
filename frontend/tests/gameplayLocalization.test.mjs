import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { resolve, dirname } from 'node:path'
import test from 'node:test'
import ts from 'typescript'
import { createI18n } from 'vue-i18n'

async function loadHelper(file) {
  const source = readFileSync(resolve('src/arkham', file), 'utf8')
  const { outputText } = ts.transpileModule(source, {
    compilerOptions: { module: ts.ModuleKind.ES2022, target: ts.ScriptTarget.ES2022 },
  })
  return import(`data:text/javascript;base64,${Buffer.from(outputText).toString('base64')}`)
}

const names = JSON.parse(readFileSync('src/locales/zh-cn/scenarioNames.json', 'utf8'))
const campaigns = JSON.parse(readFileSync('src/arkham/data/campaigns.json', 'utf8'))
const campaignNames = JSON.parse(readFileSync('src/locales/zh-cn/campaignNames.json', 'utf8'))
const composer = (locale) => createI18n({
  legacy: false,
  locale,
  fallbackLocale: 'en',
  messages: {
    en: {
      label: JSON.parse(readFileSync('src/locales/en/label.json', 'utf8')),
      create: JSON.parse(readFileSync('src/locales/en/gameBoard/create.json', 'utf8')),
    },
    'zh-cn': {
      scenarioNames: names,
      campaignNames,
      label: JSON.parse(readFileSync('src/locales/zh-cn/label.json', 'utf8')),
      create: JSON.parse(readFileSync('src/locales/zh-cn/gameBoard/create.json', 'utf8')),
    },
  },
}).global

test('scenario titles use Simplified Chinese for both metadata and engine ids', async () => {
  const { localizedScenarioName } = await loadHelper('scenarioNames.ts')
  const zh = composer('zh-cn')
  for (const id of ['07041', 'c07041']) {
    assert.equal(localizedScenarioName(id, 'The Pit of Despair', zh.t, zh.te), '绝望深坑')
  }
  assert.equal(localizedScenarioName('51012', 'Return to Extracurricular Activities', zh.t, zh.te), '重返课外活动')
  assert.notEqual(localizedScenarioName('08501a', '', zh.t, zh.te), localizedScenarioName('08501b', '', zh.t, zh.te))
  assert.equal(localizedScenarioName(':unknown:001', 'My Custom Scenario', zh.t, zh.te), 'My Custom Scenario')
  const en = composer('en')
  assert.equal(localizedScenarioName('c07041', 'The Pit of Despair', en.t, en.te), 'The Pit of Despair')
})

test('home page translates saved default titles and preserves custom game names', async () => {
  const { localizedGameName } = await loadHelper('scenarioNames.ts')
  const zh = composer('zh-cn')
  for (const campaign of campaigns) {
    const game = { name: campaign.name, campaign: { id: campaign.id }, scenario: null }
    assert.equal(localizedGameName(game, campaigns, zh.t, zh.te), campaignNames[campaign.id])
    assert.match(campaignNames[campaign.id], /\p{Script=Han}/u)
    if (campaign.returnTo) {
      game.name = `Return to ${campaign.name}`
      game.campaign.id = campaign.returnTo.id
      assert.equal(localizedGameName(game, campaigns, zh.t, zh.te), campaignNames[campaign.returnTo.id])
      assert.match(campaignNames[campaign.returnTo.id], /^重返/)
    }
  }
  const standalone = {
    name: 'The Pit of Despair', campaign: null,
    scenario: { id: 'c07041', name: { title: 'The Pit of Despair' } },
  }
  assert.equal(localizedGameName(standalone, campaigns, zh.t, zh.te), '绝望深坑')
  for (const name of ['Friday Night', 'My The Innsmouth Conspiracy', 'The Dunwich Legacy', '自定义游戏']) {
    const game = { name, campaign: { id: '07' }, scenario: null }
    assert.equal(localizedGameName(game, campaigns, zh.t, zh.te), name)
  }
  const en = composer('en')
  const original = { name: 'The Innsmouth Conspiracy', campaign: { id: '07' }, scenario: null }
  assert.equal(localizedGameName(original, campaigns, en.t, en.te), original.name)
  assert.equal(localizedGameName({ ...original, campaign: { id: '7' } }, campaigns, zh.t, zh.te), '印斯茅斯暗潮')
})

test('home page difficulty badges have Chinese labels for every level', () => {
  const zh = composer('zh-cn')
  for (const [level, title] of [['Easy', '简单'], ['Standard', '普通'], ['Hard', '困难'], ['Expert', '专家']]) {
    assert.equal(zh.t(`create.${level}`), title)
  }
})

test('every official scenario, nested side story and Return to variant has a Chinese title', () => {
  const catalog = resolve('src/arkham/data/scenarios.ts')
  const source = readFileSync(catalog, 'utf8')
  const files = [...source.matchAll(/from '@\/arkham\/data\/([^']+\.json)'/g)]
  const missing = []
  for (const [, file] of files) {
    const entries = JSON.parse(readFileSync(resolve(dirname(catalog), file), 'utf8'))
    for (const entry of entries) {
      const variants = [entry, ...(entry.scenarios ?? []), ...(entry.returnTo ? [{ id: entry.returnTo }] : [])]
      for (const variant of variants) {
        if (typeof variant.id !== 'string') continue
        if (!/\p{Script=Han}/u.test(names[variant.id] ?? '')) missing.push(`${file}:${variant.id}`)
      }
    }
  }
  assert.deepEqual(missing, [])
})

test('damage and horror allocation labels translate the exact remaining amounts', async () => {
  const { localizedAssignmentLabel } = await loadHelper('assignmentLabels.ts')
  const zh = composer('zh-cn')
  const en = composer('en')
  for (const [source, translated] of [
    ['Assign 1 damage', '分配1点伤害'],
    ['Assign 12 damage', '分配12点伤害'],
    ['Assign 0 damage', '分配0点伤害'],
    ['Assign 2 horror', '分配2点恐惧'],
    ['Assign 3 damage and 11 horror', '分配3点伤害和11点恐惧'],
  ]) {
    assert.equal(localizedAssignmentLabel(source, zh.t), translated)
    assert.equal(localizedAssignmentLabel(source, en.t), source)
  }
  for (const other of ['Choose a card', 'Assign 2 damage to an enemy', '$label.takeDamage count=i:2']) {
    assert.equal(localizedAssignmentLabel(other, zh.t), null)
  }
})
