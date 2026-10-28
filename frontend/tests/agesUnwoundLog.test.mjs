import assert from 'node:assert/strict'
import test from 'node:test'
import { readFile } from 'node:fs/promises'
import { createServer } from 'vite'
import { fileURLToPath } from 'node:url'

async function load(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return server.ssrLoadModule('/homebrew/ages-unwound/logRecords.ts')
}
test('the editor covers every Ages Unwound record exactly once', async (t) => {
  const { recordRows, countKeys, investigatorKeys, timedKeys } = await load(t)
  const source = await readFile(
    new URL('../../backend/arkham-api/library/Arkham/Homebrew/AgesUnwound/Key.hs', import.meta.url),
    'utf8',
  )
  const constructors = source
    .split('data AgesUnwoundKey')[1]
    .split('deriving stock')[0]
    .replace(/\{-[\s\S]*?-\}/g, '')
    .replace(/--[^\n]*/g, '')
  const expected = [...constructors.matchAll(/[=|]\s*([A-Z]\w+)/g)].map((match) => match[1]).sort()
  const actual = [
    ...recordRows.flatMap((row) => row.keys),
    ...countKeys,
    ...investigatorKeys,
    'RecordedTimes',
    'DisappearedUnexpectedly',
  ].sort()
  assert.deepEqual(actual, expected)
  assert.equal(new Set(actual).size, actual.length)
  const helperCalls = await Promise.all(
    [
      'Acts/TheBoundary_052.hs',
      'Acts/BigAndUgly_053.hs',
      'Acts/DoorwayToTheUnknown_054.hs',
      'Acts/BreakingTheCircle.hs',
      'Stories/Backfire.hs',
      'Scenarios/AWorldTornDown.hs',
    ].map((path) =>
      readFile(
        new URL(
          `../../backend/arkham-api/library/Arkham/Homebrew/AgesUnwound/${path}`,
          import.meta.url,
        ),
        'utf8',
      ),
    ),
  )
  const recordedTimes = new Set(
    helperCalls.flatMap((source) =>
      [...source.matchAll(/recordTheTimeFor\s+([A-Z]\w+)/g)].map((match) => match[1]),
    ),
  )
  assert.deepEqual([...recordedTimes].sort(), [...timedKeys].sort())
})
test('exclusive outcomes clear the alternative and its time before recording the choice', async (t) => {
  const { recordRows, recordChoiceMessages, recordKey } = await load(t)
  for (const row of recordRows.filter((row) => row.keys.length > 1)) {
    const choice = row.keys[1]
    const messages = recordChoiceMessages(
      row.keys,
      choice,
      'recorded',
      (key) => [{ tag: 'ForgetRecord', contents: recordKey(key) }],
      (key) => [{ tag: 'ClearTime', contents: key }],
    )
    assert.deepEqual(messages.at(-1), { tag: 'Record', contents: recordKey(choice) })
    assert.equal(messages.filter((message) => message.tag === 'Record').length, 1)
    assert.deepEqual(
      messages.filter((message) => message.tag === 'ClearTime').map((message) => message.contents),
      row.keys.filter((key) => key !== choice),
    )
  }
})
test('unrecording clears all times; crossing out preserves the chosen time', async (t) => {
  const { recordChoiceMessages } = await load(t)
  const forget = (key) => [{ tag: 'Forget', key }]
  const clear = (key) => [{ tag: 'ClearTime', key }]
  assert.deepEqual(recordChoiceMessages(['A', 'B'], 'A', 'none', forget, clear), [
    ...forget('A'),
    ...clear('A'),
    ...forget('B'),
    ...clear('B'),
  ])
  const crossed = recordChoiceMessages(['A', 'B'], 'A', 'crossedOut', forget, clear)
  assert.equal(crossed.at(-1).tag, 'CrossOutRecord')
  assert.deepEqual(
    crossed.filter((message) => message.tag === 'ClearTime'),
    clear('B'),
  )
})

test('school entrance and time destination must describe the same route', async (t) => {
  const { validateRecordKeys } = await load(t)
  const front = 'TheInvestigatorsUsedTheSchoolsFrontDoor'
  const back = 'TheInvestigatorsSnuckIntoTheBackOfTheSchool'
  const past = 'TheInvestigatorsSteppedIntoThePast'
  const future = 'TheInvestigatorsSteppedIntoTheFuture'
  assert.deepEqual(validateRecordKeys([front, past]), [])
  assert.deepEqual(validateRecordKeys([back, future]), [])
  assert.deepEqual(
    validateRecordKeys([back, past]).map((issue) => issue.reason),
    ['backPast'],
  )
  assert.deepEqual(
    validateRecordKeys([front, future]).map((issue) => issue.reason),
    ['frontFuture'],
  )
  assert.deepEqual(validateRecordKeys([]), [])
  assert.deepEqual(validateRecordKeys([back]), [])
})

test('the reported edited history highlights every conflicting record', async (t) => {
  const { validateRecordKeys } = await load(t)
  const recorded = [
    'TheInvestigatorsSnuckIntoTheBackOfTheSchool',
    'TheInvestigatorsSteppedIntoThePast',
    'TheInvestigatorsPutDownTheHoundOfUnmaking',
  ]
  const issues = validateRecordKeys(recorded)
  assert.deepEqual(issues.map((issue) => issue.reason).sort(), ['backHound', 'backPast'])
  assert.deepEqual([...new Set(issues.flatMap((issue) => issue.keys))].sort(), [...recorded].sort())
  assert.deepEqual(
    validateRecordKeys([
      'TheInvestigatorsSnuckIntoTheBackOfTheSchool',
      'TheInvestigatorsSteppedIntoTheFuture',
      'TheInvestigatorsDispelledTheWard',
    ]),
    [],
  )
})

test('Hound and ward outcomes are validated without banning later Backfire', async (t) => {
  const { validateRecordKeys } = await load(t)
  const hound = 'TheInvestigatorsPutDownTheHoundOfUnmaking'
  const ward = 'TheInvestigatorsDispelledTheWard'
  assert.ok(validateRecordKeys([hound, ward]).some((issue) => issue.reason === 'houndWard'))
  assert.deepEqual(
    validateRecordKeys([
      'TheInvestigatorsUsedTheSchoolsFrontDoor',
      'TheInvestigatorsSteppedIntoThePast',
      hound,
      'TheInvestigatorsUnleashedChaos',
    ]),
    [],
  )
})

test('all exclusive groups are checked, including preexisting invalid saves', async (t) => {
  const { validateRecordKeys, recordRows } = await load(t)
  for (const row of recordRows.filter((row) => row.keys.length > 1)) {
    assert.ok(validateRecordKeys(row.keys).some((issue) => issue.reason === 'exclusive'))
    assert.deepEqual(validateRecordKeys([row.keys[0]]), [])
  }
})

test('legacy keys normalize and crossed-out records do not invalidate the active history', async (t) => {
  const { activeRecordKeys, validateRecordKeys, recordKey } = await load(t)
  const log = {
    recorded: [
      {
        ...recordKey('TheInvestigatorsSnuckIntoTheBackOfTheSchool'),
        raw: recordKey('TheInvestigatorsSnuckIntoTheBackOfTheSchool'),
      },
      {
        tag: 'HomebrewCampaignLogKey',
        raw: { tag: 'HomebrewCampaignLogKey', contents: 'TheInvestigatorsSteppedIntoTheFuture' },
      },
    ],
    crossedOut: [{ ...recordKey('TheInvestigatorsSteppedIntoThePast') }],
  }
  assert.deepEqual(validateRecordKeys(activeRecordKeys(log)), [])
})

test('the editor compiles through the Vue template compiler', async () => {
  const { parse, compileTemplate } = await import('@vue/compiler-sfc')
  const filename = fileURLToPath(new URL('../homebrew/ages-unwound/log-editor.vue', import.meta.url))
  const source = await readFile(filename, 'utf8')
  const { descriptor, errors } = parse(source, { filename })
  assert.deepEqual(errors, [])
  const template = compileTemplate({ source: descriptor.template.content, filename, id: 'ages-log-editor' })
  assert.deepEqual(template.errors, [])
})
