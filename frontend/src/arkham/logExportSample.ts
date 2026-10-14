import type { LogEntry, LogKind, LogPart, LogRef, LogRefKind } from '@/arkham/types/GameLog'

/* A few entries of a plausible log, for the export dialog to render as a sample
 * of each format.
 *
 * Built as real `LogEntry` values and run through the real renderers, so a
 * sample cannot drift from what the export actually produces -- and so it is
 * localized, which a block of pasted English would not be. The card codes are
 * genuine (Roland Banks, the Study, the Ghoul Priest), so the names come from
 * the card database in the reader's own language, exactly as they do in the log
 * panel.
 *
 * It covers what the formats differ on: a run of headings, a skill test as a
 * block with children, a chaos token and a skill icon (pictures in the panel,
 * words in a transcript), and an entry whose cause is carried as data.
 */

function ref(kind: LogRefKind, name: string, cardCode: string): LogRef {
  return { kind, name, cardCode, cardId: null, entityId: null, faceDown: false }
}

const roland = ref('RefInvestigator', 'Roland Banks', '01001')
const study = ref('RefLocation', 'Study', '01111')
const ghoulPriest = ref('RefEnemy', 'Ghoul Priest', '01116')

const i18n = (key: string, vars: Record<string, LogPart> = {}): LogPart => ({
  tag: 'LogI18n',
  contents: [key, vars],
})

const num = (n: number): LogPart => ({ tag: 'LogNumber', contents: n })
const who = (r: LogRef): LogPart => ({ tag: 'LogRefPart', contents: r })
const intellect: LogPart = { tag: 'LogIcon', contents: 'intellect' }

function entry(seq: number, kind: LogKind, body: LogPart[], over: Partial<LogEntry> = {}): LogEntry {
  return {
    seq,
    step: 1200 + seq,
    kind,
    body,
    source: null,
    children: [],
    audience: { tag: 'Everyone' },
    context: { round: 1, phase: 'Investigation', turn: null },
    tone: null,
    group: null,
    ...over,
  }
}

const TEST_GROUP = 'skillTest:sample'

export function sampleLogEntries(): LogEntry[] {
  return [
    entry(101, 'Structure', [i18n('log.round', { count: num(1) })]),
    entry(102, 'Structure', [i18n('log.phase.investigation')]),
    entry(103, 'Structure', [i18n('log.turn', { investigator: who(roland) })]),
    entry(
      104,
      'Test',
      [
        i18n('log.testOpens', {
          investigator: who(roland),
          skill: intellect,
          difficulty: num(2),
        }),
      ],
      { group: { id: TEST_GROUP, role: 'GroupHeader' } },
    ),
    entry(
      105,
      'Test',
      [i18n('log.testRevealed', { tokens: { tag: 'LogToken', contents: 'ElderSign' } })],
      { group: { id: TEST_GROUP, role: 'GroupMember' } },
    ),
    entry(
      106,
      'Test',
      [i18n('log.skillTestPassed', { investigator: who(roland), skill: intellect, by: num(3) })],
      { group: { id: TEST_GROUP, role: 'GroupSummary' }, tone: 'Good' },
    ),
    entry(107, 'Mechanic', [
      i18n('log.discoveredCluesAt', {
        investigator: who(roland),
        count: num(1),
        location: who(study),
      }),
    ]),
    entry(108, 'Mechanic', [i18n('log.takesDamage', { target: who(roland), count: num(1) })], {
      source: ghoulPriest,
      tone: 'Bad',
    }),
  ]
}
