import { computed, inject, nextTick, provide, ref, shallowRef, type InjectionKey } from 'vue'
import * as api from '@/api'
import { img } from '@/assets'
import { readPref, writePref } from '@/prefs'
import { user } from '@/session'
import type { Catalog, CardId, Game, Monster, Tagged, TableView } from '@/types'
import { drawRects, eventRects, flyCard, type DrawRects, type EventRects } from '@/game/fly'
import { announcePhase, flash, viewTransition } from '@/game/overlays'
import { DECK_KEYS, NEIGHBOURHOOD_KEY, archiveImage, cssName, show, slug } from '@/game/util'

// initial: first load; quiet: a refetch after reconnecting; live: a move by anyone
export type ApplyMode = 'initial' | 'quiet' | 'live'

export function createGameContext(tableId: string, catalog: Catalog) {
  const tv = shallowRef<TableView | null>(null)
  const view = computed(() => tv.value?.view ?? null)
  const game = computed<Game | null>(() => view.value?.game ?? null)
  const error = ref('')

  // ---- who is who -----------------------------------------------------------
  const me = computed(() => user.value?.username ?? null)
  const mySeats = computed(
    () => new Set((tv.value?.seats ?? []).filter((s) => me.value && s.username === me.value).map((s) => s.player)),
  )
  const seated = computed(() => mySeats.value.size > 0)
  const isHost = computed(() => !!me.value && tv.value?.hostName === me.value)
  const isMine = (pid: number | string) => mySeats.value.has(Number(pid))
  const usernameOf = (pid: number | string) => tv.value?.seats.find((s) => s.player === Number(pid))?.username ?? null
  const playerOfInv = (iid: string) => game.value?.players.find((p) => p.investigator === iid)?.id
  const invOfPlayer = (pid: number | string) =>
    game.value?.players.find((p) => String(p.id) === String(pid))?.investigator ?? null
  const myInvestigators = computed(() =>
    (game.value?.players ?? []).filter((p) => p.investigator && isMine(p.id)).map((p) => p.investigator!),
  )

  // ---- names and images -----------------------------------------------------
  const cardNameRaw = (cid: CardId | string) => view.value?.cardNames[cid] as string | undefined
  const cardName = (cid: CardId | string) => cardNameRaw(cid) ?? `#${cid}`
  const invName = (iid: string) => catalog.investigatorNames[iid] ?? iid
  const spaceName = (sid: string) => game.value?.board.spaces[sid]?.name ?? sid
  const cardCode = (cid: CardId | string) => view.value?.cardCodes?.[cid] ?? slug(cardNameRaw(cid) ?? cid)
  /* A handful of cards sit in the archive rather than in a deck -- Feast of
  Umordhoth's cards 13 to 19, which the codex deals out -- so their art is the
  archive's, numbered, not the card deck's. */
  const archiveArt = (code: string, flipped: boolean) => {
    const m = /^feast-(\d{1,2})$/.exec(code)
    return m ? archiveImage(+m[1], flipped) : null
  }
  const cardFace = (cid: CardId, flipped: boolean) => {
    const code = cardCode(cid)
    return archiveArt(code, flipped) ?? img(`cards/${code}${flipped ? 'b' : ''}.webp`)
  }
  const initials = (iid: string) =>
    invName(iid)
      .replace(/"/g, '')
      .split(' ')
      .map((w) => w[0])
      .join('')
  const scenarioName = (code: string) => catalog.scenarios.find((sc) => sc.code === code)?.name ?? code
  // each scenario's event art lives in its own folder, keyed by the code's prefix
  const EVENT_ART: Record<string, string> = {
    aoa: 'approach-of-azathoth',
    feast: 'feast-of-umordhoth',
  }
  const eventImage = (cid: CardId | null | undefined) => {
    if (cid == null) return null
    const m = /^([a-z]+)-event-(\d{2})$/.exec(view.value?.cardCodes?.[cid] ?? '')
    const dir = m ? EVENT_ART[m[1]] : undefined
    return dir ? img(`events/${dir}/${m![2]}.avif`) : null
  }
  const encounterImage = (cid: CardId) => {
    const m = /^(.+)-(\d{2})$/.exec(view.value?.cardCodes?.[cid] ?? '')
    return m ? img(`encounters/${m[1]}/${m[2]}.avif`) : null
  }
  /* the archive check comes before the encounter one, whose pattern would other-
  wise read "feast-15" as card 15 of a "feast" encounter set */
  const activeCardImage = (cid: CardId) =>
    eventImage(cid) ?? archiveArt(cardCode(cid), false) ?? encounterImage(cid) ?? cardFace(cid, false)
  // 428.2: an engaged monster sits in the play area of the investigator it is engaged with;
  // a massive one stays in its space (451.3)
  const inPlayerArea = (m: Monster) => m.state?.tag === 'Engaged' && !(view.value?.massive ?? []).includes(m.card)

  const sourceLabelText = (s: Tagged | null | undefined): string => {
    switch (s?.tag) {
      case 'SourceScenario':
        return 'Scenario sheet'
      case 'SourceCodex':
        return `Codex #${s.contents}`
      case 'SourceInvestigator':
        return invName(s.contents)
      case 'SourceCard':
      case 'SourceHeadline':
      case 'SourceEncounter':
      case 'SourceMonster':
        return cardNameRaw(s.contents) ?? `Card ${s.contents}`
      case 'SourceMythos':
        return 'Mythos'
      case 'SourceRules':
        return 'Rules'
      default:
        return show(s)
    }
  }

  // ---- highlighting ---------------------------------------------------------
  // which map piece a choice refers to, so hovering it can point the piece out
  const sourceTarget = (s: Tagged | null | undefined): string | null => {
    switch (s?.tag) {
      case 'SourceScenario':
        return 'sheet:scenario'
      case 'SourceCodex':
        return `codex:${s.contents}`
      case 'SourceInvestigator':
        return `inv:${cssName(s.contents)}`
      case 'SourceCard':
      case 'SourceHeadline':
      case 'SourceEncounter':
      case 'SourceMonster':
        return `card:${s.contents}`
      default:
        return null
    }
  }
  const labelTarget = (l: Tagged): string | null => {
    const c = l.contents
    switch (l.tag) {
      case 'MonsterLabel':
      case 'CardLabel':
        return `mon:${c}`
      case 'SpaceLabel':
        return `space:${cssName(c)}`
      case 'InvestigatorLabel':
        return `inv:${cssName(c)}`
      case 'SourceLabel':
        return sourceTarget(c)
      default:
        return null
    }
  }
  const highlighted = ref<string | null>(null)
  // a reckoning resolves one source at a time; mark every source still waiting
  const reckonings = computed(() => {
    const out = new Set<string>()
    for (const q of Object.values(game.value?.questions ?? {}))
      for (const c of q.choices ?? []) {
        if (c.label?.tag !== 'SourceLabel') continue
        const t = sourceTarget(c.label.contents)
        if (t) out.add(t)
      }
    return out
  })
  // class bindings for a piece carrying these data-* identities
  type Mark = [string, string | number | null | undefined]
  const marks = (...pairs: Mark[]) => {
    const keys = pairs.filter(([, id]) => id != null).map(([k, id]) => `${k}:${id}`)
    return {
      hl: highlighted.value !== null && keys.includes(highlighted.value),
      'reckoning-pending': keys.some((k) => reckonings.value.has(k)),
    }
  }

  // ---- UI prefs ---------------------------------------------------------------
  const selectedTab = ref<string | null>(readPref('ah3e-selected-tab'))
  const selectTab = (iid: string) => {
    selectedTab.value = iid
    writePref('ah3e-selected-tab', iid)
  }
  const debugMode = ref(readPref('ah3e-debug') === '1')
  const toggleDebug = (on = !debugMode.value) => {
    debugMode.value = on
    writePref('ah3e-debug', on ? '1' : '0')
  }
  // the server refuses debug actions unless the table was opened with it, and to anyone not seated
  const debugAllowed = computed(() => !!tv.value?.options.debug && seated.value)
  const dbgOn = computed(() => debugMode.value && debugAllowed.value && !!game.value)
  const dbgIid = () => {
    const invs = game.value?.investigators ?? {}
    return selectedTab.value && invs[selectedTab.value] ? selectedTab.value : Object.keys(invs)[0]
  }

  const logOpen = ref(readPref('ah3e-log-open') === '1')
  const logSeen = ref(0)
  const toggleLog = (open = !logOpen.value) => {
    logOpen.value = open
    writePref('ah3e-log-open', open ? '1' : '0')
    if (open && game.value) logSeen.value = game.value.log.length
  }
  const logUnread = computed(() => (logOpen.value ? 0 : Math.max(0, (game.value?.log.length ?? 0) - logSeen.value)))

  // while an event card is flying to the discard, the unstable space it names stays hidden until it lands
  const unstableHeld = ref(false)
  const unstableReveal = ref(false)

  // ---- applying table views -------------------------------------------------
  let lastEvents: { discard: CardId[]; revealed: CardId | null | undefined } | null = null
  let lastDrawGame: Game | null = null
  let undoPending = false

  function deckHolding(g: Game, cid: CardId): string | null {
    const d = g.decks
    for (const [nid, cards] of Object.entries(d.neighborhoods ?? {}))
      if (cards.includes(cid)) return `${NEIGHBOURHOOD_KEY}${nid}`
    return DECK_KEYS.find((k) => d[k]?.includes(cid)) ?? null
  }

  // a card reaching the event discard travels there; one that was never shown turns face up on the way
  function flyDiscardedEvent(g: Game, before: EventRects, fly: boolean): CardId | null {
    const discard = g.decks.eventDiscard,
      prev = lastEvents
    lastEvents = { discard, revealed: g.revealedEvent }
    if (!fly || !prev || discard.length <= prev.discard.length || discard[0] === prev.discard[0]) return null
    const cid = discard[0],
      face = eventImage(cid),
      nid = view.value?.eventNeighborhoods?.[cid]
    const shown = prev.revealed === cid && before.shown
    const from = shown || before.deck
    const target = document.querySelector<HTMLElement>('[data-deck="eventDiscard"] .deck-stack > img')
    if (!face || !from || !target) return null
    const under = prev.discard.length ? eventImage(prev.discard[0]) : null
    unstableHeld.value = true
    unstableReveal.value = false
    flyCard(from, target, face, shown ? null : img(`backs/${nid}.webp`), 0, under).finally(() => {
      unstableHeld.value = false
      unstableReveal.value = true
    })
    return cid
  }

  // Cards drawn from a deck fly out of it: anything newly on screen that sat in a
  // deck a moment ago starts on that deck's top card and turns over on the way.
  function flyDrawnCards(g: Game, before: DrawRects, skip: CardId | null) {
    const prev = lastDrawGame
    lastDrawGame = g
    if (!prev) return
    let n = 0
    const seen = new Set<string>()
    document.querySelectorAll<HTMLElement>('[data-card]').forEach((el) => {
      const cid = el.dataset.card!
      if (before.shown.has(cid) || seen.has(cid) || String(skip) === cid) return
      seen.add(cid)
      const holder = deckHolding(prev, +cid)
      const deck = holder ? before.decks[holder] : undefined
      const im = el.tagName === 'IMG' ? el : el.querySelector('img')
      if (!deck || !im || !im.getBoundingClientRect().width) return
      void flyCard(deck.rect, im, im.getAttribute('src') ?? '', deck.back ?? im.getAttribute('src'), 120 * n++)
    })
  }

  async function doApply(next: TableView, mode: ApplyMode) {
    const cur = tv.value
    // keep whichever copy is newest; the socket and our own replies both deliver it
    if (cur && next.version <= cur.version) return
    const live = mode === 'live'
    // an undo puts cards back; flying them out of a deck would show the opposite of what happened,
    // and its restored state still lists the phases entered back then
    const animate = live && !undoPending && next.cause !== 'undo'
    const run = async () => {
      const drawBefore = drawRects()
      const eventBefore = eventRects()
      if (!unstableHeld.value) unstableReveal.value = false
      tv.value = next
      await nextTick()
      const g = next.view?.game
      if (!g) {
        lastEvents = null
        lastDrawGame = null
        return
      }
      const discarded = flyDiscardedEvent(g, eventBefore, live)
      if (animate) flyDrawnCards(g, drawBefore, discarded)
      else lastDrawGame = g
    }
    // cards and tokens tween between their old and new positions
    const hadGame = !!cur?.view
    if (document.startViewTransition && hadGame && mode !== 'initial') {
      try {
        const t = document.startViewTransition(run)
        viewTransition.value = t
        await t.updateCallbackDone.catch(() => {})
      } catch {
        await run()
      }
    } else await run()
    if (animate) (next.view?.game?.phasesEntered ?? []).forEach(announcePhase)
  }

  // updates apply one at a time, so each is compared against the one before it
  let chain: Promise<void> = Promise.resolve()
  const apply = (next: TableView, mode: ApplyMode) => {
    chain = chain.then(() => doApply(next, mode)).catch((e) => console.error(e))
    return chain
  }

  // ---- actions ----------------------------------------------------------------
  let answering = false
  async function choose(pid: number | string, choice: number) {
    if (!isMine(pid) || answering) return
    answering = true
    try {
      await apply(await api.answer(tableId, Number(pid), choice, tv.value?.version ?? 0), 'live')
      error.value = ''
    } catch (e) {
      error.value = api.errorText(e)
    } finally {
      answering = false
    }
  }
  // a one-argument constructor carries its value bare; the rest take a positional array
  async function debugAction(tag: string, contents?: unknown) {
    if (!debugAllowed.value) return
    try {
      await apply(await api.debug(tableId, contents === undefined ? { tag } : { tag, contents }), 'live')
      error.value = ''
    } catch (e) {
      error.value = api.errorText(e)
    }
  }
  let undoing = false
  async function undo() {
    if (undoing || !tv.value?.canUndo || !seated.value) return
    undoing = true
    undoPending = true
    try {
      await apply(await api.undo(tableId), 'live')
      error.value = ''
      flash('Undone')
    } catch (e) {
      error.value = api.errorText(e)
    } finally {
      undoing = false
      undoPending = false
    }
  }

  // ---- drag to move -------------------------------------------------------
  // The engine only ever offers the next single step, so dragging works out the
  // path itself and answers one step at a time. Paid steps are just another
  // offered choice, so the money is spent by the engine as we walk.
  interface MoveCtx {
    pid: number
    ms: { investigator: string; remaining: number; paidSteps: number; maxPaidSteps: number }
    iid: string
    paid: boolean
  }
  function moveContext(g: Game): MoveCtx | null {
    for (const [pid, q] of Object.entries(g.questions)) {
      if (!isMine(pid)) continue
      for (const c of q.choices) {
        const mv = (c.messages ?? []).find((m) => m.tag === 'MoveInvestigator')
        if (mv)
          return {
            pid: Number(pid),
            ms: mv.contents[0],
            iid: mv.contents[0].investigator,
            // the engine offers either free steps or paid ones, never both
            paid: (c.messages ?? []).some((m) => m.tag === 'PayMoney'),
          }
      }
    }
    return null
  }
  function moveBudget(g: Game, ctx: MoveCtx) {
    const money = g.investigators[ctx.iid]?.money ?? 0
    // a choice carries the move state as it would be AFTER that step, so undo it
    const free = ctx.paid ? ctx.ms.remaining : ctx.ms.remaining + 1
    const usedPaid = ctx.paid ? ctx.ms.paidSteps - 1 : ctx.ms.paidSteps
    const paid = Math.max(0, Math.min(ctx.ms.maxPaidSteps - usedPaid, money))
    return { free, paid, total: free + paid }
  }
  function movePaths(g: Game, from: string, budget: number) {
    const borders = g.board.borders ?? {}
    const dist: Record<string, number> = { [from]: 0 }
    const prev: Record<string, string> = {}
    const queue = [from]
    while (queue.length) {
      const cur = queue.shift()!
      if (dist[cur] >= budget) continue
      for (const nxt of Object.keys(borders[cur] ?? {}))
        if (dist[nxt] === undefined) {
          dist[nxt] = dist[cur] + 1
          prev[nxt] = cur
          queue.push(nxt)
        }
    }
    delete dist[from]
    return { dist, prev }
  }
  // the dragged investigator and where it may be dropped: free steps, or paid ones beyond them
  const moveDrag = computed(() => {
    const g = game.value
    if (!g) return null
    const ctx = moveContext(g)
    if (!ctx) return null
    const from = g.investigators[ctx.iid]?.space
    if (!from) return null
    const { free, total } = moveBudget(g, ctx)
    const { dist } = movePaths(g, from, total)
    const targets: Record<string, 'droppable' | 'droppable-paid'> = {}
    for (const [sid, d] of Object.entries(dist)) targets[sid] = d <= free ? 'droppable' : 'droppable-paid'
    return { ...ctx, from, targets }
  })
  async function walkTo(target: string) {
    for (let guard = 0; guard < 16; guard++) {
      const g = game.value
      if (!g) return
      const ctx = moveContext(g)
      if (!ctx) return
      const from = g.investigators[ctx.iid]?.space
      if (!from || from === target) return
      const { total } = moveBudget(g, ctx)
      const { dist, prev } = movePaths(g, from, total)
      if (dist[target] === undefined) return
      let step = target
      while (prev[step] !== undefined && prev[step] !== from) step = prev[step]
      const idx = g.questions[ctx.pid].choices.findIndex((c) => c.label.tag === 'SpaceLabel' && c.label.contents === step)
      if (idx < 0) return
      await choose(ctx.pid, idx)
      if (game.value?.investigators[ctx.iid]?.space === from) return
    }
  }

  // spaces an answer names, with the questions (of mine) that offer them
  const spaceChoices = computed(() => {
    const m: Record<string, [number, number][]> = {}
    for (const [pid, q] of Object.entries(game.value?.questions ?? {})) {
      if (!isMine(pid)) continue
      q.choices.forEach((c, i) => {
        if (c.label.tag === 'SpaceLabel') (m[c.label.contents] ??= []).push([Number(pid), i])
      })
    }
    return m
  })

  // Setup marks no unstable space until step 11 turns the first event card into the
  // discard (111.3); the starting-space fallback of 493.3 is left to the engine.
  const unstableShown = computed(() => {
    const g = game.value
    if (!g) return []
    if (g.phase === 'SetupPhase' && !g.decks.eventDiscard.length) return []
    return view.value?.unstable ?? []
  })

  return {
    tableId,
    catalog,
    tv,
    view,
    game,
    error,
    me,
    mySeats,
    seated,
    isHost,
    isMine,
    usernameOf,
    playerOfInv,
    invOfPlayer,
    myInvestigators,
    cardNameRaw,
    cardName,
    invName,
    spaceName,
    cardCode,
    cardFace,
    initials,
    scenarioName,
    eventImage,
    activeCardImage,
    inPlayerArea,
    sourceLabelText,
    labelTarget,
    highlighted,
    marks,
    selectedTab,
    selectTab,
    debugMode,
    toggleDebug,
    debugAllowed,
    dbgOn,
    dbgIid,
    logOpen,
    logSeen,
    toggleLog,
    logUnread,
    unstableHeld,
    unstableReveal,
    unstableShown,
    apply,
    choose,
    debugAction,
    undo,
    moveDrag,
    walkTo,
    spaceChoices,
  }
}

export type GameContext = ReturnType<typeof createGameContext>
const key: InjectionKey<GameContext> = Symbol('game')
export const provideGame = (ctx: GameContext) => provide(key, ctx)
export const useGame = () => {
  const ctx = inject(key)
  if (!ctx) throw new Error('useGame outside a table')
  return ctx
}
