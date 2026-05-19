// Browser-evaluated check helpers.
//
// These functions return JavaScript SOURCE STRINGS that the harness runner
// passes to `mcp__chrome-devtools__evaluate_script`. They run inside the page
// and return JSON-serialisable results.
//
// Why strings? Chrome DevTools MCP takes a function expression as text. Keeping
// the source here (rather than inlining at every call site) means scripts can
// be reviewed, tested with `node --check`, and reused across scenarios.

// Returns the number of matching elements + a small sample of their textContent.
export function count(selector) {
  return `() => {
    const els = document.querySelectorAll(${JSON.stringify(selector)})
    return {
      selector: ${JSON.stringify(selector)},
      count: els.length,
      sample: Array.from(els).slice(0, 5).map(e => (e.textContent || '').trim().slice(0, 80))
    }
  }`
}

// Asserts a selector resolves to N (or >=min) elements. Throws inside the
// page, which propagates back as an error result.
export function expectCount(selector, { exact, atLeast } = {}) {
  const check = exact !== undefined
    ? `if (n !== ${exact}) throw new Error('expected ' + ${exact} + ' got ' + n + ' for ' + ${JSON.stringify(selector)})`
    : `if (n < ${atLeast ?? 1}) throw new Error('expected at least ' + ${atLeast ?? 1} + ' got ' + n + ' for ' + ${JSON.stringify(selector)})`
  return `() => {
    const n = document.querySelectorAll(${JSON.stringify(selector)}).length
    ${check}
    return { selector: ${JSON.stringify(selector)}, count: n }
  }`
}

// Snapshot of the current game's high-level structural state. Useful as a
// before/after invariant check during refactors of mega-components.
export function gameStructure() {
  return `() => ({
    locations: document.querySelectorAll('.location').length,
    enemies: document.querySelectorAll('.enemy').length,
    investigators: document.querySelectorAll('.portrait-container, .investigator').length,
    abilityButtons: document.querySelectorAll('.ability-button, [class*="--can-interact"]').length,
    skillTestOpen: !!document.querySelector('.skill-test, [class*="skillTest"]'),
    questionOpen: !!document.querySelector('.question, .choice-modal'),
    url: location.pathname,
  })`
}

// Returns any uncaught errors that were stashed on window.__harnessErrors.
// Pair with installErrorTrap() at the start of a scenario.
export function readErrors() {
  return `() => (window.__harnessErrors || [])`
}

// Installs a window error + unhandledrejection listener that records crashes.
// Run this once after navigating, BEFORE driving interactions.
export function installErrorTrap() {
  return `() => {
    window.__harnessErrors = window.__harnessErrors || []
    if (window.__harnessTrapInstalled) return { installed: false, alreadyInstalled: true }
    window.__harnessTrapInstalled = true
    window.addEventListener('error', e => {
      window.__harnessErrors.push({ kind: 'error', message: e.message, source: e.filename + ':' + e.lineno })
    })
    window.addEventListener('unhandledrejection', e => {
      window.__harnessErrors.push({ kind: 'unhandledrejection', message: String(e.reason) })
    })
    return { installed: true }
  }`
}
