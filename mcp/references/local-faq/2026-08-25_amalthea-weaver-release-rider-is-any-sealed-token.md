---
title: Amalthea Weaver's release rider is any sealed token, not only ☾
date_added: 2026-08-25
source: internal decision (user instruction) — campaign-specific ruling for the Circus Ex Mortis homebrew campaign
affects:
  - Amalthea Weaver — Aspirant of Courage
  - Amalthea Weaver — Oracle of Purity
  - Amalthea Weaver — Oracle of Resolve
  - Moon token (☾)
  - sealed chaos tokens
---

# Amalthea Weaver's release rider is any sealed token, not only ☾

**Q: Aspirant of Courage, Oracle of Purity and Oracle of Resolve say "release a
token sealed on a card at your location" — no ☾ symbol, unlike the +X clause on
the same cards, which does say ☾. Is the release restricted to ☾ tokens?**

A: No — read it literally. **Any** sealed chaos token on a card at your
location may be released: ☾, {bless}, {curse}, or anything else an effect has
sealed there. The token returns to the chaos bag as normal.

The other half of these cards is unaffected: "+X skill value, where X is half
the number of ☾ tokens sealed on cards at your location" explicitly names ☾ and
counts **only** ☾ tokens.

## Affected cards / systems

- Amalthea Weaver — Aspirant of Courage (`:circus-ex-mortis:229`) — `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Assets/AmaltheaWeaverAspirantOfCourage.hs`
- Amalthea Weaver — Oracle of Purity (`:circus-ex-mortis:231`) — `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Assets/AmaltheaWeaverOracleOfPurity.hs`
- Amalthea Weaver — Oracle of Resolve (`:circus-ex-mortis:232`) — `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Assets/AmaltheaWeaverOracleOfResolve.hs`
- Shared rider — `amaltheaWeaverRelease` in `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/DestinyAndProphecy.hs`
- Helpers — `getSealedTokensAt` (any face) vs `getSealedMoonTokensAt` (☾ only), `chooseReleaseTokens` in `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Helpers.hs`

## Implementation status

- **`amaltheaWeaverRelease`**: ✏️ updated — was `getSealedMoonTokensAt` +
  `chooseReleaseMoonTokens` (☾ only); now `getSealedTokensAt` +
  `chooseReleaseTokens`, offering every sealed token on an investigator card or
  asset at your location.
- **Aspirant of Courage / Oracle of Purity / Oracle of Resolve**: ✅ no card-side
  change — all three go through the shared rider (release 1, up to 2, and 1
  respectively).
- **`amaltheaWeaverBoost` (the +X clause)**: ✅ unchanged — still counts ☾ only,
  via `getSealedMoonTokensAt`.
- **Invocation of Diana** (`:circus-ex-mortis:242`): ✅ unchanged — its release
  clause does print ☾ and stays ☾-only.
