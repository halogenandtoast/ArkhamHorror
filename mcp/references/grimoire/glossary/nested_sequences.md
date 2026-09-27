---
title: Nested Sequences
source: Arkham Grimoire v1.0 (2026)
---

# Nested Sequences

Some game effects may prompt a chain reaction, or nested sequence.
ormerEach time a triggering condition occurs, the following sequence occurs:
r factories,
e able1. Execute “when…” effects that interrupt that triggering condition.
ties.
   2. Resolve the triggering condition itself, as well as “at...” or “if...” effects
      that resolve simultaneously to that triggering condition.
   3. Execute “after…” effects in response to that triggering condition’s
      completed resolution.
  Within this sequence, if the use of a  or Forced ability leads to a new
  triggering condition, the game pauses and starts a new sequence, following
  the same steps 1–3 as outlined above, in response to the new triggering
  condition. This is called a nested sequence.
  Once a nested sequence is completed, the game returns to where it left off,
  continuing with the original triggering condition’s sequence.
  It is possible that a nested sequence generates further triggering conditions
  (and hence further nested sequences). There is no limit to the number of
  nested sequences that may occur, but each nested sequence must complete
  before returning to the sequence that triggered it. In effect, these sequences
  are resolved in a last in, first out manner.