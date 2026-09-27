---
title: mercury_structured_logging
description: "One `MercuryLog` class for all logging, structured key-value pairs appended with `(-:)`, and `withLogContext` to attach ids to every log inside a block"
---

**Source:** `local-packages/mercury-logging/`, `.cursor/rules/logging.mdc`.

```haskell
log Info $ "Processing transaction"
  <> "transactionId" -: txId
  <> "userId"        -: userId

withLogContext "userId" userId $ do
  log Info "Starting user processing"   -- and everything nested below carries userId
  validateUser
```

The points that generalise:

- **A class constraint, not a global handle.** `MercuryLog m` in the signature says a
  function logs; tests can supply a capturing implementation. `putStrLn`/`trace` are
  banned by hlint, so there is exactly one path out.
- **`(-:)` builds structured fields** that the log aggregator can index — the message
  string stays constant and the variables are separate fields, so you can search on
  `userId` instead of grepping interpolated text.
- **`withLogContext` scopes metadata** rather than repeating it on every line.
- Conventions: `Warn` for expected errors, `Error` for unexpected system failures;
  `displayException` not `show`; `pack (displayException e)` not `tshow` (which
  double-encodes and leaves stray quotes); never log PII.

**How to apply here:** the engine logs game events through `HasGameLogger`, but
diagnostics are mostly ad-hoc. The transferable bits are the constant-message +
structured-fields discipline (so a log line can be searched by `gameId`/`cardCode`
rather than by substring) and the `withLogContext`-style scoping around message
dispatch, which would attach the current `Message` and card to anything logged beneath it.
Related: [[mercury_exception_design]].
