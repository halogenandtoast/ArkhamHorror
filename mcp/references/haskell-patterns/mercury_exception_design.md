---
title: mercury_exception_design
description: "One record-shaped exception type per failure, carrying every id needed to debug it, with `displayException` written out — never a stringly `InvariantViolated`"
---

**Source:** `.cursor/rules/exception_handling.mdc`,
`local-packages/a-mercury-prelude/src/A/MercuryPrelude/Exception/`.

Rules, in their order of importance:

1. **Exceptions are for the exceptional.** Expected failure is `Maybe`/`Either` in the
   return type. `getUserById :: UserId -> DB (Maybe User)`, never a throwing lookup.
2. **No generic exception types.** A single shared `InvariantViolated Text` makes every
   failure look identical in the error tracker. One type per failure mode:

```haskell
data PaymentProcessingFailed = PaymentProcessingFailed
  { paymentId :: PaymentId, accountId :: AccountId, amount :: Dollar, failureReason :: Text }
  deriving stock Show

instance Exception PaymentProcessingFailed where
  displayException PaymentProcessingFailed {..} = mconcat [...]
```

3. **Single constructor, record fields, verbose names, every relevant id included.**
   The exception is the bug report.
4. **Exception types live in their own `Exceptions.hs` module** so they're importable
   without dragging in the implementation.
5. **Add context, don't re-report.** `mercuryCheckpoint someId $ ...` and
   `withLogContext` annotate anything thrown inside (via `annotated-exception`); manual
   "catch, log, rethrow" is banned because it duplicates reports and loses the stack.
6. **Banned outright:** `throwIO`/`throwM` (use `throwWithCallStack`), `undefined`,
   bare `error`, `unsafePerformIO`, `unsafeCoerce`, `head`/`tail`/`read`.

**How to apply here:** the engine's failure mode is `error "Unknown asset: ..."` /
`error "invalid target"` — a stringly invariant violation with no game id, no card code,
no window. A small set of typed engine exceptions (`UnknownCard`, `NoSuchEntity`,
`ImpossibleMessage`) carrying the `Message` and the entity id would make the production
500s self-diagnosing, and would survive the `annotated-exception` context trick since
the runner already knows which card it's dispatching to.
Related: [[mercury_require_callstack]], [[mercury_structured_logging]].
