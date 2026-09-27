---
title: mercury_this_or_that_untagged_union
description: "`ThisOrThat a b` is `Either` whose JSON is transparent — no constructor tag on the wire — for splitting a sum type without breaking existing consumers"
---

**Source:** `local-packages/this-or-that/`.

```haskell
encode (Right 10 :: Either Text Int)     == "{\"Right\":10}"
encode (That  10 :: ThisOrThat Text Int) == "10"
```

The README's framing is the useful part: `Either` means "tagged choice"; `ThisOrThat`
means "treated *transparently* as either" — same shape, different contract, and encoding
that intent in the type stops someone from "fixing" the missing tag later.

Its stated use is splitting one sum type into two while keeping one wire format: the old
consumers see the same JSON, the Haskell side gets the distinction it needs.

**How to apply here:** the frontend decodes tagged unions from the backend, and the
places where a tag is *absent* on purpose are currently invisible in the types. Worth
reaching for when a `Message`/`Card` payload has to accept two shapes for
backward-compatible replay of old saved games — a hand-written `FromJSON` with an
alternative is the status quo, and it doesn't say why.
