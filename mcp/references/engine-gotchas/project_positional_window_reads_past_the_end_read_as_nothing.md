---
title: project_positional_window_reads_past_the_end_read_as_nothing
description: $w0, $w1 … read a window's fields by position, and one past the end is not an error — it reads as nothing, and whatever it fed quietly does nothing
---

A custom card reaches a window's payload positionally: `$w0` is its first
field, `$w1` the second. An index past the end reads as `Null`, which is not an
error anywhere -- it just makes whatever consumed it answer nothing. A criterion
is never true, a query finds nobody, a step is skipped.

The trap is that the **matcher** and the **window** carry different fields. The
matcher is what the card writes:

```haskell
Matcher.EnemyAttacksEvenIfCancelled Timing Who EnemyAttackMatcher EnemyMatcher   -- 4
Window.EnemyAttacksEvenIfCancelled EnemyAttackDetails                            -- 1
```

so `$w2` looks like "the enemy" (the matcher's third argument) and is past the
end of the window. The enemy is *inside* `$w0`:
`{"field": "enemy", "of": "$w0"}` -- and the field can be named without its
record prefix, so `enemy` finds `attackEnemy`.

Live Dangerously read `$w2` and so never granted its second half, on either
printing, while reading as a finished card.

Two things that make this easy to get wrong and hard to see:

* the matcher's arity is what is in front of you while writing the card;
* a criterion is **substituted, not evaluated**, so reaching into `$w0` has to
  be bound with a `let` first -- an expression left inside a criterion is
  dropped the same way one inside a `push` is.

The custom-card checker now compares every `$wN` against the window's field
count, wherever the matcher and the window share a name.
