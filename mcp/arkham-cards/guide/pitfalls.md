# The ways a custom card silently does nothing

Every entry here is a real failure found in this project. None of them raised an
error; each one produced a card that looked written and did nothing.

## `null` in a field that is not `Maybe`

The worst one, because the builder writes it for any field left unfilled. `null`
does not fail the field — it fails **the whole enclosing value**.

```json
{"tag": "ActionAbility", "actions": null, "cost": null}
```

`Actions` parses only an array or a tagged object, so this `AbilityType` does not
decode, so `customAbilities` drops the ability entirely. The card has no ability
at all. Thirst for Knowledge's action was dead from the day it was created, and
two more cards in this library still are.

Written correctly: `"actions": []` (= `AndActions []`, no named action type) and
`"cost": {"tag": "ActionCost", "contents": 1}`. `skillTypes` really is `Maybe`
and may be omitted.

**Omit the key. Never write `null` unless the type is `Maybe`.**

## The three encodings

- all constructors nullary → **a bare string**. `{"tag": "IsUpkeepPhase"}` does
  not decode. Applies to `PhaseMatcher`, `Timing`, `ForPlay`, `ForMovement`,
  `SlotType`, `Token`, `ChaosTokenFace`, `CardType`, `SkillType`, `ClassSymbol`,
  `DiscardType` and more.
- one constructor → **no tag at all**. `Modifier`, `Discover`, `Investigate`,
  `Search`, `Movement`, `CardDraw`, `HandDiscard`, `Exhaustion`,
  `DamageAssignment` and about 45 others. Tagging them anyway fails to decode.
- otherwise `{"tag", "contents"}` — `contents` omitted or `[]` for a nullary
  constructor, bare for one field, a list for several. **Except** a record
  constructor, whose named fields sit beside the tag.

A constructor is a record iff **every** one of its fields has a name. The
type-level `record` flag is true when *any* constructor is, so reading that
instead invents `contents` for the record constructors of a mixed type and drops
it from the positional ones. `schema_type` prints the right shape per constructor.

## Step keys live in two different places

A step the dispatch hands to a handler keeps its keys **inside** its own value:

```json
{"chooseFrom": {"bind": "card", "query": {...}, "steps": [...]}}
```

A step the dispatch handles itself reads the step object, so its keys sit
**beside** it:

```json
{"let": "cid", "be": {"get": "id", "kind": "card", "of": "$card"}}
```

Put a key in the wrong place and it is simply never read. `step_reference` says
which for every step, and `validate_card` reports it as "belongs inside".

## A step object with two step keys

Only the first in dispatch order runs; the rest are ignored. Split them.

## A hidden card that places itself

`Hidden` on an enemy or a treachery **is** the revelation. The def is given
`IsRevelation` whether or not it asks for one, and the revelation secretly places
the card in the drawing investigator's hand before any `_onRevelation` steps run.
`_revelationPlacement` is not read for a hidden card, and a step that places it
anywhere else is fighting the keyword. A card that only *might* hide (Delusory
Evils) is in hand by the time its steps run, so the other branch has to discard
it from there.

Nothing discards a hidden card, so the only way out of hand is an ability the card
prints itself: give it one with `"zone": "hand"`. A hidden enemy does not spawn,
is not engaged with you and does not attack while it is in your hand.

On any other card type the keyword is printed text and nothing more.

## A cost written as a step

"Discard a card: draw 2" pays before resolving and does not resolve at all if
you cannot pay. As a step, the card draws whether or not the discard happened.
Costs belong in the ability's `type.cost`.

Cost *order* is not list order either: `Semigroup Cost` sorts, so a cost that must
be paid after another has to be declared later in `data Cost`.

## An ability that needs a window but names none

`ReactionAbility` takes a `WindowMatcher`. `schema_type WindowMatcher` reports,
per constructor, which windows it fires on — a matcher that fires on no window is
a combinator or a question about state, and an ability built on one never
triggers.

## A handler on a message the engine never sends

`_handlers[i].on` names a message constructor. Most messages sit inside a
grouping constructor — `Defeated` is really `DefeatMessage (Defeated_ …)` — and a
handler names it the way the engine's pattern synonyms do, without the trailing
underscore. A name that matches nothing can never fire. `validate_card` checks
the name against every message the engine has.

Also: a handler only fires when the message **mentions this card** (by source,
target or id) unless it sets `"global": true`. A message about the game itself
names nobody and can only be heard globally.

## A modifier condition that asks the game to act

Modifiers are gathered while *reading* the game, not while changing it. A
`_modifiers` entry's `if` can ask questions but cannot do anything. To gate on
where the card is, use `requires` — it asks the game nothing, which is the point:
a card with an in-hand ability is an entity in hand *and* once committed, and a
query to tell those apart would ask for modifiers while modifiers are being
collected.

## A pure matcher that needs to read the campaign log

A `CardMatcher` is matched purely and cannot read the log. Bind the answer in the
spec's `let` and substitute it into the matcher: that moves the read to the one
place it can happen, while modifiers are being collected.

## Arithmetic over anything but a literal

Fixed, but worth knowing the shape of: an operator's operands are themselves
expressions, and each must be evaluated. If you write an expression where a
number is wanted, check `expression_reference` for whether that operator
broadcasts.

## Timing

Validation cannot see timing. Before trusting a clause that turns on a window, an
"instead", a cancel, or a defeat, grep `mcp/references/engine-gotchas/` —
51 entries there are about windows and the queue alone, and several describe
cards that looked right and fired at the wrong moment.
