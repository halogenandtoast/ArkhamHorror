# Writing a custom card, end to end

A custom card has no Haskell behind it. Its behaviour is JSON in the card def's
`meta`, decoded at the moment it runs. That one fact sets the whole process,
because **a value in the wrong shape is never an error**: it fails to parse and
whatever contained it keeps its default. An ability whose `type` will not decode
is dropped outright — the card ends up with no ability at all, and nothing says
so. One card in this library had a dead action for its entire existence.

So the work is not "write JSON and see if it runs". It is "write JSON that is
checked against the types before it is saved". Follow the order below.

## 1. Read the card, and say what each clause is

Split the printed text into clauses and name what kind of thing each one is,
because each kind has exactly one home:

| The text says | It is | It goes in |
|---|---|---|
| "[action]: do X", "[reaction] After Y, do X", "[fast] do X" | an ability | `meta._abilities` |
| "Cost: exhaust, discard a card, spend 2 resources" | part of the ability's `type.cost`, not a step | `_abilities[i].type` |
| a standing rule, "you get +1", "enemies at your location get…" | a modifier | `meta._modifiers` |
| "when this is revealed…" | a revelation | `meta._onRevelation` |
| "when you play this…" (event) | play behaviour | `meta._onPlay` |
| "[elder_sign]: …" (investigator) | elder sign | `meta._elderSign*` |
| something the card must notice happening to it | a handler | `meta._handlers` |

Two distinctions worth getting right at this stage, because they are expensive to
undo:

- **A cost is not a step.** "Discard a card: draw 2" pays first and does not
  resolve at all if you cannot pay. Written as a step, the card draws whether or
  not the discard happened. Costs go in `type.cost`.
- **A standing rule is not an ability.** "You can only play cards whose traits
  you have learned" is a `_modifiers` entry, not something anyone activates.

## 2. Find the precedent

Do this before writing anything. Two places, and both are cheaper than reasoning
from the types:

- `official_cards` with `text=` — find the printed card that already says this.
  The engine implements that wording somewhere, so matching it is most of getting
  the card right.
- `custom_card_examples` — find a construct already used in this library. A
  fragment a player has actually used is *known to decode*, which is more than
  the schema can promise.

If the rules are unclear, `rules_search` before deciding. Local rulings win over
everything; the Grimoire outranks the FAQ only for Chapter 2 cards.

## 3. Look up every type you are about to write

`schema_search` to find the constructor, `schema_type` to see how it is written.
Never guess an encoding: three shapes are easy to get wrong and all three fail
silently.

- a type whose constructors are **all nullary** is a **bare string** —
  `"IsUpkeepPhase"`, never `{"tag": "IsUpkeepPhase"}`
- a type with **one constructor** carries **no tag at all** — `Modifier`,
  `Discover`, `Investigate` and 45 others
- everything else is `{"tag": …, "contents": …}`, with `contents` omitted for a
  nullary constructor, bare for one field, and a list for several — **except** a
  record constructor, whose fields sit beside the tag

`schema_type` prints the right one for every constructor, so read it rather than
inferring it.

## 4. Write the def

`card_def_reference` for the printed fields. Behaviour goes in `meta`. Refer to
values by `$name`: see `bindings_reference`. Steps: `step_reference`. Expressions:
`expression_reference`.

The single most useful habit: **never write `null`**. The builder writes it for a
field nobody filled in, and `null` in a field that is not `Maybe` kills the whole
enclosing value, not the field. Omit the key instead. If a field genuinely takes
"nothing", check whether its type is `Maybe` first — `"actions": []` is right,
`"actions": null` is a dead ability.

## 5. Validate

`validate_card` on the whole def. It walks every value against its type and every
step against the grammar, and reports:

- an unknown tag, a wrong arity, an enum written as an object, a
  single-constructor type written with a tag
- `null` in a field that is not `Maybe`
- a step key nothing reads, a property an entity does not have, a `$binding`
  nothing bound, a message no handler could ever hear

Fix every error. Warnings are keys and names the engine ignores in silence —
each one is a line of your card doing nothing.

## 6. Save

`card_sets` to pick the destination, then `save_card`. A 200 is the parser's own
verdict that the def decodes as a `CardDef`; nothing else proves that. Saving is
an upsert on (user, card code), so re-saving an edited card replaces its row.

## 7. Say what is untested

Validation proves the card *decodes*. It cannot prove the card *does what the
text says* — no tool here plays it. Say plainly which clauses have never been
exercised in a game, and if a clause turns on timing (a window, an "instead", a
cancel), read `mcp/references/engine-gotchas/` before trusting it.
