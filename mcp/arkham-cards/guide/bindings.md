# `$bindings`

The JSON is written before the card exists, so it cannot name the entity it
belongs to. It refers to values by `$name`, and those are substituted into the
JSON **before it is decoded** — so the decoder only ever sees ordinary, fully
applied JSON. A whole string is replaced by a whole value, so a binding keeps its
type instead of being spliced into text.

A `$name` that was never bound stays the literal string `"$name"`, which then
fails to decode, which silently drops whatever contained it. Nothing warns you.
`validate_card` checks this.

## Always available

| Binding | What it is |
|---|---|
| `$source` | this card, as a `Source` |
| `$target` | this card, as a `Target` |
| `$meta` | the card's own `meta`, for steps that read a declaration |
| `$investigator` | for a signature card, whose card it is |
| `$iid` | whoever used the ability (not bound before anyone does) |
| `$payment` | what the ability's cost took — read it with `apply` |
| `$window` | the window the ability triggered on |
| `$w0`, `$w1`, … | that window's fields, positionally |
| `$message` | in a handler, the whole message |
| `$0`, `$1`, … | in a handler, the message's fields, positionally |

## The card's own fields

Every field of the card's serialized attrs is bound under its JSON name: `$id`,
`$placement`, `$controller`, `$owner`, `$cardId`, `$tokens`, `$exhausted`, and so
on. Which exist depends on the card's type — an asset has `$controller`, a
treachery has `$drawnBy`. `bindings_reference` with a `cardType` lists them.

## What steps bind

A step binds for the steps *after* it, and for its own nested steps — never for
the step itself.

| Step | Binds | Under |
|---|---|---|
| `query` | what the matcher found | its `bind` (beside the step) |
| `let` | the expression's value | the name in `let` |
| `random` | a fresh id, or one element of `from` | `bind`, else `random` |
| `forEach` | each thing found | `each`, else `bind`, else `each` |
| `chooseFrom` | the thing chosen | `bind`, else `chosen` |
| `repeat` | the iteration number | `i`, else `bind`, else `i` |
| `withSkillTest` | the test | `bind`, else `skillTest` |
| `withLocationOf` | the location | `bind`, else `location` |
| `distribute` | who, and their share | `bind`/`amount`, else `who`/`amount` |
| `fight` `investigate` `evade` `parley` `test` | the test it started | `$sid` |
| `choose` (per option) | what that option's query found | the option's `bind`, else `chosen` |

`$sid` is the important one. "For this investigation" is a modifier scoped to a
test that did not exist when the card was written, so the step that starts the
test hands its id to the steps after it.

## Reading inside a binding

A binding holds a whole serialized value, so `$placement` is
`{"tag": "AttachedToLocation", "contents": <id>}` — not the id. Reach inside it
with the `field` expression, and ask the game about it with `get`:

```json
{"let": "where", "be": {"field": "contents", "of": "$placement"}}
```
