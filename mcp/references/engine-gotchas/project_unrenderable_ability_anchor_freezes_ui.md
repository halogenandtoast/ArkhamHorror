# An ability anchored to an unrenderable card freezes the game

The frontend renders a choice by finding the card the ability's source points at
and hanging a click handler on it. If that card is drawn by a component that
emits a bare `<img>` with no `@click` and no `data-index` ability lookup, the
choice exists in `gameQuestion` but the player can never pick it. A *mandatory*
question (`WindowChooseOne` of `ForcedAbility`s) then hard-stops the game: the
board renders, nothing is highlighted, nothing is clickable.

Known instance: `FacedownInThreatArea` (Dark Matter / Lost Quantum).
`frontend/src/arkham/components/Player.vue` renders `facedownThreatCards` as

```vue
<div v-for="facedown in facedownThreatCards" class="card-container" :data-index="facedown.cardId">
  <img class="card" :src="facedownThreatCardImage(facedown.cardId)" />
</div>
```

— deliberately inert, because nobody is supposed to know what those cards are.

## How to spot it

The symptom is "the game just stops" with no server error. Check
`current_data->'gameQuestion'` in the DB:

```sql
select jsonb_pretty(current_data->'gameQuestion') from arkham_games where id = '<uuid>';
```

If the choices are `AbilityLabel`s whose `ability.source` resolves to an entity
in a hidden/out-of-play placement, that's this bug.

## How to avoid it

Prefer not generating the choice at all: put the real condition in the window
key so only the cards that should trigger are offered
([[project_window_key_vs_payload_guard]]). A hidden card that legitimately must
act should use `SilentForcedAbility`, which queues its `UseCardAbility` without
asking the player to click anything.
