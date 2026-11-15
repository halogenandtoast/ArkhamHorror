---
name: project_printed_you_may_needs_an_explicit_decline
description: "A printed \"you may\" must be chooseOne with an explicit decline label — chooseOrRunOne auto-runs a lone option and silently removes the choice; Dirty Deeds (3) forced Liquid Courage's [action] (#5998)"
metadata:
  node_type: memory
  type: project
  originSessionId: b3c47971-ad01-4de3-b29c-3bc18b06cd74
  modified: 2026-11-14T10:35:52.015Z
---

`chooseOrRunOne` / `chooseOrRunOneM` **auto-resolve when exactly one option is
available**. That is right for a mandatory effect with one legal target, and wrong for
anything the card prints as "you **may**" — with one candidate the player is never
asked, and the optional effect becomes compulsory.

An optional ability choice is `chooseOne` with the decline label first:

```haskell
when (notNull abilities) do
  chooseOne iid
    $ Label "$label.doNotUseAbility" []
    : [AbilityLabel iid ab (defaultWindows iid) [] [] | ab <- abilities]
```

`$label.doNotUseAbility` is translated in `en`, `zh`, `zh-cn` and `ko`
(`frontend/src/locales/*/label.json`); the near neighbours are
`$label.doNotPlayAsset`, `doNotTriggerAbility` and `doNotResolveElderSign`.

**Why:** issue #5998 — Dirty Deeds (3) prints "Then, you **may** resolve an [action] or
[fast] ability on that asset", but used `chooseOrRunOne`. Searching out Liquid Courage
left exactly one ability, so the engine fired its `[action]` unasked: it healed a
teammate's horror and dropped them into a willpower (2) test that discards a card at
random on failure, none of it chosen. Throw the Book at Them, which has the same
"resolve an [action] or [fast] ability ignoring all costs" text, already had the
decline label — use it as the template, including its `defaultWindows iid` on the
`AbilityLabel` (an empty window list is unsafe for a borrowed `[fast]` ability).

**How to apply:** when implementing or reviewing a card, read the printed text for
"may". If it is there, the ask must survive down to one option. This is separate from
[[feedback_keep_single_option_asks_deliberate]], which says not to collapse a
single-option ask to fix a *UI* complaint — both point the same way: keep the click.

Related: [[feedback_no_invented_abilities]],
[[feedback_forced_ability_only_when_printed]].
