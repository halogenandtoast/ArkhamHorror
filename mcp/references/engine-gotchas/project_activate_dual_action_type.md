---
title: project_activate_dual_action_type
description: "An [action] ability on a controlled/scenario card counts as BOTH an activate action and its designated type (e.g. fight)"
---

Per the 2026 Grimoire (`.claude/references/grimoire/glossary/activate_action.md`,
Cosmic Flame example), triggering an `[action]` ability on a card you control (or
a scenario card at your location / act / agenda) counts as **both an activate
action AND** its bold designated type. So a weapon's "[action]: Fight" (Azure
Flame) is simultaneously an activate and a fight action; it does not provoke AoO
because it is also another action type.

The engine already models this in `abilityTypeActions` (`Arkham/Ability.hs`):
a non-basic `ActionAbility` is tagged with an implicit `#activate` plus its
designated actions, so `azureFlame.actions == [#activate, #fight]`. Basic actions
(`abilityBasic == True`: Fight/Engage/Evade/Investigate/Move/Draw/Resource taken
directly) are NOT activate actions.

Consequence for "different types of actions" logic (e.g. Captivating Performance
3, card 11108): a streak of multi-type actions is a system of distinct
representatives (SDR), and a candidate fourth action is only a valid "different
type" if it is disjoint from at least one complete SDR of the streak — use
`sdrExists`/`longestUniqueStreak`/`pickSDR` (now in `Arkham/Helpers/Action.hs`).
Don't flatten the streak to a single type list; it loses the multi-type info.
Fixed in issue #4834. Grimoire is the top-priority source for Chapter 2 (11xxx)
cards. See [[feedback_i18n_card_implementation]].
