---
title: Ability
source: ArkhamDB Rules Reference (https://arkhamdb.com/rules)
---

# Ability

An ability is the specialized game text that indicates how a card
affects the game.

- Card abilities only interact with the game if the card bearing the
  ability is in play, unless the ability (or rules for the cardtype)
  specifically references its use from an out-of-play area.
- Card abilities only interact with other cards that are in play, unless
  the ability specifically references an interaction with cards in an
  out-of-play area.
- If multiple instances of the same ability are in play, each instance
  interacts with (or may interact with) the game state individually.

The various types of card abilities are: constant abilities, forced
abilities, revelation abilities, triggered abilities, keywords, and
enemy instructions (spawn and prey). Each type is described in detail
below.

See also: "[Costs](#Costs)" on page 7, "[Effects](#Effects)" on page 9,
"[Qualifiers](#Qualifiers)" on page 17, "[Self-Referential
Text](#Self_Referential_Text)" on page 18.

### Constant Abilities

Constant abilities are simply stated on a card with no special
formatting. Constant abilities are always interacting with the game
state as long as the card is in play. (Some constant abilities
continuously seek a specific condition, denoted by words such as
"during" or "while." The effects of such abilities are active any time
the specified condition is met.) Constant abilities have no point of
initiation.

### Forced Abilities

A forced ability is identified by a bold "**Forced** – " command. Forced
abilities initiate and interact with the game state automatically at a
specified timing point. Such a timing point is usually indicated by
words such as: "when," "after," "if," or "at."

- If a forced ability does not have the potential to change the game
  state, the ability does not initiate.
- The initiation of a forced ability that has the potential to change
  the game state is mandatory each time its specified timing point is
  met.
- A forced ability with a timing point beginning with the word "when..."
  automatically initiates as soon as the specified timing point is
  reached, but before its impact upon the game state resolves.
- A forced ability with a timing point beginning with the word
  "after..." automatically initiates immediately after that timing
  point's impact upon the game state has resolved.
- For any given timing point, all forced abilities initiated in
  reference to that timing point must resolve before any \[reaction\]
  abilities (see below) referencing the same timing point in the same
  manner may be initiated.

See "[Priority of Simultaneous
Resolution](#Priority_of_Simultaneous_Resolution)" on page 17.

### Revelation Abilities

A revelation ability, indicated by a bold "**Revelation** – " command on
an encounter card or weakness, initiates as that card is drawn by an
investigator (see "[Revelation](#Revelation)" on page 18).

### Triggered Abilities

A triggered ability is any ability prefaced by either a \[free\] icon, a
\[reaction\] icon, or an \[action\] icon. If the ability has one or more
prerequisites (costs and/or conditions), these are listed in text
immediately following the icon. A player must always meet the
prerequisites of a triggered ability in order to trigger that ability.
There are three types of triggered abilities:

Free triggered abilities (\[free\]) – A \[free\] triggered ability may
be triggered as a player ability during any player window. (See
"[Appendix II: Timing and Gameplay](#Appendix_II_Timing_and_Gameplay)"
on page 22 for a complete list of player windows.)

Reaction triggered abilities (\[reaction\]) – A \[reaction\] triggered
ability with a specified triggering condition may be triggered any time
that triggering condition is met. *For example: "\[reaction\] After you
defeat an enemy:"*

- A \[reaction\] ability with a triggering condition beginning with the
  word "when..." may be used after the specified triggering condition
  initiates, but before its impact upon the game state resolves.
- A \[reaction\] ability with a triggering condition beginning with the
  word "after..." may be used immediately after that triggering
  condition's impact upon the game state has resolved.
- Each \[reaction\] ability may be triggered only once each time the
  specified condition on the ability is met. *For example, an ability
  that is triggered "After X occurs," may be used once each time "X"
  occurs.*

Action triggered abilities (\[action\]) – An \[action\] triggered
ability may be triggered during a player's turn in the investigation
phase through the use of the activate action, and only if the player
uses one action for each \[action\] specified in the ability's cost.

All triggered abilities are governed by the following rules:

- Triggered abilities on a card a player controls are optionally
  triggered (or not) by that player at the appropriate timing moment, as
  indicated by the ability.
- A triggered ability can only be initiated if its effect has the
  potential to change the game state, and its cost (if any) has the
  potential to be paid in full, taking active cost modifiers into
  account. This potential is assessed without taking into account the
  consequences of the cost payment or any other ability interactions.
- Once an ability is initiated, players must resolve as much of the
  effect as possible, unless the effect uses the word "may" (see
  "[May](#May)" on page 15).

<span style="color:blue">(Added in FAQ, section 'Game Play', point
1.2)</span> An investigator is permitted to use triggered abilities
(\[fast\], \[reaction\], and \[action\] abilities) from the following
sources:

- A card in play and under his or her control. This includes his or her
  investigator card.
- A scenario card that is in play and at the same location as the
  investigator. This includes the location itself, encounter cards
  placed at that location, and all encounter cards in the threat area of
  any investigator at that location.
- The current act or current agenda card.
- Any card that explicitly allows the investigator to activate its
  ability.

### Keywords

A keyword is a card ability which conveys specific rules to its card
(see "[Keywords](#Keywords)" on page 13).

### Spawn Instructions and Prey Instructions

Spawn instructions inform where an enemy spawns as it enters play (see
"[Spawn](#Spawn)" on page 19).

Prey instructions inform which investigator an enemy pursues and/or
engages if it has a choice (see "[Prey](#Prey)" on page 17).

### Action Designators

Some abilities have bold action designators (such as **Fight**,
**Evade**, **Investigate**, or **Move**). Activating such an ability
performs the designated action as described in the rules, but modified
in the manner described by the ability.
