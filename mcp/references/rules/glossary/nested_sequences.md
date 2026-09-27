---
title: Nested Sequences
source: ArkhamDB Rules Reference (https://arkhamdb.com/rules)
---

# Nested Sequences

Each time a triggering condition occurs, the following sequence is
followed: 1) execute “when...” effects that interrupt that triggering
condition, (2) resolve the triggering condition, and then, (3) execute
“after...” effects in response to that triggering condition.

Within this sequence, if the use of a \[reaction\] or **Forced** ability
leads to a new triggering condition, the game pauses and starts a new
sequence: (1) execute “when...” effects that interrupt the new
triggering condition, (2) resolve the new triggering condition, and
then, (3) execute “after...” effects in response to the new triggering
condition. This is called a **nested sequence**. Once this nested
sequence is completed, the game returns to where it left off, continuing
with the original triggering condition’s sequence.

It is possible that a nested sequence generates further triggering
conditions (and hence more nested sequences). There is no limit to the
number of nested sequences that may occur, but each nested sequence must
complete before returning to the sequence that spawned it. In effect,
these sequences are resolved in a Last In, First Out (LIFO) manner.

*For example: Roland and Agnes are embroiled in a fierce battle. Roland
has a Guard Dog in his play area, and is engaged with a Goat Spawn with
2 damage on it. Agnes is engaged with a Ghoul Minion. Roland wishes to
play a .45 Automatic, which provokes an attack of opportunity from the
Goat Spawn, dealing 1 damage to Roland. Roland assigns this damage to
his Guard Dog, which has a \[reaction\] ability: “When an enemy attack
deals damage to Guard Dog: Deal 1 damage to the attacking enemy.” Before
resolving the playing of Roland’s .45 Automatic, Guard Dog’s ability
resolves, and 1 damage is dealt to the Goat Spawn, which would defeat
it. Goat Spawn has the following **Forced** ability: “When Goat Spawn is
defeated: Each investigator at this location takes 1 horror.” Before
resolving the damage dealt to the Guard Dog, 1 horror is dealt to each
investigator at the location, including Agnes, who has a \[reaction\]
ability: “After 1 or more horror is placed on Agnes Baker: Deal 1 damage
to an enemy at your location.” Before resolving the Goat Spawn’s defeat,
Agnes deals 1 damage to the Ghoul Minion engaged with her. Now that
there are no further \[reaction\] or **Forced** abilities to trigger,
the players return to the previous triggering condition and resolve the
Goat Spawn’s defeat, and resolve any “After...” effects that might occur
when it is defeated. Then, the players resolve the damage dealt to the
Guard Dog, and resolve any “After...” effects that might occur from that
damage. Finally, the players return to the original triggering
condition, and Roland is able to put his .45 Automatic into play.*
