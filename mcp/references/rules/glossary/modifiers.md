---
title: Modifiers
source: ArkhamDB Rules Reference (https://arkhamdb.com/rules)
---

# Modifiers

Some abilities cause values or quantities of characteristics to be
modified. The game state constantly checks and (if necessary) updates
the count of any variable value or quantity that is being modified.

Any time a new modifier is applied (or removed), the entire quantity is
recalculated from the start, considering the unmodified base value and
all active modifiers.

- When calculating a value, treat all modifiers as being applied
  simultaneously. However, while performing the calculation, all
  additive and subtractive modifiers are calculated before doubling
  and/or halving modifiers.
- Fractional values are rounded up after all modifiers have been
  applied.
- A quantity on a card (such as a stat, an icon, a number of instances
  of a trait or keyword) cannot be reduced so that it functions with a
  value below zero. Negative modifiers in excess of a value's current
  quantity can be applied, but, after all active modifiers have been
  applied, any resultant value below zero is treated as zero. *(For
  example: Danny tests agility and reveals a –8 chaos token. When
  applied to his agility of 4, this would reduce his skill value to –4.
  However, his agility cannot be reduced so that it functions with a
  value below zero. While the –8 modifier still exists, his agility is
  treated as zero. If Danny were to play "Lucky!" to receive a +2 bonus
  to the test, this bonus would not be applied to the functioning skill
  value of zero; but rather, it is applied in conjunction with all
  active modifiers. Danny's agility would then be calculated as follows:
  base skill 4, –8 from chaos token, +2 from "Lucky!" for a total of –2,
  which is still treated as zero.)*
