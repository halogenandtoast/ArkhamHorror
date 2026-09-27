---
name: project-labeledi-erases-i18n-vars
description: "`labeledI` rebinds withI18n internally, so any ambient countVar/cardNameVar at the call site is silently dropped from the label"
metadata:
  type: project
---

`labeledI` (`Arkham/Message/Lifted/Choose.hs`) has **no** `HasI18n` constraint — it binds the
implicit params itself with `withI18n`, which resets `?scope` **and** `?scopeVars`
(`Arkham/I18n.hs`). Any `countVar` / `cardNameVar` / `keyVar` wrapping the call is therefore
gone before `ikey` serialises the key.

So `withI18n $ countVar 2 $ labeledI "drawCards"` emits `$label.drawCards`, not
`$label.drawCards count=i:2`. **The shape `<var> $ labeledI "key"` is always a bug.**

It fails silently in two different ways, neither of which throws:

- keys with a plural branch (`"Draw 1 card | Draw {count} cards"`) fall back to branch 0, so the
  label reads "Draw 1 card" no matter the count — True Awakening (2) drew 2 and said 1 (#5686);
- keys with a bare interpolation and no plural branch (`healDamage`/`healHorror`/`takeDamage`/
  `takeHorror`/`takeDirectDamage` = `"Heal {count} damage"`, `returnNameToHand` =
  `"Return {name} to hand"`) lose their only variable and render with a hole — wrong even at 1.

Use `labeled` instead; it is the same function minus the self-inflicted `withI18n`, and takes
`HasI18n` from the call site:

- ambient scope already empty (`withI18n do` / `withI18n $ …`): `labeled "key"` emits the
  identical key string, now carrying the vars;
- inside `scenarioI18n $ scope "<card>"`: a bare `labeled` would resolve to
  `<scenario>.label.<card>.takeHorror`, which is not in the locale files — use
  `unscoped $ countVar n $ labeled "key"`.

`labeledI` stays correct for a **var-less** label in a scoped block (`labeledI "doNotReturn"`),
which is the case it exists for; ~94 files call it with no ambient i18n context at all, which is
why it cannot simply take `HasI18n`.

Related: [[project_i18n_lazy_loading]], [[project_cards_json_label_vs_tooltips_sections]].
