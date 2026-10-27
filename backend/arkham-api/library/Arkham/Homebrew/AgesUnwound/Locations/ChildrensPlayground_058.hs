module Arkham.Homebrew.AgesUnwound.Locations.ChildrensPlayground_058 (childrensPlayground_058) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Action (getUnrestrictedActions)
import Arkham.Helpers.Modifiers (ModifierType (ShroudModifier))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChildrensPlayground_058 = ChildrensPlayground_058 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

childrensPlayground_058 :: LocationCard ChildrensPlayground_058
childrensPlayground_058 =
  symbolLabel $ location ChildrensPlayground_058 Cards.childrensPlayground_058 5 (PerPlayer 1)

{- | "[reaction] When an investigator initiates an investigation of Children's
Playground, spend X actions: Children's Playground gets -X shroud for this
investigation."

X is chosen in the handler rather than paid as a cost, the way /Contacting the
Lodge/ in this campaign handles its own "spend any number of additional
actions": 'Arkham.Cost.Cost' has no variable-action constructor, and only
/standard/ actions may be spent on an arbitrary ability -- which is exactly what
'getUnrestrictedActions' counts and 'loseUnrestrictedActions' takes.

Gated on having a standard action to spend. Only standard actions can pay for an
arbitrary ability, so an investigator whose last action is a restricted
additional one (Daisy's Tome-only) cannot use this at all -- and without the
criterion the reaction was still offered to them, then silently did nothing
because `chooseAmount` was skipped. 'InvestigatorWithUnrestrictedActionsRemaining'
is the same read 'getUnrestrictedActions' makes below.

The printed subject is "an investigator", not "you", so anyone at the Playground
may chip in on someone else's investigation; the ability is 'Here'-restricted
because a location's abilities are only available to investigators there.
-}
instance HasAbilities ChildrensPlayground_058 where
  getAbilities (ChildrensPlayground_058 a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> youExist (InvestigatorWithUnrestrictedActionsRemaining (atLeast 1)))
      $ freeReaction
      $ InitiatedSkillTest #when Anyone AnySkillType AnySkillTestValue (WhileInvestigating (be a))

instance RunMessage ChildrensPlayground_058 where
  runMessage msg l@(ChildrensPlayground_058 attrs) = runQueueT $ campaignI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      spendable <- getUnrestrictedActions iid
      when (spendable > 0)
        $ chooseAmount iid "childrensPlayground.spendActions" "$actions" 1 spendable attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$actions" -> n) (isTarget attrs -> True) | n > 0 -> do
      loseUnrestrictedActions iid (attrs.ability 1) n
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) attrs (ShroudModifier (-n))
      pure l
    _ -> ChildrensPlayground_058 <$> liftRunMessage msg attrs
