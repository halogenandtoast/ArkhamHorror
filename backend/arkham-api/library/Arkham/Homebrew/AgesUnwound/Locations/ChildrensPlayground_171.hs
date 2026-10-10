module Arkham.Homebrew.AgesUnwound.Locations.ChildrensPlayground_171 (childrensPlayground_171) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (ShroudModifier))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n, getStandardActions)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChildrensPlayground_171 = ChildrensPlayground_171 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

childrensPlayground_171 :: LocationCard ChildrensPlayground_171
childrensPlayground_171 =
  symbolLabel $ location ChildrensPlayground_171 Cards.childrensPlayground_171 5 (PerPlayer 1)

{- | "[reaction] When an investigator initiates an investigation of Children's
Playground, spend X actions: Children's Playground gets -X shroud for this
investigation."

Identical to Scenario III's Children's Playground (@:ages-unwound:058@): X is
chosen in the handler rather than paid as a cost, because 'Arkham.Cost.Cost' has
no variable-action constructor and only /standard/ actions may be spent on an
arbitrary ability -- which is what 'getStandardActions' counts and
'loseStandardActions' takes. The printed subject is "an investigator", so anyone
here may chip in on someone else's investigation.
-}
instance HasAbilities ChildrensPlayground_171 where
  getAbilities (ChildrensPlayground_171 a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction
      $ InitiatedSkillTest #when Anyone AnySkillType AnySkillTestValue (WhileInvestigating (be a))

instance RunMessage ChildrensPlayground_171 where
  runMessage msg l@(ChildrensPlayground_171 attrs) = runQueueT $ campaignI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      spendable <- getStandardActions iid
      when (spendable > 0)
        $ chooseAmount iid "childrensPlayground.spendActions" "$actions" 1 spendable attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$actions" -> n) (isTarget attrs -> True) | n > 0 -> do
      loseStandardActions iid (attrs.ability 1) n
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) attrs (ShroudModifier (-n))
      pure l
    _ -> ChildrensPlayground_171 <$> liftRunMessage msg attrs
