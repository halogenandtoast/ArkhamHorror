module Arkham.Homebrew.AgesUnwound.Locations.Himalayas (himalayas) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Himalayas = Himalayas LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

himalayas :: LocationCard Himalayas
himalayas = symbolLabel $ location Himalayas Cards.himalayas 5 (PerPlayer 1)

{- | "You must spend an additional action to investigate the Himalayas."

Same seam as every printed "additional cost to investigate", so the second
action is part of the Investigate action's cost rather than a separate payment.
-}
instance HasModifiersFor Himalayas where
  getModifiersFor (Himalayas a) =
    whenRevealed a $ modifySelf a [AdditionalCostToInvestigate (ActionCost 1)]

{- | "__Forced__ - After you fail a skill test while investigating the
Himalayas: Take 1 damage."
-}
instance HasAbilities Himalayas where
  getAbilities (Himalayas a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a) #failure

instance RunMessage Himalayas where
  runMessage msg l@(Himalayas attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> Himalayas <$> liftRunMessage msg attrs
