module Arkham.Location.Cards.Bedroom (bedroom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.Cards qualified as Cards (bedroom)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Bedroom = Bedroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroom :: LocationCard Bedroom
bedroom = location Bedroom Cards.bedroom 2 (PerPlayer 1)

instance HasAbilities Bedroom where
  getAbilities (Bedroom a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a) #failure

instance RunMessage Bedroom where
  runMessage msg l@(Bedroom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscard iid $ attrs.ability 1
      pure l
    _ -> Bedroom <$> liftRunMessage msg attrs
