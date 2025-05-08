module Arkham.Location.Cards.ReturnToMontparnasse (returnToMontparnasse) where

import Arkham.Ability
import Arkham.Helpers.Cost
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Window (WindowType (NonFast), mkWhen)

newtype ReturnToMontparnasse = ReturnToMontparnasse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToMontparnasse :: LocationCard ReturnToMontparnasse
returnToMontparnasse = location ReturnToMontparnasse Cards.returnToMontparnasse 1 (PerPlayer 1)

instance HasAbilities ReturnToMontparnasse where
  getAbilities (ReturnToMontparnasse a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) AnyValue

instance RunMessage ReturnToMontparnasse where
  runMessage msg l@(ReturnToMontparnasse attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      let cost = SkillIconCost n (singleton #willpower)
      hasSkills <- getCanAffordCost iid (attrs.ability 1) [] [mkWhen NonFast] cost

      if hasSkills
        then push $ PayForAbility (abilityEffect attrs [] cost) []
        else assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> ReturnToMontparnasse <$> liftRunMessage msg attrs
