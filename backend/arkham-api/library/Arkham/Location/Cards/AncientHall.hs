module Arkham.Location.Cards.AncientHall (ancientHall) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDoomOfEztli.Helpers

newtype AncientHall = AncientHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientHall :: LocationCard AncientHall
ancientHall =
  location AncientHall Cards.ancientHall 3 (PerPlayer 2)
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities AncientHall where
  getAbilities (AncientHall a) =
    extendRevealed1 a $ restricted a 1 (cluesOnThis 1) (forced $ RoundEnds #when)

instance RunMessage AncientHall where
  runMessage msg l@(AncientHall attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ investigatorAt (toId attrs) <> InvestigatorCanSpendResources (Static 3)
      if null investigators
        then flipCluesToDoom attrs 1
        else leadChooseOneM $ scenarioI18n do
          questionLabeled' "ancientHall.instructions"
          labeled' "ancientHall.doNotSpendResources" $ flipCluesToDoom attrs 1
          targets investigators $ spendResourcesOf 3
      pure l
    _ -> AncientHall <$> liftRunMessage msg attrs
