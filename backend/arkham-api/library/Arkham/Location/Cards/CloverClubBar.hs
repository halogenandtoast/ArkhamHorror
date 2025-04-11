module Arkham.Location.Cards.CloverClubBar (cloverClubBar) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (cloverClubBar)
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.Name
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype CloverClubBar = CloverClubBar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubBar :: LocationCard CloverClubBar
cloverClubBar = symbolLabel $ location CloverClubBar Cards.cloverClubBar 3 (Static 0)

instance HasAbilities CloverClubBar where
  getAbilities (CloverClubBar attrs) =
    extendRevealed1 attrs
      $ playerLimit PerGame
      $ restricted attrs 1 (OnAct 1 <> Here)
      $ actionAbilityWithCost (ResourceCost 2)

instance RunMessage CloverClubBar where
  runMessage msg l@(CloverClubBar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      name <- field InvestigatorName iid
      gainClues iid (attrs.ability 1) 2
      drawCards iid (attrs.ability 1) 2
      remember $ HadADrink $ labeled name iid
      pure l
    _ -> CloverClubBar <$> liftRunMessage msg attrs
