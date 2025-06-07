module Arkham.Location.Cards.GrandChamber (grandChamber) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GrandChamber = GrandChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandChamber :: LocationCard GrandChamber
grandChamber =
  location GrandChamber Cards.grandChamber 2 (PerPlayer 1)
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities GrandChamber where
  getAbilities (GrandChamber a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #when You (WhileInvestigating $ be a)
      $ ResultOneOf [#failure, SuccessResult $ LessThan $ Static 2]

instance RunMessage GrandChamber where
  runMessage msg l@(GrandChamber attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      flipCluesToDoom attrs 1
      pure l
    _ -> GrandChamber <$> liftRunMessage msg attrs
