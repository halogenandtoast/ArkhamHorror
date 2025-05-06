module Arkham.Act.Cards.SearchingForTheTome (searchingForTheTome) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers

newtype SearchingForTheTome = SearchingForTheTome ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForTheTome :: ActCard SearchingForTheTome
searchingForTheTome =
  act (3, A) SearchingForTheTome Cards.searchingForTheTome Nothing

instance HasAbilities SearchingForTheTome where
  getAbilities = actAbilities \x ->
    [ restricted x 1 (exists $ locationIs Cards.exhibitHallRestrictedHall <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SearchingForTheTome where
  runMessage msg a@(SearchingForTheTome attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      leadChooseOneM do
        labeled' "r1" $ push R1
        labeled' "r2" $ push R2
      pure a
    _ -> SearchingForTheTome <$> liftRunMessage msg attrs
