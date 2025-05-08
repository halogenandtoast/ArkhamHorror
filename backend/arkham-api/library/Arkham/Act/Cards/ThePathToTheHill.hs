module Arkham.Act.Cards.ThePathToTheHill (thePathToTheHill) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)

newtype ThePathToTheHill = ThePathToTheHill ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePathToTheHill :: ActCard ThePathToTheHill
thePathToTheHill = act (1, A) ThePathToTheHill Cards.thePathToTheHill Nothing

instance HasAbilities ThePathToTheHill where
  getAbilities = actAbilities \x ->
    [ mkAbility x 1
        $ Objective
        $ ForcedAbilityWithCost FastPlayerWindow (GroupClueCost (PerPlayer 2) Anywhere)
    ]

instance RunMessage ThePathToTheHill where
  runMessage msg a@(ThePathToTheHill attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach Anywhere (removeAllClues (attrs.ability 1))
      ascendingPath <- selectJust $ LocationWithTitle "Ascending Path"
      reveal ascendingPath
      advanceActDeck attrs
      pure a
    _ -> ThePathToTheHill <$> liftRunMessage msg attrs
