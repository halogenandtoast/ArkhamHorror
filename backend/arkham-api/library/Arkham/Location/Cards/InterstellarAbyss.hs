module Arkham.Location.Cards.InterstellarAbyss (interstellarAbyss) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers

newtype InterstellarAbyss = InterstellarAbyss LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interstellarAbyss :: LocationCard InterstellarAbyss
interstellarAbyss = location InterstellarAbyss Cards.interstellarAbyss 6 (Static 0)

instance HasModifiersFor InterstellarAbyss where
  getModifiersFor (InterstellarAbyss a) = modifySelect a (investigatorAt a) [CannotSpendClues]

instance HasAbilities InterstellarAbyss where
  getAbilities (InterstellarAbyss a) = extendRevealed1 a $ cosmos a 1

instance RunMessage InterstellarAbyss where
  runMessage msg l@(InterstellarAbyss attrs) = runQueueT $ case msg of
    RunCosmos iid (is attrs -> True) msgs -> do
      valids <- findCosmosPosition iid >>= maybe (pure []) (`getEmptyPositionsInDirections` [minBound ..])
      selectOne (locationIs Cards.theBlackThrone) >>= \case
        Just loc -> do
          findLocationInCosmos loc >>= \case
            Just (Pos throneX throneY) -> do
              let manhattanDistance (Pos x y) = abs (x - throneX) + abs (y - throneY)
              let closest = mins $ map (toSnd manhattanDistance) valids
              chooseCosmos attrs iid closest msgs
            Nothing -> error "the black throne not in cosmos"
        Nothing -> chooseCosmos attrs iid valids msgs
      pure l
    Do (PlaceCosmos _ (is attrs -> True) cloc) -> do
      handleCosmos attrs cloc
      pure l
    _ -> InterstellarAbyss <$> liftRunMessage msg attrs
