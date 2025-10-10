module Arkham.Location.Cards.NightmareBreach (nightmareBreach) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers

newtype NightmareBreach = NightmareBreach LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightmareBreach :: LocationCard NightmareBreach
nightmareBreach =
  locationWith NightmareBreach Cards.nightmareBreach 5 (Static 1) (connectsToL .~ adjacentLocations)

instance HasAbilities NightmareBreach where
  getAbilities (NightmareBreach a) = extendRevealed1 a $ cosmos a 1

instance RunMessage NightmareBreach where
  runMessage msg l@(NightmareBreach attrs) = runQueueT $ case msg of
    RunCosmos iid (is attrs -> True) msgs -> do
      valids <-
        findCosmosPosition iid >>= maybe (pure []) (`getEmptySpacePositionsInDirections` [minBound ..])
      chooseCosmos attrs iid valids msgs
      pure l
    Do (PlaceCosmos _ (is attrs -> True) cloc) -> do
      handleCosmosWithHandleEmptySpace attrs cloc \_ c -> do
        removeCardFromGame c
        selectEachDiscardable c (toDiscard attrs)
        eachInvestigator \iid' -> do
          cards <- select $ inHandOf NotForPlay iid' <> basic (CardWithCardCode c.cardCode)
          for_ cards (discardCard iid' attrs)
      pure l
    _ -> NightmareBreach <$> liftRunMessage msg attrs
