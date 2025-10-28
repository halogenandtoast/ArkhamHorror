module Arkham.Act.Cards.TheSpectralRealm (theSpectralRealm) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Query

newtype TheSpectralRealm = TheSpectralRealm ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theSpectralRealm :: ActCard TheSpectralRealm
theSpectralRealm = act (2, A) TheSpectralRealm Cards.theSpectralRealm (groupClueCost (PerPlayer 4))

instance RunMessage TheSpectralRealm where
  runMessage msg a@(TheSpectralRealm attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      entryHallId <- getJustLocationByName "Entry Hall"
      push $ RevealLocation Nothing entryHallId
      shuffleEncounterDiscardBackIn
      advanceActDeck attrs
      pure a
    _ -> TheSpectralRealm <$> liftRunMessage msg attrs
