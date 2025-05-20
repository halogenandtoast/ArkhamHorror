module Arkham.Agenda.Cards.TheThirdNight (theThirdNight) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Scenarios.APhantomOfTruth.Helpers

newtype TheThirdNight = TheThirdNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThirdNight :: AgendaCard TheThirdNight
theThirdNight = agenda (3, A) TheThirdNight Cards.theThirdNight (Static 5)

instance HasModifiersFor TheThirdNight where
  getModifiersFor (TheThirdNight a) = do
    moreConvictionThanDoubt <- getMoreConvictionThanDoubt
    modifySelf a [OtherDoomSubtracts | moreConvictionThanDoubt]

instance RunMessage TheThirdNight where
  runMessage msg a@(TheThirdNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      conviction <- getConviction
      doubt <- getDoubt
      if doubt >= conviction then push R3 else advanceCurrentAct attrs
      pure a
    _ -> TheThirdNight <$> liftRunMessage msg attrs
