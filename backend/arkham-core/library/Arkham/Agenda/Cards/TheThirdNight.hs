module Arkham.Agenda.Cards.TheThirdNight
  ( TheThirdNight(..)
  , theThirdNight
  ) where

import Arkham.Prelude

import Arkham.Agenda.Types
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenarios.APhantomOfTruth.Helpers

newtype TheThirdNight = TheThirdNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThirdNight :: AgendaCard TheThirdNight
theThirdNight = agenda (3, A) TheThirdNight Cards.theThirdNight (Static 5)

instance HasModifiersFor TheThirdNight where
  getModifiersFor target (TheThirdNight a) | not (isTarget a target) = do
    moreConvictionThanDoubt <- getMoreConvictionThanDoubt
    pure $ toModifiers a $ [ DoomSubtracts | moreConvictionThanDoubt ]
  getModifiersFor _ _ = pure []

instance RunMessage TheThirdNight where
  runMessage msg a@(TheThirdNight attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      conviction <- getConviction
      doubt <- getDoubt
      actId <- selectJust AnyAct
      push $ if doubt >= conviction
        then ScenarioResolution $ Resolution 3
        else AdvanceAct actId (toSource attrs) AdvancedWithOther
      pure a
    _ -> TheThirdNight <$> runMessage msg attrs
