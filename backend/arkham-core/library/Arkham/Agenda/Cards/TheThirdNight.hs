module Arkham.Agenda.Cards.TheThirdNight (
  TheThirdNight,
  theThirdNight,
) where

import Arkham.Prelude

import Arkham.Agenda.Attrs
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Resolution

newtype TheThirdNight = TheThirdNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThirdNight :: AgendaCard TheThirdNight
theThirdNight = agenda (3, A) TheThirdNight Cards.theThirdNight (Static 5)

instance HasRecord env () => HasModifiersFor env TheThirdNight where
  getModifiersFor _ target (TheThirdNight a) | not (isTarget a target) = do
    conviction <- getRecordCount Conviction
    doubt <- getRecordCount Doubt
    pure $ toModifiers a $ [DoomSubtracts | conviction > doubt]
  getModifiersFor _ _ _ = pure []

instance AgendaRunner env => RunMessage env TheThirdNight where
  runMessage msg a@(TheThirdNight attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      conviction <- getRecordCount Conviction
      doubt <- getRecordCount Doubt
      actId <- selectJust AnyAct
      push $
        if doubt >= conviction
          then ScenarioResolution $ Resolution 3
          else AdvanceAct actId (toSource attrs) AdvancedWithOther
      pure a
    _ -> TheThirdNight <$> runMessage msg attrs
