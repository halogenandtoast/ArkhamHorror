module Arkham.Types.Agenda.Cards.TheThirdNight (
  TheThirdNight,
  theThirdNight,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Resolution

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
          else AdvanceAct actId (toSource attrs)
      pure a
    _ -> TheThirdNight <$> runMessage msg attrs
