module Arkham.Agenda.Cards.TheShadowOfTheEclipse
  ( TheShadowOfTheEclipse(..)
  , theShadowOfTheEclipse
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Types
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Source
import Arkham.Target

newtype TheShadowOfTheEclipse = TheShadowOfTheEclipse AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShadowOfTheEclipse :: AgendaCard TheShadowOfTheEclipse
theShadowOfTheEclipse =
  agenda (2, A) TheShadowOfTheEclipse Cards.theShadowOfTheEclipse (Static 3)

instance RunMessage TheShadowOfTheEclipse where
  runMessage msg a@(TheShadowOfTheEclipse attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      maskedCarnevaleGoers <- selectList
        (AssetWithTitle "Masked Carnevale-Goer")
      leadInvestigatorId <- getLeadInvestigatorId
      case maskedCarnevaleGoers of
        [] -> a <$ push (AdvanceAgendaDeck agendaDeckId (toSource attrs))
        xs -> a <$ pushAll
          [ chooseOne
            leadInvestigatorId
            [ Flip leadInvestigatorId (InvestigatorSource leadInvestigatorId) (AssetTarget x)
            | x <- xs
            ]
          , RevertAgenda aid
          ]
    _ -> TheShadowOfTheEclipse <$> runMessage msg attrs
