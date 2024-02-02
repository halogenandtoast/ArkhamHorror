module Arkham.Agenda.Cards.TheShadowOfTheEclipse (
  TheShadowOfTheEclipse (..),
  theShadowOfTheEclipse,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher

newtype TheShadowOfTheEclipse = TheShadowOfTheEclipse AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theShadowOfTheEclipse :: AgendaCard TheShadowOfTheEclipse
theShadowOfTheEclipse =
  agenda (2, A) TheShadowOfTheEclipse Cards.theShadowOfTheEclipse (Static 3)

instance RunMessage TheShadowOfTheEclipse where
  runMessage msg a@(TheShadowOfTheEclipse attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      maskedCarnevaleGoers <-
        selectList
          (AssetWithTitle "Masked Carnevale-Goer")
      lead <- getLeadPlayer
      leadInvestigator <- getLead
      case maskedCarnevaleGoers of
        [] -> push $ AdvanceAgendaDeck agendaDeckId (toSource attrs)
        xs ->
          pushAll
            [ chooseOne
                lead
                [ targetLabel x [Flip leadInvestigator (toSource leadInvestigator) (toTarget x)]
                | x <- xs
                ]
            , RevertAgenda aid
            ]
      pure a
    _ -> TheShadowOfTheEclipse <$> runMessage msg attrs
