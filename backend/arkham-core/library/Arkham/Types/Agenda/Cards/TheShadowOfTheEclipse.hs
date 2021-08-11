module Arkham.Types.Agenda.Cards.TheShadowOfTheEclipse
  ( TheShadowOfTheEclipse
  , theShadowOfTheEclipse
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype TheShadowOfTheEclipse = TheShadowOfTheEclipse AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShadowOfTheEclipse :: AgendaCard TheShadowOfTheEclipse
theShadowOfTheEclipse =
  agenda (2, A) TheShadowOfTheEclipse Cards.theShadowOfTheEclipse (Static 3)

instance HasModifiersFor env TheShadowOfTheEclipse
instance HasActions TheShadowOfTheEclipse

instance AgendaRunner env => RunMessage env TheShadowOfTheEclipse where
  runMessage msg a@(TheShadowOfTheEclipse attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      maskedCarnevaleGoers <- selectList
        (AssetWithTitle "Masked Carnevale-Goer")
      leadInvestigatorId <- getLeadInvestigatorId
      case maskedCarnevaleGoers of
        [] -> a <$ push (NextAgenda aid "82004")
        xs -> a <$ pushAll
          [ chooseOne
            leadInvestigatorId
            [ Flip (InvestigatorSource leadInvestigatorId) (AssetTarget x)
            | x <- xs
            ]
          , RevertAgenda aid
          ]
    _ -> TheShadowOfTheEclipse <$> runMessage msg attrs
