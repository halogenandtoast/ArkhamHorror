module Arkham.Types.Agenda.Cards.WhatsGoingOn where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source

newtype WhatsGoingOn = WhatsGoingOn AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatsGoingOn :: AgendaCard WhatsGoingOn
whatsGoingOn = agenda (1, A) WhatsGoingOn Cards.whatsGoingOn (Static 3)

instance HasModifiersFor env WhatsGoingOn
instance HasActions WhatsGoingOn

instance AgendaRunner env => RunMessage env WhatsGoingOn where
  runMessage msg a@(WhatsGoingOn attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      a <$ push
        (chooseOne
          leadInvestigatorId
          [ Label
            "Each investigator discards 1 card at random from his or her hand"
            [AllRandomDiscard, NextAgenda aid "01106"]
          , Label
            "The lead investigator takes 2 horror"
            [ InvestigatorAssignDamage
              leadInvestigatorId
              (AgendaSource aid)
              DamageAny
              0
              2
            , NextAgenda aid "01106"
            ]
          ]
        )
    _ -> WhatsGoingOn <$> runMessage msg attrs
