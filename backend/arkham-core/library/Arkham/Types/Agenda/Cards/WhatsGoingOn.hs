module Arkham.Types.Agenda.Cards.WhatsGoingOn where


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner

newtype WhatsGoingOn = WhatsGoingOn AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatsGoingOn :: WhatsGoingOn
whatsGoingOn =
  WhatsGoingOn $ baseAttrs "01105" "What's Going On?!" (Agenda 1 A) (Static 3)

instance HasModifiersFor env WhatsGoingOn where
  getModifiersFor = noModifiersFor

instance HasActions env WhatsGoingOn where
  getActions i window (WhatsGoingOn x) = getActions i window x

instance AgendaRunner env => RunMessage env WhatsGoingOn where
  runMessage msg a@(WhatsGoingOn attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      a <$ unshiftMessage
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
