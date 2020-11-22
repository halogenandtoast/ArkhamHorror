{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.WhatsGoingOn where

import Arkham.Import hiding (sequence)

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner

newtype WhatsGoingOn = WhatsGoingOn Attrs
  deriving newtype (Show, ToJSON, FromJSON)

whatsGoingOn :: WhatsGoingOn
whatsGoingOn =
  WhatsGoingOn $ baseAttrs "01105" 1 "What's Going On?!" "Agenda 1a" (Static 3)

instance HasModifiersFor env WhatsGoingOn where
  getModifiersFor = noModifiersFor

instance HasActions env WhatsGoingOn where
  getActions i window (WhatsGoingOn x) = getActions i window x

instance AgendaRunner env => RunMessage env WhatsGoingOn where
  runMessage msg (WhatsGoingOn attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1a" -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      unshiftMessage
        (Ask leadInvestigatorId $ ChooseOne
          [ Label
            "Each investigator discards 1 card at random from his or her hand"
            [AllRandomDiscard, NextAgenda aid "01106"]
          , Label
            "The lead investigator takes 2 horror"
            [ InvestigatorAssignDamage leadInvestigatorId (AgendaSource aid) 0 2
            , NextAgenda aid "01106"
            ]
          ]
        )
      pure $ WhatsGoingOn $ attrs & sequence .~ "Agenda 1b" & flipped .~ True
    _ -> WhatsGoingOn <$> runMessage msg attrs
