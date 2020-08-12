{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.WhatsGoingOn where

import Arkham.Json
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import ClassyPrelude hiding (sequence)
import Lens.Micro

newtype WhatsGoingOn = WhatsGoingOn Attrs
  deriving newtype (Show, ToJSON, FromJSON)

whatsGoingOn :: WhatsGoingOn
whatsGoingOn =
  WhatsGoingOn $ baseAttrs "01105" "What's Going On?!" "Agenda 1a" (Static 3)

instance (AgendaRunner env) => RunMessage env WhatsGoingOn where
  runMessage msg (WhatsGoingOn attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1a" -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      unshiftMessage $ Ask leadInvestigatorId (ChooseOne [AdvanceAgenda aid])
      pure $ WhatsGoingOn $ attrs & sequence .~ "Agenda 1b" & flipped .~ True
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1b" -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      unshiftMessage
        (Ask leadInvestigatorId $ ChooseOne
          [ Label
            "Each investigator discards 1 card at random from his or her hand"
            [AllRandomDiscard, NextAgenda aid "01106"]
          , Label
            "The lead investigator takes 2 horror"
            [ InvestigatorDamage leadInvestigatorId (AgendaSource aid) 0 2
            , NextAgenda aid "01106"
            ]
          ]
        )
      pure $ WhatsGoingOn $ attrs & sequence .~ "Agenda 1b" & flipped .~ True
    _ -> WhatsGoingOn <$> runMessage msg attrs
