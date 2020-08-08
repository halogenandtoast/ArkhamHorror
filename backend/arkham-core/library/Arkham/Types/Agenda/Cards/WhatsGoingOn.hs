{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.WhatsGoingOn where

import Arkham.Json
import Arkham.Types.Message
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import ClassyPrelude

newtype WhatsGoingOn = WhatsGoingOn Attrs
  deriving newtype (Show, ToJSON, FromJSON)

whatsGoingOn :: WhatsGoingOn
whatsGoingOn = WhatsGoingOn $ baseAttrs
  "01105"
  "What's Going On?!"
  "Agenda 1a"
  (Static 3)

instance (AgendaRunner env) => RunMessage env WhatsGoingOn where
  runMessage msg a@(WhatsGoingOn attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      a <$ unshiftMessages
        [ Ask $ ChooseOneFromSource $ MkChooseOneFromSource
          { chooseOneSource = AgendaSource aid
          , chooseOneChoices =
            [ label
              "Each investigator discards 1 card at random from his or her hand"
              AllRandomDiscard
            , label
              "The lead investigator takes 2 horror"
              (InvestigatorDamage leadInvestigatorId (AgendaSource aid) 0 2)
            ]
          }
        , NextAgenda aid "01106"
        ]
    _ -> WhatsGoingOn <$> runMessage msg attrs
