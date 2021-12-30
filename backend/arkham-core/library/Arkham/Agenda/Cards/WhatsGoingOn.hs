module Arkham.Agenda.Cards.WhatsGoingOn where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Attrs
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message

newtype WhatsGoingOn = WhatsGoingOn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatsGoingOn :: AgendaCard WhatsGoingOn
whatsGoingOn = agenda (1, A) WhatsGoingOn Cards.whatsGoingOn (Static 3)

instance AgendaRunner env => RunMessage env WhatsGoingOn where
  runMessage msg a@(WhatsGoingOn attrs) = case msg of
    AdvanceAgenda aid
      | aid == toId attrs && agendaSequence attrs == Agenda 1 B -> do
        iid <- getLeadInvestigatorId
        hasDiscardableCards <- notNull
          <$> select (HandWith $ LengthIs $ GreaterThan $ Static 0)
        a <$ pushAll
          [ chooseOne iid
          $ Label
              "The lead investigator takes 2 horror"
              [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2]
          : [ Label
                "Each investigator discards 1 card at random from his or her hand"
                [AllRandomDiscard]
            | hasDiscardableCards
            ]
          , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
          ]
    _ -> WhatsGoingOn <$> runMessage msg attrs
